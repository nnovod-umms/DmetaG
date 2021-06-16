package edu.umassmed.dmetag.dmeta

import edu.umassmed.dmetag.utils.RequestUtils
import edu.umassmed.dmetag.utils.Types.DBID
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.immutable.Graph

import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

/**
 * Representation of a graph.  Each graph has an entry that is the "focus" of the graph.  The graph contains parents
 * and children, direct and indirect, of the focus element.  In addition, a path from the top parent(s) to the
 * focus is tracked so it can be displayed if wanted.
 * @param basedOn DBID of entry graph is based around
 * @param wfGraph graph with nodes (including basedOn entry) and edges leading to and from basedOn entry
 * @param path graph of path (nodes and edges) from top of graph to baseOn entry (including entry)
 * @param entryData map of nodes (DBID) to (collectionName, map of entry data)
 * @param dmMetaDefs metadata definitions
 */
case class WFGraph(basedOn: DBID, wfGraph: Graph[DBID, DiEdge], path: Graph[DBID, DiEdge],
                   entryData: Map[DBID, (String, Map[String, Any])], dmMetaDefs: DMetaDefs)

object WFGraph {
  /**
   * Build a graph of all entries around a specified single entry.  All generations of parents and children are built
   * into the graph.  Note that children of parents that do not lead to the single entry, as well as parents of
   * children that are not part of the entrie's descendants, are not included in the graph.  For example if X is the
   * entry for which the graph is being built and its parent is A which has children X and Y then Y is not included
   * in the graph.  Similarly if X has children A and B and A has parents X and Z then Z is also not included in the
   * graph.
   * @param mdDefs metadata definitions
   * @param collName collection name (projectName_collectionName) for entry
   * @param nodeDBID entry DBID
   * @param nodeValues map of node values (fieldName->fieldValue)
   * @return Either error (Left) or graph, along with path to entry (Right)
   */
  def buildGraphWithPath(mdDefs: DMetaDefs, collName: String, nodeDBID: DBID, nodeValues: Map[String, Any])
  : Future[Either[String, WFGraph]] = {
    makeGraph(collName = collName, nodeDBID = nodeDBID, nodeValues = nodeValues, mdDefs = mdDefs).map {
      case Left(err) => Left(err)
      case Right((foundDBID, nodesMap, graph)) =>
        // @TODO should be easier way to traverse
        // Record current node and edges to parents at next level. then recurse
        // to get next level for parents
        def findPreds(topNode: DBID): (List[DBID], List[DiEdge[DBID]]) = {
          val node = graph.get(topNode)
          val parents = node.diPredecessors.toList.map(_.toOuter)
          parents.foldLeft((List(topNode), List.empty[DiEdge[DBID]])) {
            case ((nodes, edges), nextNode) =>
              val nextNodeList = nextNode :: nodes
              val nextEdge = DiEdge(nextNode, topNode) :: edges
              val (parentNodes, parentEdges) = findPreds(nextNode)
              (nextNodeList ++ parentNodes, nextEdge ++ parentEdges)
          }
        }

        // Go up through predecessors to make the path to node we want to find
        val (predNodes, predEdges) = findPreds(foundDBID)
        // If no edges then must be only a single top level node
        // Otherwise edges will include all nodes as well
        val outGraph = {
          if (predEdges.isEmpty)
            Graph[DBID, DiEdge](predNodes: _*)
          else
            Graph[DBID, DiEdge](predEdges: _*)
        }
        Right(
          WFGraph(basedOn = foundDBID, wfGraph = graph, path = outGraph,
            entryData = nodesMap, dmMetaDefs = mdDefs)
        )
    }
  }

  /**
   * Make graph for a chosen entry.
   * @param collName collection name (projectName_collectionName) for entry
   * @param nodeDBID entry DBID
   * @param nodeValues map of node values (fieldName->fieldValue)
   * @param mdDefs metadata definitions
   * @return Left(error) or Right((entryDBID, Nodes(entryDBID -> (collectionName, entryValues)), graphWithEdges))
   */
  private def makeGraph(collName: String, nodeDBID:DBID, nodeValues: Map[String, Any], mdDefs: DMetaDefs)
  :Future[Either[String, (DBID, Map[DBID, (String, Map[String, Any])], Graph[DBID, DiEdge]) ]] = {
    // Get parents
    getParents(nodeDBID, collName, nodeValues, mdDefs).flatMap {/* Future result with all parents */
      case Left(err) => Future.successful(Left(err))
      // Now go get children and merge results with parents
      case Right(parentEntries) =>
        getChildren(nodeDBID, collName, nodeValues, mdDefs).map(/* Future result with all children */
          children => {
            children.map(childrenRight => { /* Right value for children*/
              val (nodes, edges) = (childrenRight._1 ++ parentEntries._1, childrenRight._2 ++ parentEntries._2)
              val graph =
                if (edges.isEmpty) {
                  Graph[DBID, DiEdge](nodeDBID)
                } else
                  Graph(edges: _*)
              (nodeDBID, nodes, graph)
            })
          })
    }
  }

  /**
   * Get parent entries (foreign key pointers) for an entry, saving the input entry and edges from the parents to the
   * input entry.  Recursion is used to go up to the top parents, gathering data and merging it into the current
   * level's output.
   * @param nodeDBID DBID entry for which to find parents
   * @param collectionName name of collection (projectName_collectionName) for node
   * @param nodeFieldValues field values for node
   * @param mdDefs metadata definitions
   * @return Left(error) or Right((Nodes(entryDBID -> (collectionName, entryValues)), edges(parent->child)))
   */
  private def getParents(nodeDBID: DBID, collectionName: String, nodeFieldValues: Map[String, Any], mdDefs: DMetaDefs)
  :Future[Either[String, (Map[DBID, (String, Map[String, Any])], List[DiEdge[DBID]])]] = {
    // Get collection for entry found
    mdDefs.collByName.get(collectionName) match {
      case None => Future.successful(Left(s"Could not find collection $collectionName"))
      case Some(coll) =>
        // Get keys for collection
        mdDefs.collKeyMap.get(coll.id) match {
          case None =>
            // No foreign keys - just record the current node and leave
            Future.successful(Right((Map(nodeDBID -> (collectionName, nodeFieldValues)), List.empty)))
          case Some(collKeys) =>
            // Retrieve all the parent entries from foreign key values that are set in the entry
            val parentsFutures: List[Future[Either[String, (DBID, String, Map[String, Any])]]] =
              collKeys.foreignKeys.flatMap(fk =>
                // Get value for foreign key set in entry's map
                nodeFieldValues.get(fk.name) match {
                  // None will be filtered out by flatMap
                  case None => None
                  case Some(value) =>
                    mdDefs.collByName.get(fk.ref.get) match {
                      case None => Some(Future.successful(Left(s"Unable to find collection ${fk.ref.get}")))
                      case Some(fkColl) =>
                        // Go fetch parent entry based on id found for foreign key value - map Right value
                        // to (entryDBID, collectionName, entryData)
                        Some(
                          DMetaRequests.findEntryDataForID(mdDefs, value.toString, fkColl)
                            .map(_.flatMap(entryValues => Right((value.toString, fk.ref.get, entryValues))))
                        )
                    }
                }
              )
            // Make list of Futures into a single Future returning a list of parent entry data
            Future.sequence(parentsFutures).flatMap(parents => {
              // Get failures and successes - if any failures abort
              val (failures, parentsData) = RequestUtils.getEitherLists(parents)
              if (failures.nonEmpty)
                Future.successful(Left(s"Error retrieving parent collection values: $failures"))
              else {
                // Recurse to get values for next generation of parents
                val nextGen = parentsData.map {
                  case (entryID, collName, entryVals) =>
                    getParents(
                      nodeDBID = entryID, collectionName = collName,
                      nodeFieldValues = entryVals, mdDefs = mdDefs
                    )
                }
                // Merge in next generation of parents
                val edges = parentsData.map(parent => DiEdge(parent._1, nodeDBID))
                mergeNext(nextGenFutures = nextGen, nextGenLabel = "parent",
                  currentEntry = (Map(nodeDBID -> (collectionName, nodeFieldValues)), edges))
              }
            })
        }
    }
  }

  /**
   * Get child entries (child with foreign keys pointing to entry), saving each child entry and edges going from the
   * input entry to the children.  Recursion goes down to the children, adding lower level data to the current level.
   * @param nodeDBID DBID entry for which to find parents
   * @param collectionName name of collection (projectName_collectionName) for node
   * @param nodeFieldValues field values for node
   * @param mdDefs metadata definitions
   * @return Left(error) or Right((Node(entryDBID -> (collectionName, fieldValues)), edges(parent->child)))
   */
  def getChildren(nodeDBID: DBID, collectionName: String, nodeFieldValues: Map[String, Any], mdDefs: DMetaDefs)
  :Future[Either[String, (Map[DBID, (String, Map[String, Any])], List[DiEdge[DBID]])]] = {
    // Get collection for entry found
    mdDefs.collByName.get(collectionName) match {
      case None => Future.successful(Left(s"Could not find collection $collectionName"))
      case Some(coll) =>
        // Get children relationships pointing to current collection
        val children =
          mdDefs.relList.filter(relation => {
            relation.pk.coll.id == coll.id
          })
        // Get the children entries where foreign keys of children point to current collection
        val childrenValues = children.map {
          case CollectionRelationship(fk, pk) =>
            val collName = DMetaDefs.getCollName(fk.coll, mdDefs.projects)
            // Map list of Right side of Future completion to be list of (collName, List(mapOfDataValuesFoundForEntry))
            DMetaRequests.findEntryData(mdDefs, Document(collName, Map(fk.field.name -> nodeDBID)))
              .map(/* Either Completion */_.map(/* Right List */_.map((collName, _))))
        }
        // Make list of Futures into a single Future returning a list of child entry data
        Future.sequence(childrenValues).flatMap(children => {
          // If any failures end with them
          val withoutNotFound = children.filterNot {
            case Left(err) => DMetaRequests.isNotFound(err)
            case _ => false
          }
          val (failures, childsData) = RequestUtils.getEitherLists(withoutNotFound)
          if (failures.nonEmpty)
            Future.successful(Left(s"Error retrieving children collection values: $failures"))
          else {
            // Make list of list of entries per relationship into a single list of entries (flatten)
            // and then make list of (DBID, collName, entryData)
            val childEntries = childsData.flatten.map {
              case (collName, childEntryData) =>
                childEntryData.get(DMetaDefs.ID_FIELD) match {
                  case Some(childDBID: String) => Right((childDBID, collName, childEntryData))
                  case _ => Left(s"${DMetaDefs.ID_FIELD} not found for $collName $childEntryData")
                }
            }
            // If any failures end with them
            val (err, entryList) = RequestUtils.getEitherLists(childEntries)
            if (err.nonEmpty)
              Future.successful(Left(err))
            else {
              // Recurse to get next generation of entries
              val nextGen =
                entryList.map {
                  case (entryDBID, entryCollName, entryData) => getChildren(entryDBID, entryCollName, entryData, mdDefs)
                }
              // Get nodes (children) and edges to children for current level
              val (nodes, edges) =
                entryList.foldLeft((Map.empty[DBID, (String, Map[String, Any])], List.empty[DiEdge[DBID]])) {
                  case ((nodeMapSoFar, edgesSoFar), (nextDBID, nextCollectionName, nextEntries)) =>
                    (nodeMapSoFar + (nextDBID -> (nextCollectionName, nextEntries)), DiEdge(nodeDBID, nextDBID) :: edgesSoFar)
                }
              // Merge in next generation of children
              mergeNext(nextGenFutures = nextGen, nextGenLabel = "children", currentEntry = (nodes, edges))
            }
          }
        })
    }
  }

  /**
   * Merge in next generation of data.
   * @param nextGenFutures list of futures to get next generation of data
   * @param nextGenLabel label for next generation (used for error messages)
   * @param currentEntry current generation entry data
   * @return Left(error) or Right((Map(DBID -> (collectionName, entryValues)), edges))
   */
  private def mergeNext
  (
    nextGenFutures: List[Future[Either[String, (Map[DBID, (String, Map[String, Any])], List[DiEdge[DBID]])]]],
    nextGenLabel: String,
    currentEntry: (Map[DBID, (String, Map[String, Any])], List[DiEdge[DBID]])
  ): Future[Either[String, (Map[DBID, (String, Map[String, Any])], List[DiEdge[DBID]])]] = {
    // Make list of Futures into a single Future returning a list of next generation data
    Future.sequence(nextGenFutures).map(nextGenParents => {
      // Get failures and successes - if any failures abort
      val (nextGenFailures, nextGenData) = RequestUtils.getEitherLists(nextGenParents)
      if (nextGenFailures.nonEmpty)
        Left(s"Error retrieving next generation of $nextGenLabel data: $nextGenFailures")
      else {
        // Finally finish up returning complete list of entry data as well as list of edges
        Right(
          nextGenData.foldLeft(currentEntry) {
            case (soFar, next) => (soFar._1 ++ next._1, soFar._2 ++ next._2)
          }
        )
      }
    })
  }
}
