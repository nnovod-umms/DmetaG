package edu.umassmed.dmetag.dmeta

import DMeta.{ColDataEntry, DataRequired, FldDataEntry, MetaData, ProjDataEntry}
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.immutable.Graph
import upickle.default.read

import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import DMetaDefs._
import edu.umassmed.dmetag.dmeta.DMeta.{ColDataEntry, FldDataEntry, MetaData, ProjDataEntry}
import edu.umassmed.dmetag.events.Requests
import edu.umassmed.dmetag.js.JsDagre
import edu.umassmed.dmetag.utils.RequestUtils
import edu.umassmed.dmetag.utils.Types.DBID

/**
 * DMeta metadata definitions.
 * @param projects project definitions with project id as key
 * @param collections collection definitions with collection id as key
 * @param collFields collection field definitions with collection id as key
 * @param collByName collection definitions with collection name (projectName_collectionName) as key
 * @param collLabels map of collection name (projectName_collectionName) to collection label
 * @param collKeyMap collection primary/foreign keys with collection id as key
 * @param idFieldMap fields matching common field name to search for across catalogs
 * @param relList relationship list (collectionForeignKey->collectionPrimaryKey)
 */
case class DMetaDefs(projects: Map[MDID, ProjDataEntry],
                     collections: Map[MDID, ColDataEntry],
                     collFields: Map[MDID, List[FldDataEntry]],
                     collByName: Map[String, ColDataEntry],
                     collLabels: Map[String, String],
                     collKeyMap: Map[MDID, CollKeys],
                     idFieldMap: List[FldDataEntry],
                     relList: List[CollectionRelationship]
                    ) {
  /**
   * Get a collections name (projectName_collectionName).
   * @param collID collection id
   * @return collection name
   */
  private[dmeta] def getCollName(collID: MDID): Option[String] =
    collections.get(collID).map(DMetaDefs.getCollName(_, projects))

  /**
   * Get separate project and collection url names (slugs) from combined name (projName_collName)
   * @param collName collection name (projName_collName)
   * @return project and collection slug name
   */
  def getCollAndProjSlugs(collName: String): Option[(String, String)] =
    collByName.get(collName)
      .flatMap(coll => getSlugs(coll))

  /**
   * Get separate project and collection url names (slugs) from collection metadata entry.
   * @param coll collection metadata
   * @return project and collection slug name
   */
  private[dmeta] def getSlugs(coll: ColDataEntry): Option[(String, String)] =
    coll.projectID
      .flatMap(projID => projects.get(projID)
        .map(proj => (proj.slug, coll.slug))
      )

  /**
   * Find children collections for a designated collection.  A child is one who points to the parent via a foreign key.
   * @param collName collection name (projectName_collectionName)
   * @return either Left(error) or Right(List(childCatalogNames))
   */
  def findChildrenNames(collName: String): Either[String, List[String]] =
    findRelationsNames(
      collName = collName,
      getRelCollIDtoMatch = rel => rel.pk.coll.id,
      getRelCollIDMatched = rel => rel.fk.coll.id
    )

  /**
   * Find parent collections for a designated collection.  A parent is one who is pointed to by a foreign key.
   * @param collName collection name (projectName_collectionName)
   * @return either Left(error) or Right(List(childCatalogNames))
   */
  def findParentNames(collName: String): Either[String, List[String]] =
    findRelationsNames(
      collName = collName,
      getRelCollIDtoMatch = rel => rel.fk.coll.id,
      getRelCollIDMatched = rel => rel.pk.coll.id
    )

  /**
   * Find related (children or parents) collections for a designated collection.  Callbacks determine exactly what
   * the relationship by matching to either the primary or foreign keys in relationships.
   * @param collName collection name (projectName_collectionName)
   * @param getRelCollIDtoMatch callback to retrieve collection ID from relationship that we're looking to match on
   * @param getRelCollIDMatched callback to retrieve collection ID of relative from relationship that we're in
   * @return
   */
  private def findRelationsNames(collName: String,
                                 getRelCollIDtoMatch: CollectionRelationship => DBID,
                                 getRelCollIDMatched: CollectionRelationship => DBID)
  : Either[String, List[String]] = {
    collByName.get(collName)
      .map(coll => {
        // Find relationships we're in
        relList.filter(relation => {
          getRelCollIDtoMatch(relation) == coll.id
        })
          // Get names of our relatives
          .flatMap(rel => {
            getCollName(getRelCollIDMatched(rel))
          })
      })
    match {
      case None => Left(s"Unable to find collection $collName")
      case Some(matched) =>
        Right(matched)
    }
  }

  /**
   * Find a relationship between a parent and child.
   * @param parentName parent collection name (projectName_collectionName)
   * @param childName child collection name (projectName_collectionName)
   * @return relationship with parent as the primary key and child as the foreign key
   */
  def findRelationship(parentName: String, childName: String): Option[CollectionRelationship] = {
    collByName.get(parentName)
      .flatMap(parentColl =>
        collByName.get(childName)
          .flatMap(childColl => {
            relList.find(rel => rel.fk.coll.id == childColl.id && rel.pk.coll.id == parentColl.id)
          })
      )
  }

  /**
   * Get fields that should be shown from a map of field values (fieldName->fieldValue)
   * @param collName collection name (projectName_collectionName)
   * @param inputFields map of fieldNames to fieldValues
   * @return map containing only those fields that are set as visible
   */
  def getVisibleFieldData(collName: String, inputFields: Map[String, Any]): Map[String, Any] = {
    val visibleFields = getVisibleFields(collName, inputFields.keySet)
    inputFields.filter {
      case (key, _) => visibleFields.exists(_.name == key)
    }
  }

  /**
   * Get fields that should be shown from a list of field names.
   * @param collName collection name (projectName_collectionName)
   * @param inputFields set of fields that are to be filtered
   * @return list of field metadata for fields to be shown
   */
  def getVisibleFields(collName: String, inputFields: Set[String]): List[FldDataEntry] = {
    // Get list of entry fields in collection
    val fields =
      collByName.get(collName)
        .flatMap(coll => collFields.get(coll.id))
        .getOrElse(List.empty)
    // Get which fields are in entries (and are not to be hidden)
    fields.flatMap(fldData =>
      if (inputFields.contains(fldData.name) && !fldData.hidden) {
        List(fldData)
      } else {
        List.empty
      }
    )
  }

  /**
   * Get primary key value for an entry
   * @param coll collection definition
   * @param entryData entry data
   * @return primary key for entry
   */
  private[dmeta] def getCollEntryPK(coll: ColDataEntry, entryData: Map[String, Any]): String = {
    // Get keys for collection and then map to get only primary keys
    val pks =
      collKeyMap.get(coll.id)
        .map(_.primaryKeys)
        .getOrElse(List.empty)
    // Make string of primary keys
    getCollEntryLabel(entryData, pks)
  }

  /**
   * Get collection and entry (concatenated primary key values) labels
   * @param entryColl entry collection name
   * @param entryData entry data
   * @return (collectionLabel, entryLabel)
   */
  def getLabel(entryColl: String, entryData: Map[String, Any])
  : Either[String, (String, String)] = {
    def errStr(collModifier: Option[String]) = {
      val mod = collModifier.getOrElse("")
      s"Unable to find collection$mod${if (mod.nonEmpty) " " else ""} for $entryColl"
    }

    // Find collection
    collByName.get(entryColl) match {
      case None => Left(errStr(None))
      case Some(coll) => collLabels.get(entryColl) match {
        case None => Left(errStr(Some("label")))
        case Some(collLabel) =>
          collKeyMap.get(coll.id) match {
            case None => Left(errStr(Some("keys")))
            case Some(keys) =>
              // Get entry label from primary keys
              val entryLabel = getCollEntryLabel(entryData, keys.primaryKeys)
              Right((collLabel, entryLabel))
          }
      }
    }
  }

  /**
   * Make a display graph for the relationships between all the collections, including ones that stand alone without
   * parents or children.
   * @return graph, with layout done, for all the collection relationships.
   */
  def makeRelDisplayGraph(): JsDagre.Graph = {
    // Get graph of relationships
    val relGraph = makeRelGraph()
    // Make graph to setup for display
    val graph = JsDagre.newGraph
    // Get nodes (collections) and their corresponding labels
    val nodes = relGraph.nodes.map(_.toOuter)
    val nodeCollLabels = nodes.map(n => (n, collLabels(n)))
    // Set the collections as nodes in the display graph
    nodeCollLabels.foreach{
      case (collName, collLabel) =>
        val maxLabelLength = if (collLabel.length > collName.length) collLabel.length else collName.length
        // Set node in graph
        graph.setNode(collName,
          JsDagre.Attrs(
            label = collLabel, collName = collName, width = maxLabelLength * 18, height = 85, entryID = collName)
        )
    }
    // Set edges into graph
    relGraph.edges.map(edge => {
      graph.setEdge(sourceId = edge.from.toOuter, targetId = edge.to.toOuter)
    })
    // Do layout for graph and return graph with nodes/edges and layout done
    JsDagre.Dagre.layout(graph)
    graph
  }

  /**
   * Make a graph with edges that are parentCollection->childCollection where parents are collections pointed to
   * by foreign keys (mongoose.schema.ObjectID) in child collections.  Node labels are set to projectName_collectionName.
   * @return graph containing edges for parent->child relationships
   */
  private def makeRelGraph()
  : Graph[String, DiEdge] = {
    // Now make edges for graph (parentCollection->childCollection) of collections as identified by foreign keys
    // Each collection is set as projectName_collectionName
    val edges = collKeyMap.foldLeft(List.empty[DiEdge[String]]) {
      case (soFar, (collID, keys)) =>
        // Get graph edges for foreign keys that point to collections with primary key(s)
        val edgesList = {
          // Flatten to get values (deletes Nones and unwraps Somes)
          keys.foreignKeys.flatMap(fk => {
            // Get collection name to be used for collection containing foreign key
            getCollName(collID) match {
              case Some(fkCollName) =>
                // Get parent collection
                collByName.get(fk.ref.get) match {
                  // Make sure parent has primary key(s)
                  case Some(pkColl) if collKeyMap.contains(pkColl.id) && collKeyMap(pkColl.id).primaryKeys.nonEmpty =>
                    // Point parent to child (collection containing foreign key)
                    Some(DiEdge(fk.ref.get, fkCollName))
                  case _ => None
                }
              case _ => None
            }
          })
        }
        // Add new edges found
        soFar ++ edgesList
    }
    // Make graph with all the edges found
    val nodes = collLabels.keys.toList
    Graph.from(nodes, edges)
  }
}

/**
 * Companion object
 */
object DMetaDefs {
  type MDID = String
  // ID field name in documents
  val ID_FIELD = "_id"
  // ID field name to look for across collections
  // @TODO eventually make a field in dMeta or at least have it be part of UI settings?
  val COMMON_FIELD = "barcode"

  /**
   * Keys for a collection
   * @param primaryKeys primary keys (fields that are used to represent an individual document)
   * @param foreignKeys foreign keys (fields that are IDs pointing to parent collections)
   */
  case class CollKeys(primaryKeys: List[FldDataEntry], foreignKeys: List[FldDataEntry])

  /**
   * Enumerated values for different type of metadata
   */
  private object DmetadataType extends Enumeration {
    type DmetadataType = Value
    val DMField, DMColl, DMProj = Value
  }
  import DmetadataType._

  // Type to designate a string is Json
  private type Json = String

  /**
   * Metadata maps.
   * @param projects project definitions with id as key
   * @param collections collection definitions with id as key
   * @param fields field definitions with collection id as key, list of collection's fields as value
   */
  private case class MDMaps(projects: Map[MDID, ProjDataEntry], collections: Map[MDID, ColDataEntry],
                            fields: Map[MDID, List[FldDataEntry]])

  /**
   * Fetch and organize DMeta definitions.
   * @return DMeta definitions
   */
  def getDMetaDefs: Future[Either[String, DMetaDefs]] =
    getMDMaps.map(_.flatMap {
      case MDMaps(projsMap, collsMap, collFields) =>
        // Get map of collection name (projectName_collectionName) to collection dataEntries
        val collsNameMap = collsMap.values.map(coll => getCollName(coll, projsMap) -> coll).toMap
        // Get map of collection id to (list of primary keys, list of foreign keys)
        val collKeys = getKeys(collFields)
        // Get collections with common name
        val idMap =
          collFields.toList.flatMap {
            case (collID, fldEntries) =>
              fldEntries.find(_.name == COMMON_FIELD)
          }
        Right(DMetaDefs(
          projects = projsMap,
          collections = collsMap,
          collFields = collFields,
          collByName = collsNameMap,
          collLabels = getCollLabels(collByName = collsNameMap, projects = projsMap),
          collKeyMap = collKeys,
          idFieldMap = idMap,
          relList =
            getRelationships(collKeys = collKeys, projsMap = projsMap, collsNameMap = collsNameMap, collsMap = collsMap)
        ))
    })

  /**
   * Get the metadata maps.
   * @return error (Left(errString)) or metadata maps (Right(MDMaps))
   */
  private def getMDMaps: Future[Either[String, MDMaps]] = {
    // Startup requests for metadata
    val dataRequests = List(getMetaData(DMField), getMetaData(DMColl), getMetaData(DMProj))
    // Make list of futures in a single future returning a list
    val data = Future.sequence(dataRequests)
    // No go get the data
    data.map(requestsData => {
      val (failures, successes) = RequestUtils.getEitherLists(requestsData)
      // If any failures exit with error
      if (failures.nonEmpty) {
        Left(s"Error retrieving Metadata: $failures")
      } else {
        // Method to get wanted DMeta type data
        def findData(metaType: DmetadataType): Json = {
          val dataWanted = successes.find(_._2 == metaType)
          if (dataWanted.isEmpty)
            throw new Exception(s"Lost metadata for ${metaType.toString}")
          else
            dataWanted.get._1
        }

        try {
          // Get map of projects and catalogs with ids as keys and dataEntries as values
          // For each type we first convert the json to an object and then convert the resulting lists into the wanted map
          val projs = read[MetaData[ProjDataEntry]](findData(DmetadataType.DMProj))
          val projsMap = projs.data.data.map(proj => proj.id -> proj).toMap
          val colls = read[MetaData[ColDataEntry]](findData(DmetadataType.DMColl))
          val collsMap = colls.data.data.map(coll => coll.id -> coll).toMap
          // Get all the fields
          val fields = read[MetaData[FldDataEntry]](findData(DmetadataType.DMField))
          // Group fields by collection id
          val collFieldsByID = fields.data.data.groupBy(_.collectionID)
          // Add any parents as a foreign key field (what it should have been)
          val collFieldsWithParent =
            collFieldsByID.map {
              case (collID, collFields) =>
                // Get collection entry
                collsMap.get(collID).flatMap(collEntry =>
                  // Get parent ID (if one there)
                  collEntry.parentCollectionID.flatMap(parent => {
                    // Find collection for parent
                    collsMap.get(parent).flatMap(parentEntry => {
                      // Get parent's project
                      parentEntry.projectID.flatMap(projectID => {
                        // Get project entry
                        projsMap.get(projectID).map(proj => {
                          // Finally - make the fake field data entry setting up the parent as a foreign key with
                          // the name fkCollectionName_id
                          val entryName = parentEntry.name
                          FldDataEntry(id = s"${entryName}_${parentEntry.id}",
                            name = s"${entryName}_id", label = parentEntry.label,
                            `type` = "mongoose.Schema.ObjectId", enum = List.empty,
                            required = DataRequired(required = true, msg = None), active = true,
                            collectionID = collID, ref = Some(s"${proj.name}_${parentEntry.name}"),
                            unique = false, header = false, parent = true)
                        })
                      })
                    })
                  })
                ) match {
                  case Some(parentField) =>
                    // Add parent field to collection's list of fields
                    collID -> (parentField :: collFields)
                  case None =>
                    // No parent - just leave field list as is
                    collID -> collFields
                }
            }
          Right(MDMaps(projsMap, collsMap, collFieldsWithParent))
        } catch {
          case e: Exception => Left(s"Error processing metadata: ${e.getLocalizedMessage}")
        }
      }
    }).recover(e => Left(e.getLocalizedMessage))
  }

  /**
   * Get metadata for specified type.
   * @param metaType type of metadata to be fetched
   * @return either json with metadata (Right) or error message (Left)
   */
  private def getMetaData(metaType: DmetadataType): Future[Either[String, (Json, DmetadataType)]] = {
    val url =
      metaType match {
        case DMField => URLs.DMetaFieldURL
        case DMColl => URLs.DMetaCollectionURL
        case DMProj => URLs.DMetaProjectURL
      }
    Requests.getRequest(url)
      .map(s => Right((s, metaType))) // Remember type of each request so we can assign returned json to right request
      .recover(e => Left(e.getLocalizedMessage))
  }

  /**
   * Make map of collections to collection keys (foreign and primary).
   * @param collFields map of collections to collection fields
   * @return map of collectionID->(List(primaryKey), List(foreignKey))
   */
  private def getKeys(collFields: Map[String, List[FldDataEntry]])
  : Map[String, CollKeys] = {
    // Get map of collection id to (list of primary keys, list of foreign keys)
    collFields.map {
      case (collID, collFields) =>
        val (pkFields, fkFields) =
          collFields.foldLeft((List.empty[FldDataEntry], List.empty[FldDataEntry]))(
            (soFar, fld) => {
              // Foreign key
              if (fld.`type` == "mongoose.Schema.ObjectId" && fld.ref.isDefined && fld.active)
                (soFar._1, fld :: soFar._2)
              else if (fld.header)
                (fld :: soFar._1, soFar._2)
              // Field that's not a key
              else
                soFar
            }
          )
        // If no header fields then make primary key of either single field "name" or all fields of simple type
        val finalPK =
          if (pkFields.nonEmpty)
            pkFields
          else {
            collFields.find(_.name.toLowerCase == "name") match {
              case Some(fld) => List(fld)
              case None =>
                val pkTypes = List("String", "Boolean", "Number", "Date")
                collFields.filter(entry => pkTypes.contains(entry.`type`))
            }
          }
        // Return map entry collectionID -> (primaryKeyFields, foreignKeyFields)
        collID -> CollKeys(finalPK, fkFields)
    }
  }

  /**
   * Make map of collection names (collectionName_projectName) to labels.  If the collection label by itself is unique
   * for all collections we use it, otherwise we set the label as "projectLabel collectionLabel"
   * @param collByName map of collection names to collection metadata entries
   * @param projects map of project IDs to project metadata entries
   * @return map of collection names to collection labels
   */
  private def getCollLabels(collByName: Map[String, ColDataEntry], projects: Map[MDID, ProjDataEntry])
  : Map[String, String] = {
    // Group collections by label
    val collNames = collByName.groupBy(coll => coll._2.label)
    // Make each label unique
    collNames.toList.flatMap {
      case (label, collNameMap) =>
        // If just one entry with label then use it
        if (collNameMap.size == 1)
          List(collNameMap.head._1 -> label)
        else {
          // Map each name to label that includes project (assuming one is specified)
          collNameMap.map {
            case (collName, coll) =>
              val collLabel =
                coll.projectID.flatMap(projects.get) match {
                  case Some(proj) =>
                    s"${proj.label} ${coll.label}"
                  case _ => coll.label
              }
            collName -> collLabel
          }.toList
        }
    }.toMap
  }

  /**
   * Get relationship map of foreign keys to associated primary keys.
   * @return map foreignKey->primaryKeysOfAssociatedCollection
   */
  private def getRelationships(collKeys: Map[MDID, CollKeys],
                               projsMap: Map[MDID, ProjDataEntry],
                               collsNameMap: Map[String, ColDataEntry], collsMap: Map[MDID, ColDataEntry])
  : List[CollectionRelationship] = {
    collKeys.values.flatMap(keys =>
      // For each foreign key first get foreign key data
      keys.foreignKeys.flatMap(fk => {
        collsMap.get(fk.collectionID)
          .map(coll => {
            val optProj = coll.projectID.flatMap(projsMap.get)
            ForeignKey(optProj, coll, fk)
          })
          .flatMap(fkData => {
            // Now that we've got a legit foreign key go get associated primary key data
            fk.ref.flatMap(collsNameMap.get).flatMap(pkCat =>
              collKeys.get(pkCat.id).map(pKeys => {
                val pkProj = pkCat.projectID.flatMap(projsMap.get)
                CollectionRelationship(fkData, PrimaryKey(pkProj, pkCat, pKeys.primaryKeys))
              })
            )
          })
      })
    ).toList
  }

  /**
   * Get a collections name (projectName_collectionName).
   * @param collection collection metadata definition
   * @param projsMap map of projectID->project metadata definition
   * @return collection name
   */
  private[dmeta] def getCollName(collection: ColDataEntry, projsMap: Map[MDID, ProjDataEntry]) =
    collection.projectID.flatMap(projsMap.get) match {
      case Some(collProj) => s"${collProj.name}_${collection.name}"
      case _ => collection.name
    }

  /**
   * Get label used for a collection entry, based upon primary key values.
   * @param entryData retrieved entry's data
   * @param primaryKeys list of primary key fields
   * @return label (primary key values separated by /)
   */
  private[dmeta] def getCollEntryLabel(entryData: Map[String, Any], primaryKeys: List[FldDataEntry]) = {
    // Get primary key values that are there (flatMap eliminates Nones for entries not in data map)
    val pkVals = primaryKeys.flatMap(pk => {
      entryData.get(pk.name)
    })
    pkVals.mkString(" / ")
  }

  /**
   * Look for metadata definitions.
   * @return either an error (Left) if metadata definitions can not be accessed or the metadata definitions (Right)
   */
  def getMDDefs: Either[String, DMetaDefs] =
    DMetaDefsVar.getMDDef match {
      case None =>
        Left("No metadata available - please login")
      case Some(mdDefs) =>
        Right(mdDefs)
    }
}
