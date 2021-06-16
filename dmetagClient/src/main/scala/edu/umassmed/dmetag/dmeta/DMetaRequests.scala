package edu.umassmed.dmetag.dmeta

import edu.umassmed.dmetag.dmeta.DMeta.ColDataEntry
import edu.umassmed.dmetag.events.Requests
import DMeta.ColDataEntry
import DMetaDefs.{COMMON_FIELD, ID_FIELD}
import edu.umassmed.dmetag.utils.RequestUtils
import edu.umassmed.dmetag.utils.Types.DBID

import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

/**
 * Methods to query DMeta.  These methods all take as input a copy of cached metadata definitions DMetaDefs, and then
 * do Future requests to DMeta.
 */
object DMetaRequests {
  /**
   * Get labels (collection label and entry label) for parent and it's children
   * @param mdDefs metadata definitions
   * @param parentDBID parent entry
   * @param collectionName collection name (projectName_collectionName)
   * @return error (Left) or map for parent and children (entryID -> (collectionName, collectionLabel, entryLabel)
   */
  def getChildrenLabels(mdDefs: DMetaDefs, parentDBID: DBID, collectionName: String)
  : Future[Either[String, Map[DBID, (String, String, String)]]] = {
    findEntryDataForIDWithCollName(mdDefs = mdDefs, dbID = parentDBID, collName = collectionName).flatMap {
      case Left(err) =>
        Future.successful(Left(err))
      case Right(entryData) =>
        // Map to future and then map to get Right result
        WFGraph.getChildren(
          nodeDBID = parentDBID,
          collectionName = collectionName,
          nodeFieldValues = entryData,
          mdDefs = mdDefs
        ).map( /* Future */
          _.map { /* Right */
            case (childMap, _) =>
              // Setup labels for entries found, including one initially input
              val parentWithChildrenMap = childMap + (parentDBID -> (collectionName, entryData))
              parentWithChildrenMap.map {
                case (entryID, (collName, entryMap)) =>
                  val (collLabel, entryLabel) =
                    mdDefs.getLabel(collName, entryMap) match {
                      case Left(err) => ("UNKNOWN", "UNKNOWN")
                      case Right(labels) => labels
                    }
                  entryID -> (collName, collLabel, entryLabel)
              }
          }
        )
    }
  }

  /**
   * Get label, based on primary keys, for an entry.
   * @param mdDefs metadata definitions
   * @param collName collection name (projectName_collectionName)
   * @param entryID entry DBID
   * @return Either an error (Left) or entry label (Right)
   */
  def getPKForEntry(mdDefs: DMetaDefs, collName: String, entryID: DBID): Future[Either[String, String]] = {
    mdDefs.collByName.get(collName) match {
      case None =>
        Future.successful(Left(s"Can't find collection for $collName"))
      case Some(coll) =>
        findEntryDataForID(mdDefs = mdDefs, dbID = entryID, coll = coll).map {
          case Left(err) => Left(err)
          case Right(entryValues) =>
            Right(mdDefs.getCollEntryPK(coll = coll, entryData = entryValues))
        }
    }
  }

  /**
   * Callback to startup fetches for primary key values for collection entries that are pointed to by foreign keys in
   * the designated entry.
   * @param mdDefs metadata definitions
   * @param fkRels relationships for collection foreign keys
   * @param entryData data values for initial entry
   * @return eithers (one per foreign key) with Left(errorString) or Right(FKName, PK, List(PKDocValues))
   */
  private def findFKValueWithData(mdDefs: DMetaDefs, fkRels: List[CollectionRelationship], entryData: Map[String, Any])
  : Future[List[Either[String, (String, PrimaryKey, List[Map[String, Any]])]]] = {
    // Go fetch associated primary key values in primary key collections
    val fkVals =
      fkRels.flatMap(rel => {
        val pkColl = rel.pk.coll
        val fkFld = rel.fk.field.name
        // Look for foreign key value that points to primary collection
        entryData.get(fkFld).map(pkIDValue => {
          val pkID = pkIDValue.toString
          if (pkID.isEmpty) {
            Future.successful(Right(rel.fk.field.name, rel.pk, List.empty))
          } else {
            // Get primary ID document, map future and then and map successful (Right) values to
            // (foreignKeyfieldName, primaryKey, valuesInPrimaryCollectionDocument)
            findEntryDataForID(mdDefs = mdDefs, dbID = pkID, coll = pkColl)
              .map(
                _.map(collVals => (rel.fk.field.name, rel.pk, List(collVals)))
              )
          }
        })
      })
    // Make list of futures into a single future containing a list
    Future.sequence(fkVals)
  }

  /**
   * Get the foreign key values for an entry, with entry data supplied.
   * Note that returned lists of primary key entryID and values will contain only a single entry.
   * @param mdDefs metadata definitions
   * @param collName collection name (projectName_collectionName)
   * @param entryID DBID for document with foreign key entries
   * @param entryData fetched entry data values
   * @return either Left(errorString) or Right(map of foreignKeyName->list(primaryKeyDocID, primaryKeyValue))
   */
  def findFKDataForIDWithData(mdDefs: DMetaDefs, collName: String, entryID: DBID, entryData: Map[String, Any])
  : Future[Either[String, Map[String, List[(DBID, String)]]]] = {
    def getFKValueForIDWithData(coll: ColDataEntry, fkRels: List[CollectionRelationship])
    : Future[List[Either[String, (String, PrimaryKey, List[Map[String, Any]])]]] =
      findFKValueWithData(mdDefs, fkRels, entryData)
    getFKValues(mdDefs = mdDefs, collName = collName, getKeyData = getFKValueForIDWithData)
  }

  /**
   * Get all possible foreign key values for a collection.
   * @param mdDefs metadata definitions
   * @param collName collection name (projectName_collectionName)
   * @param fkDataFound foreign key values already found to be used (don't look up these foreign keys again)
   * @return either Left(errorString) or Right(map of foreignKeyName->list(primaryKeyDocID, primaryKeyValue))
   */
  def findFKDataForAll(mdDefs: DMetaDefs, collName: String, fkDataFound: Option[Map[String, List[(DBID, String)]]] = None)
  : Future[Either[String, Map[String, List[(DBID, String)]]]] = {
    /*
     * Callback to startup fetches for all primary key values for collections that are pointed to by foreign keys in
     * the designated collection.
     * @param coll collection with foreign keys
     * @param fkRels relationships for collection foreign keys
     * @return eithers (one per foreign key) with Left(errorString) or Right(FKName, PK, List(PKDocValues))
     */
    def getFKValueForAll(coll: ColDataEntry, fkRels: List[CollectionRelationship])
    : Future[List[Either[String, (String, PrimaryKey, List[Map[String, Any]])]]] = {
      // Go fetch associated primary key values in primary key collections
      val pkFetches =
        fkRels.flatMap(rel => {
          val fkFieldName = rel.fk.field.name
          // Don't fetch foreign keys already found (if foreign key in fkDataFound it comes out as Some from flatMap/get)
          fkDataFound.flatMap(_.get(fkFieldName)) match {
            case Some(_) => None
            case None =>
              val pkColl = rel.pk.coll
              // Get all documents in collection, map future and then and map successful (Right) values to
              // (foreignKeyfieldName, primaryKey, valuesInPrimaryCollectionDocument)
              Some(
                getCollDocuments(mdDefs, pkColl)
                  .map(
                    _.map(collVals => (fkFieldName, rel.pk, collVals))
                  )
              )
          }
        })
      // Make list of futures into a single future returning a list
      Future.sequence(pkFetches)
    }
    // Do fetch of values
    val fkValues = getFKValues(mdDefs = mdDefs, collName = collName, getKeyData = getFKValueForAll)
    // If some foreign keys were fetched before merge them in now
    fkDataFound match {
      case None => fkValues
      case Some(foundFKs) =>
        // Get future result (map) and Right result (2nd map) and merge it with foreign keys previously found
        fkValues.map(_.map(_ ++ foundFKs))
    }
  }

  /**
   * Get possible values for primary keys in collections pointed to by a single collection's foreign keys.
   * @param mdDefs metadata definitions
   * @param collName name (projectName_collectionName) of collection with foreign keys
   * @param getKeyData get primary key values: (collection, collectionFKRelationships) => eithers (one per foreign key) with Left(errorString) or Right(FKName, PK, List(PKDocValues))
   * @return either with Left(errorString) or Right(map of foreignKeyName->list(primaryKeyDocID, primaryKeyValue))
   */
  private def getFKValues(mdDefs: DMetaDefs, collName: String,
                          getKeyData: (ColDataEntry, List[CollectionRelationship]) =>
                            Future[List[Either[String, (String, PrimaryKey, List[Map[String, Any]])]]]
                         )
  : Future[Either[String, Map[String, List[(DBID, String)]]]] = {
    mdDefs.collByName.get(collName) match {
      case None => Future.successful(Left(s"Unable to find collection $collName"))
      case Some(coll) =>
        // Get relationships where wanted collection has a foreign key
        val fkRels = mdDefs.relList.filter(rel => rel.fk.coll.id == coll.id)
        if (fkRels.isEmpty)
          Future.successful(Right(Map.empty))
        else {
          // Use callback to get list of primary key collection data (one per collection pointed to by foreign keys)
          getKeyData(coll, fkRels).map(requestsData => {
            // Get ones that failed and succeeded
            val (failures, successes) = RequestUtils.getEitherLists(requestsData)
            // If any failures exit with error
            if (failures.nonEmpty) {
              Left(s"Error retrieving field data: $failures")
            } else {
              Right(
                // successes are a list of (foreignKeyName, primaryKeyDefinition, primaryCollectionValues)
                successes.map {
                  // Make map of foreignKeyName -> (pkDBID, pkValue)
                  case (fldName, pk, primaryData) =>
                    // Get primary key value by concatenating together primary key fields
                    val pkValue =
                      primaryData.flatMap(fldMap => { // flatMap gets rid of Nones if "_id" not there
                        // Get _id in document (it should always be there but map will skip field if _id not there)
                        fldMap.get(ID_FIELD).map(docID => {
                          // Find fields we want (primary key fields)
                          val pKeys = pk.fields
                          // Get label for primary keys
                          val pkValue = DMetaDefs.getCollEntryLabel(fldMap, pKeys)
                          // Return primaryDocumentID -> primaryKeyValue
                          docID.toString -> pkValue
                        })
                      })
                    // Make final value (foreignFieldName -> primaryKeyValues)
                    fldName -> pkValue.sortBy(_._2)
                }.toMap // Make whole thing into a map (foreignFieldName -> primaryKeyValues)
              )
            }
          })
        }
    }
  }

  /**
   * Get all foreign key values for a collection's relationship.  The list retrieved is sorted by values and have DBID
   * keys that point to the direct parent but values, that can be used for display, that include values from all
   * generations of parents. For example if a relationship pointing to a Patient Visit parent is input then the
   * DBIDs returned will point to Patient Visit entries but the values will include both the Patient Visit and
   * associated Patient values.
   * @param mdDefs metadata definitions
   * @param field relationship we're looking at - foreign key is for root collection and primary key is first level up
   * @return either an error (Left) or a map with a single entry of (foreignKeyName -> (primaryKeyID, primaryKeyValue))
   */
  def getCompleteFK(mdDefs: DMetaDefs, field: CollectionRelationship)
  : Future[Either[String, Map[String, List[(DBID, String)]]]] = {

    /*
     * Recurse up to the parents to get a label for each foreign key entries that includes labels from all the
     * parents
     * @param fkField relationship to use at current level
     * @return Either error (Left) or list of foreignKeyDBID -> primaryKeyLabel
     */
    def getParentKeys(fkField: CollectionRelationship): Future[Either[String, List[(DBID, String)]]] = {
      // Get parent collection and name
      val parentColl = fkField.pk.coll
      val collName = mdDefs.getCollName(parentColl.id).getOrElse(s"ID ${parentColl.id}")
      // Get all entries in parent collection
      getAllCollData(mdDefs, parentColl).flatMap {
        case Left(err) => Future.successful(Left(err))
        case Right(docsFound) =>
          // Get next generation relationships (foreign keys that exist as primary keys in current level)
          val nextFKs = mdDefs.relList.filter(rel => rel.fk.coll.id == parentColl.id)
          // If no more parents we're done - return what we got as DBID->label
          if (nextFKs.isEmpty) {
            Future.successful(Right(
              docsFound.map{
                case (docDBID, doc) => docDBID -> doc.pkVal
              }
            ))
          } else {
            // Recurse to get each parents values
            val parentList =
              nextFKs.map(rel => getParentKeys(rel)
                .map( // Get future result and then map Right results to include field name
                  _.map(fkVals => (rel.fk.field.name, fkVals))
                )
              )
            // Make list of futures into a future of a list
            Future.sequence(parentList).map(nextGenParents => {
              // Get failures and successes - if any failures abort
              val (nextGenFailures, nextGenData) = RequestUtils.getEitherLists(nextGenParents)
              if (nextGenFailures.nonEmpty)
                Left(s"Error retrieving next generation of $collName data: $nextGenFailures")
              else {
                // Make next gen collections into a map with the fk field names as the key and the
                // entries in a map with the DBID as key
                val nextGenColls = nextGenData.toMap.map {
                  case (fkName, pkList) => fkName -> pkList.toMap
                }
                // Return documents originally found with updated "label" to include new parents labels (pk value)
                Right(
                  docsFound.map {
                    case (dbID, docData) =>
                      // Get parent values
                      val fkValues =
                        docData.fkVals.flatMap {
                          case (fkFieldName, fkDBID) =>
                            nextGenColls.get(fkFieldName).flatMap(collData => collData.get(fkDBID))
                        }
                      // Separate parents with " || "
                      val fksWithSep =
                        if (nextFKs.size > 1)
                          fkValues.map(fkVal => "(" + fkVal + ")")
                        else
                          fkValues
                      val parentLabel = fksWithSep.mkString(" || ")
                      // Return new label: parents labels + (child label)
                      dbID -> (parentLabel + " (" + docData.pkVal + ")")
                  }
                )
              }
            })
          }
      }
    }

    // Get complete foreign key labels and sort results by labels (we return map with single entry to follow
    // convention for other methods returning foreign key results)
    getParentKeys(field)
      .map(
        _.map(fkData => Map(
          field.fk.field.name -> fkData.sortBy(_._2))
        )
      )
  }

  /**
   * Data for a single document.
   * @param fkVals list of foreign keys found: fieldName->fkDBID
   * @param pkVal primary key (i.e., label) for entry
   */
  private case class DocData(fkVals: List[(String, DBID)], pkVal: String)

  /**
   * Retrieve all entries in a collection, with corresponding label ("primary keys").
   * @param mdDefs metadata definitions
   * @param coll foreign key collection metadata
   * @return either error (Left) or Right(list of entryDBID->DocData)
   */
  private def getAllCollData(mdDefs: DMetaDefs, coll: ColDataEntry)
  : Future[Either[String, List[(DBID, DocData)]]] = {
    // Get foreign key field names we need to retrieve
    val fkFieldNames =
      mdDefs.relList.filter(rel => rel.fk.coll.id == coll.id)
        .map(_.fk.field.name)
    // Get all documents for the collection - map Future and Either
    getCollDocuments(mdDefs, coll).map(
      _.map(collVals => {
        // make list of DBID -> doc
        collVals.flatMap(entryData => { // flatMap to get rid of None where _id not found - should never happen
          entryData.get(ID_FIELD).map(entryID => {
            // Get foreign keys as map of foreignKeyName -> foreignKeyDBID (DBID to parent)
            val fkVals =
              fkFieldNames.flatMap(fieldName =>
                // Get foreign key if it exists (Nones will be eliminated by flatMap we're within)
                entryData.get(fieldName).map(fieldValue => fieldName -> fieldValue.toString)
              )
            // Get primary key value
            val pkVal = mdDefs.getCollEntryPK(coll = coll, entryData = entryData)
            entryID.toString -> DocData(fkVals, pkVal)
          })
        })
      })
    )
  }

  /**
   * Do a request for a single entry.
   * @param mdDefs metadata definitions
   * @param collName collection name (projectName_collectionName) for request
   * @param doRequest callback to do request - called with (projectURLname, collectionURLname)
   * @param idString string describing what we're retrieving (used for error messages)
   * @return Left with error message or Right with json returned by the request
   */
  def doIDRequest(mdDefs: DMetaDefs, collName: String, doRequest: (String, String) => Future[String], idString: String)
  : Future[Either[String, String]] = {
    mdDefs.collByName.get(collName) match {
      case None => Future(Left(s"Collection $collName not found"))
      case Some(coll) =>
        doCollIDRequest(mdDefs = mdDefs, coll = coll, doRequest = doRequest, idString = idString)
    }
  }

  /**
   * Do a request for a single entry.
   * @param mdDefs metadata definitions
   * @param coll collection for request
   * @param doRequest callback to do request - called with (projectURL, collectionURL) returns json from request
   * @param idString string describing what we're retrieving (used for error messages)
   * @return Left with error message or Right with json returned by the request
   */
  private[dmeta] def doCollIDRequest(mdDefs: DMetaDefs, coll: ColDataEntry,
                                     doRequest: (String, String) => Future[String], idString: String)
  : Future[Either[String, String]] = {
    mdDefs.getSlugs(coll) match {
      case None => Future(Left(s"Project not found for ${coll.label}"))
      case Some((projSlug, collSlug)) =>
        doRequest(projSlug, collSlug)
          .map(Right(_))
          .recover(e => {
            val msg = e.getLocalizedMessage
            if (msg.contains("code 404"))
              Left(s"${coll.label} entry for $idString$notFound")
            else
              Left(s"Failure accessing ${coll.label} entry for $idString: ${e.getLocalizedMessage}")
          })
    }
  }

  /**
   * Find entry with value set for standard field name across collections.  There must be only one entry across all
   * collections with the id value, otherwise we'll exit with an error.
   * @param mdDefs metadata definitions
   * @param id value to look for
   * @return either Left(error) or Right(collectionName, entryValues)
   */
  def findEntryByCommonID(mdDefs: DMetaDefs, id: String): Future[Either[String, (String, Map[String, Any])]] = {
    val findFutures =
      mdDefs.idFieldMap.map(fldEntry => {
        // Look for collection for field found with proper name
        mdDefs.collections.get(fldEntry.collectionID).flatMap(coll => mdDefs.getCollName(coll.id)) match {
          case None =>
            Future.successful(Left(s"Unable to find collection ${fldEntry.collectionID} for ${fldEntry.label}"))
          case Some(collName) =>
            // Go find any entries in collection that match criteria
            findEntryData(mdDefs = mdDefs, doc = Document(collName, Map(fldEntry.name -> id)))
              .map {
                case Left(err) =>
                  // If no entries found make that ok (Right) without any entries
                  if (isNotFound(err))
                    Right((collName, List.empty))
                  else
                    Left(err)
                case Right(found) =>
                  // If entry found then make sure there's only one
                  if (found.length > 1)
                    Left(s"${found.length} entries found in $collName for ${fldEntry.label} = $id")
                  else {
                    Right((collName, found))
                  }
              }
        }
      }
      )
    // Put all the futures into one future and then return with error(s) or entries found
    Future.sequence(findFutures).map(fut => {
      val (failures, entriesFound) = RequestUtils.getEitherLists(fut)
      if (failures.nonEmpty) Left(failures)
      else {
        // Find everywhere we found an entry (we've already filtered out collections with multiple entries)
        val entries =
          entriesFound.filter {
            case (_, entries) => entries.nonEmpty
          }
        if (entries.length > 1) {
          val entryCollections = entries.map(_._1).mkString(", ")
          Left(s"${entries.length} entries found for ${DMetaDefs.COMMON_FIELD} = $id across entry types $entryCollections")
        } else if (entries.isEmpty) {
          Left(s"No entries found for $COMMON_FIELD = $id")
        } else {
          // Got one wanted entry - return with what we got.
          val (collName, entryValues) = entries.head
          Right(collName, entryValues.head) // displayNewWorkflow(mdDefs, collName, entryValues.head)
        }
      }
    })
  }

  // Fixed ending to not found error
  private val notFound = " not found"

  /**
   * See if error was not found
   * @param errMsg error message
   * @return true if error is not found
   */
  def isNotFound(errMsg: String): Boolean = errMsg.endsWith(notFound)

  /**
   * Find DMeta documents that match specified criteria.
   * @param mdDefs metadata definitions
   * @param doc entry to search for (has type and data values)
   * @return Either error (Left) or list of map of values for entries found (Right)
   */
  def findEntryData(mdDefs: DMetaDefs, doc: Document): Future[Either[String, List[Map[String, Any]]]] = {
    /*
     * Find DMeta documents that match specified criteria.
     * @param coll collection metadata for entry
     * @param doc entry to search for (has data values)
     * @return Either error (Left) or json (Right)
     */
    def doDocFind(coll: ColDataEntry, doc: Document): Future[Either[String, String]] = {
      // If just looking by id then be more efficient and just look for document by that id
      if (doc.data.size == 1 && doc.data.head._1 == ID_FIELD) {
        val docID = doc.data.head._2.toString
        doFindRequest(mdDefs = mdDefs, coll = coll, id = docID, getURL = URLs.dmetaDocumentURL, idString = docID)
      } else {
        val idData = doc.data
        val id = idData.flatMap { case (k, v) => if (v.toString.isEmpty) None else Some(s"$k = $v") }.mkString(", ")
        val idStr = if (id.isEmpty) "unqualified criteria" else id
        doFindRequest(mdDefs = mdDefs, coll = coll, id = idData, getURL = URLs.dmetaFindURL, idString = idStr)
      }
    }

    findEntryJson(mdDefs = mdDefs, collName = doc.collectionName, criteria = doc, doFind = doDocFind)
      .map(_.flatMap(DMeta.parseJson))
  }

  /**
   * Find DMeta documents that match specified criteria.
   * @param mdDefs metadata definitions
   * @param collName entry collection name
   * @param criteria entry selection criteria
   * @param doFind callback to do find (ColDataEntry, criteria) => jsonReturned
   * @return Either error (Left) or returned json (Right)
   */
  private def findEntryJson[T](mdDefs: DMetaDefs, collName: String, criteria: T,
                               doFind: (ColDataEntry, T) => Future[Either[String, String]])
  : Future[Either[String, String]] = {
    mdDefs.collByName.get(collName) match {
      case None => Future.successful(Left(s"$collName collection not found"))
      case Some(coll) =>
        doFind(coll, criteria)
    }
  }

  /**
   * Find entry in DB
   * @param mdDefs metadata definitions
   * @param dbID database ID for entry
   * @param collName name of collection containing entry
   * @return Either error (Left) or map of values (Right)
   */
  def findEntryDataForIDWithCollName(mdDefs: DMetaDefs, dbID: DBID, collName: String)
  : Future[Either[String, Map[String, Any]]] = {
    mdDefs.collByName.get(collName) match {
      case Some(coll) =>
        findEntryDataForID(mdDefs = mdDefs, dbID = dbID, coll = coll)
      case None => Future.successful(Left(s"$collName not found"))
    }
  }

  /**
   * Find entry in DB
   * @param dbID database ID for entry
   * @param coll collection containing entry
   * @return Either error (Left) or map of values (Right)
   */
  def findEntryDataForID(mdDefs: DMetaDefs, dbID: DBID, coll: ColDataEntry): Future[Either[String, Map[String, Any]]] = {
    // Get json and map future, then flatMap Either returned
    doFindRequest(mdDefs = mdDefs, coll = coll, id = dbID, getURL = URLs.dmetaDocumentURL, idString = dbID).map(
      _.flatMap(
        json => {
          // Get single entry found or return with error
          val entry = DMeta.parseJson(json)
          entry.flatMap(entries =>
            if (entries.size == 1)
              Right(entries.head)
            else {
              Left(s"${entries.size} entries found for database ID $dbID")
            }
          )
        }
      )
    )
  }

  /**
   * Do a find request.
   * @param mdDefs metadata definitions
   * @param coll collection for request
   * @param id id to use to get request
   * @param getURL callback to get url for request - called with (projectURL, collectionURL, inputID)
   * @param idString string describing id (used for error messages)
   * @tparam T type of input parameter
   * @return Left with error message or Right with json returned by the request
   */
  private def doFindRequest[T](mdDefs: DMetaDefs, coll: ColDataEntry,
                               id: T, getURL: (String, String, T) => String, idString: String)
  : Future[Either[String, String]] = {
    val requestAction = {
      (projName: String, collName: String) =>  {
        Requests.getRequest(getURL(projName, collName, id))
      }
    }
    DMetaRequests.doCollIDRequest(mdDefs = mdDefs, coll = coll, doRequest = requestAction, idString = idString)
  }

  /**
   * Do a collection field request - get all documents for a collection
   * @param coll collection for request
   * @return Left with error message or Right with list of documents (returned as a map of fieldName->value)
   */
  private[dmeta] def getCollDocuments(mdDefs: DMetaDefs, coll: ColDataEntry): Future[Either[String, List[Map[String, Any]]]] = {
    mdDefs.getSlugs(coll) match {
      case None => Future(Left(s"Project not found for ${coll.label}"))
      case Some((projSlug, collSlug)) =>
        val url = URLs.dmetaCollectionURL(projSlug, collSlug)
        Requests.getRequest(url).map(Right(_))
          .recover(e => {
            Left(s"Failure retrieving ${coll.label}: ${e.getLocalizedMessage}")
          }).map(_.flatMap(DMeta.parseJson))
    }
  }

  /**
   * Get the foreign key values for a entry.  Note that returned lists of primary key docID and values
   * will contain only a single entry.
   * @param mdDefs metadata definitions
   * @param collName collection name (projectName_collectionName)
   * @param entryID DBID for document with foreign key entries
   * @return either Left(errorString) or Right(map of foreignKeyName->list(primaryKeyDocID, primaryKeyValue))
   */
  def findFKDataForID(mdDefs: DMetaDefs, collName: String, entryID: DBID)
  : Future[Either[String, Map[String, List[(DBID, String)]]]] = {
    /*
     * Callback to startup fetches for primary key values for collection entries that are pointed to by foreign keys in
     * the designated entry.
     * @param coll collection with foreign keys
     * @param fkRels relationships for collection foreign keys
     * @return eithers (one per foreign key) with Left(errorString) or Right(FKName, PK, List(PKDocValues))
     */
    def getFKValueForID(coll: ColDataEntry, fkRels: List[CollectionRelationship])
    : Future[List[Either[String, (String, PrimaryKey, List[Map[String, Any]])]]] = {
      findEntryDataForID(mdDefs = mdDefs, dbID = entryID, coll = coll).flatMap {
        case Left(err) => Future.successful(List(Left(err)))
        case Right(entryData) =>
          findFKValueWithData(mdDefs = mdDefs, fkRels = fkRels, entryData = entryData)
      }
    }
    // Do fetch of values using callback
    getFKValues(collName = collName, mdDefs = mdDefs, getKeyData = getFKValueForID)
  }

  /**
   * Get all possible foreign key values for a single relationship in a collection.  Note return map should have
   * only a single entry but is returned as a map to be compatible with other findFK methods.
   * @param mdDefs metadata definitions
   * @param collName collection name (projectName_collectionName)
   * @param field field we're looking for
   * @return either Left(errorString) or Right(map of foreignKeyName->list(primaryKeyDocID, primaryKeyValue))
   */
  def findFKDataForField(mdDefs: DMetaDefs, collName: String, field: CollectionRelationship)
  : Future[Either[String, Map[String, List[(DBID, String)]]]] = {
    /*
     * Callback to startup fetches for all primary key values for collections that are pointed to by foreign keys in
     * the designated collection.
     * @param coll collection with foreign keys
     * @param fkRels relationships for collection foreign keys
     * @return eithers (one per foreign key) with Left(errorString) or Right(FKName, PK, List(PKDocValues))
     */
    def getFKValueForField(coll: ColDataEntry, fkRels: List[CollectionRelationship])
    : Future[List[Either[String, (String, PrimaryKey, List[Map[String, Any]])]]] = {
      // Go fetch associated primary key values in primary key collections for relationship we're interested in
      fkRels.find(rel => rel.fk.field.id == field.fk.field.id) match {
        case None => Future.successful(List.empty)
        case Some(rel) =>
          val pkColl = rel.pk.coll
          // Get all documents in collection, map future and then and map successful (Right) values to
          // (foreignKeyfieldName, primaryKey, valuesInPrimaryCollectionDocument).  Note to do single entry list and
          // Future.sequence to follow callback signature.
          val pkFetch = List(
            getCollDocuments(mdDefs, pkColl)
              .map(
                _.map(collVals => (rel.fk.field.name, rel.pk, collVals))
              )
          )
          // Make list of futures into a single future returning a list
          Future.sequence(pkFetch)
      }
    }
    // Do fetch of values
    getFKValues(collName = collName, mdDefs = mdDefs, getKeyData = getFKValueForField)
  }
}
