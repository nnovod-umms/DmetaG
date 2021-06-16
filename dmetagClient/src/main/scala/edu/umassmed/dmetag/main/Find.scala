package edu.umassmed.dmetag.main

import edu.umassmed.dmetag.dmeta.{DMetaDefs, DMetaRequests, Document}
import edu.umassmed.dmetag.utils.RequestUtils
import edu.umassmed.dmetag.dmeta.DMeta.FldDataEntry
import edu.umassmed.dmetag.utils.Types.DBID

import scala.collection.mutable.ListBuffer
import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

object Find {
  /**
   * Do find with specified criteria and return either a single entry found or a list of possible entries that match
   * the criteria.  If a list (Right inner Either) is returned all entries have their foreign keys set with values from
   * the foreign collections.  Also, for lists of entries, the list of values returned is in same order as the list of
   * field metadata returned.  If a single value is returned (Left inner Either) then a map of values is returned.
   * @param mdDefs metadata definitions
   * @param collName collection name
   * @param entryCriteria search criteria
   * @return Either Left(error) or Either Left((DBID, EntryValues) or Right(listOfFldMetadata, listOfDBIDsAndFieldValues)
   */
  private[main] def getFindRows(mdDefs: DMetaDefs, collName: String, entryCriteria: Map[String, Any])
  : Future[Either[String, Either[(DBID, Map[String, Any]), (List[FldDataEntry], List[(DBID, List[String])])]]] = {
    /*
     * Look for wanted entries and return with list of entries found.
     * @param mdDefs metadata definitions
     * @param collName collection name
     * @param entryCriteria search criteria
     * @return Either Left(errorMessage) or Right(List(Map(fieldName->fieldValue)))
     */
    def doFind(mdDefs: DMetaDefs, collName: String, entryCriteria: Map[String, Any])
    : Future[Either[String, List[Map[String, Any]]]] = {
      DMetaRequests.findEntryData(mdDefs, Document(collectionName = collName, data = entryCriteria)).map {
        case Left(err) =>
          // Make pretty error message if entry not found
          val errMsg =
            if (DMetaRequests.isNotFound(err)) {
              val valStr = entryCriteria.toList.flatMap {
                case (k, v) =>
                  val valStr = v.toString
                  if (valStr.nonEmpty) Some(s"$k = ${v.toString}") else None
              }.mkString(", ")
              val collStr = mdDefs.collLabels.getOrElse(collName, collName)
              val valueStr = if (valStr.isEmpty) valStr else s" where $valStr"
              s"No entries found for entry type $collStr$valueStr"
            } else
              err
          // Say we're done with error
          Left(errMsg)
        case Right(entryMap) =>
          Right(entryMap)
      }
    }

    // Go find entries we want
    doFind(mdDefs, collName, entryCriteria).flatMap {
      case Left(err) =>
        // Say we're done with error
        Future.successful(Left(err))
      case Right(entryMap) =>
        // If more than one entry then fill in foreign keys so we can setup a table with rows to pick from
        if (entryMap.length != 1) {
          // Get values for possible rows
          val (values, headers) = getRowsWithIDs(mdDefs = mdDefs, collName = collName, entryMap = entryMap)
          // Replace foreign keys (initially simply IDs) with values
          setFKValues(mdDefs = mdDefs, collName = collName, values = values, fieldNames = headers)
            .map( /* Future */
              _.flatMap( /* Right of Either */
                entriesWithFKs => {
                  // Return headers we found and entries with list of values (with values in same order as headers)
                  Right(Right(headers, entriesWithFKs))
                }
              )
            )
        } else {
          // If a single entry then just return that without filling in foreign keys (it will be done later when
          // the graph is created
          val entry = entryMap.head
          entry.get(DMetaDefs.ID_FIELD) match {
            case None => Future.successful(Left(s"Can not find _id in $entry"))
            case Some(dbID) =>
              // Return single entry found
              Future.successful(Right(Left((dbID.toString, entry))))
          }
        }
    }
  }

  /**
   * Get header row and value rows.  Columns are made for each field in the collection that is not hidden.
   * The column headings are set to the fields definitions.  The rows are returned sorted by their 1st column.
   * @param mdDefs metadata definitions
   * @param collName collection name (projectName_collectionName)
   * @param entryMap list of entries to set in value rows
   * @return (entry value rows (id, column values), header column field definitions)
   */
  private def getRowsWithIDs(mdDefs: DMetaDefs, collName: String, entryMap: List[Map[String, Any]])
  : (List[(String, List[String])], List[FldDataEntry]) = {
    // Get unique fields in entries
    val entryKeys = entryMap.flatMap(e => e.keys).toSet
    // Get list of entry fields in collection
    val keyFields = mdDefs.getVisibleFields(collName, entryKeys)
    // Get rows of values
    val values =
      entryMap.map(entry => {
        // Get values from entries, along with index to position in headings
        val valuesWithIndex =
          entry.flatMap {
            case (key, v) =>
              val index = keyFields.indexWhere(heading => heading.name == key)
              if (index == -1)
                None
              else
                Some(index -> v)
          }
        // Make list to contain values
        val valuesList = new ListBuffer[String]
        // Initialize values
        valuesList.addAll(List.fill(keyFields.length)(""))
        // Fill values into list
        valuesWithIndex.foreach {
          case (index, value) =>
            valuesList(index) = value.toString
        }
        // Return list made with id for row
        (entry.getOrElse(DMetaDefs.ID_FIELD, "UNKNOWN").toString, valuesList.toList)
      })
    (values, keyFields)
  }

  /**
   * Replace foreign key id values with values from foreign collections.
   * @param mdDefs metadata definitions
   * @param collName collection name (projectName_collectionName)
   * @param values list of (entryId, field values)
   * @param fieldNames list of field entry metadata in same order as field values
   * @return either Left(error) or Right(entryDBID -> valuesWithFKIDsReplacedWithValuesInSameOrderAsOriginalInput)
   */
  private def setFKValues(mdDefs: DMetaDefs, collName: String,
                          values: List[(DBID, List[String])], fieldNames: List[FldDataEntry])
  : Future[Either[String, List[(DBID, List[String])]]] = {
    // A little efficiency trick which should be made much better based on size of foreign key collections but for now...
    // If more than 25 entries then get all foreign keys possible at once, otherwise go through each entry one at a time
    // to get foreign key values specific to individual entries
    if (values.length > 25) {
      DMetaRequests.findFKDataForAll(mdDefs = mdDefs, collName = collName, fkDataFound = None)
        .map(/* Get Future */
          _.map( /* Get Right */
            fkMap => {
              val fkIDMap = fkMap.map{case (k, v) => k -> v.toMap}
              // Create map of fieldName to field value, replacing foreign key IDs where appropriate
              values.map {
                case (entryID, entryData) =>
                  // Replace foreign key field values with foreign collection primary key value
                  val valuesWithPKs =
                    entryData.zipWithIndex.map {
                      case (fieldValue, fieldIndex) =>
                        // Get map of foreign keys and then look for one we want (based on FKID)
                        // If foreign key value found then do replacement
                        fkIDMap.get(fieldNames(fieldIndex).name)
                          .flatMap(_.get(fieldValue)) match {
                          case Some(pkVal) =>
                            pkVal
                          case None =>
                            fieldValue
                        }
                    }
                  // Return new entry values with replaced foreign keys
                  entryID -> valuesWithPKs
              }
            }
          )
        )
    } else
      setIndividualFKValues(mdDefs = mdDefs, collName = collName, values = values, fieldNames = fieldNames)
  }

  /**
   * Replace foreign key id values with values from foreign collections, doing foreign key retrieval
   * a single entry at a time.
   * @param mdDefs metadata definitions
   * @param collName collection name (projectName_collectionName)
   * @param values list of (entryId, field values)
   * @param fieldNames list of field entry metadata in same order as field values
   * @return either Left(error) or Right(entryDBID -> valuesWithFKIDsReplacedWithValuesInSameOrderAsOriginalInput)
   */
  private def setIndividualFKValues(mdDefs: DMetaDefs, collName: String,
                                    values: List[(DBID, List[String])], fieldNames: List[FldDataEntry])
  : Future[Either[String, List[(DBID, List[String])]]] = {
    // Get group of futures to replace all the foreign keys with their corresponding value in the foreign collections
    val fkFutures =
      values.map {
        case (entryID, entryData) =>
          // Create map of fieldName to field value
          val entryDataMap =
            fieldNames.zipWithIndex.map {
              case (fieldDefinition, fieldIndex) => fieldDefinition.name -> entryData(fieldIndex)
            }.toMap
          // Go fetch foreign key data for entry
          DMetaRequests.findFKDataForIDWithData(
            mdDefs = mdDefs, collName = collName,
            entryID = entryID, entryData = entryDataMap
          ).map(/* Get Future */
              _.flatMap( /* Get Right result */
                fkMap => {
                  // Replace foreign key field values with foreign collection primary key value
                  val valuesWithPKs =
                    entryData.zipWithIndex.map {
                      case (fieldValue, fieldIndex) =>
                        // If foreign key value found then do replacement
                        fkMap.get(fieldNames(fieldIndex).name) match {
                          case Some(pkVal) =>
                            pkVal.map(_._2).mkString(" / ")
                          case None =>
                            fieldValue
                        }
                    }
                  // Return new entry values with replaced foreign keys
                  Right(entryID -> valuesWithPKs)
                }
              )
            )
      }
    // Put all the futures into one future and then return with error(s) or values with foreign key values
    Future.sequence(fkFutures).map(fut => {
      val (failures, valuesWithFKs) = RequestUtils.getEitherLists(fut)
      if (failures.nonEmpty) Left(failures)
      else {
        Right(valuesWithFKs)
      }
    })
  }
}
