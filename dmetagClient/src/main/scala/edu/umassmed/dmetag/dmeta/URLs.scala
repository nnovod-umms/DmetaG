package edu.umassmed.dmetag.dmeta

import DMetaDefs.MDID

object URLs {
  // A few URLs that are needed - root url should eventually be moved to config file
  //private val DMetaSkinURL: String =  "https://dmeta-skin.dolphinnext.com/api/v1"
  private val DMetaSkinURL: String =  "http://localhost:4000/api/v1"
  val DMetaCollectionURL: String = s"$DMetaSkinURL/collections"
  val DMetaProjectURL: String = s"$DMetaSkinURL/projects"
  val DMetaFieldURL: String = s"$DMetaSkinURL/fields"
  val DMetaLoginURL: String = s"$DMetaSkinURL/users/login"

  /**
   * Get URL to fetch a collection's field definitions
   * @param collectionID collection id
   * @return URL to fetch fields
   */
  def dmetaFieldURL(collectionID: String): String = s"$DMetaSkinURL/fields?collectionID=$collectionID"

  /**
   * Get URL to fetch a project's definitions
   * @param projectID project id
   * @return URL to fetch project definition
   */
  def dmetaProjectDataURL(projectID: String): String = s"$DMetaProjectURL/$projectID"

  /**
   * Get URL to fetch a collection's data
   * @param projectName project name
   * @param collectionName collection name
   * @return url for fetching collection data
   */
  def dmetaCollectionURL(projectName: String, collectionName: String) =
    s"$DMetaProjectURL/$projectName/data/$collectionName"

  /**
   * Get URL to fetch an individual document from a collection
   * @param projectName project name
   * @param collectionName collection name
   * @param docID document ID
   * @return url for fetching document
   */
  def dmetaDocumentURL(projectName: String, collectionName: String, docID: MDID) =
    s"${dmetaCollectionURL(projectName, collectionName)}/$docID"

  /**
   * Get URL to fetch a collection's data based on query fields
   * @param projectName project name
   * @param collectionName collection name
   * @param criteria map of fieldName->value to query for
   * @return url for fetching collection data
   */
  def dmetaFindURL(projectName: String, collectionName: String, criteria: Map[String, Any]) = {
    // Get rid of empty fields
    val findFields = criteria.filter {
      case (_, value) => value.toString.nonEmpty
    }
    // Get query parameters
    val queryStr =
      if (findFields.isEmpty) ""
      else {
        val queryList =
          findFields.toList.map {
            case (key, value) => s"$key=$value"
          }
        s"?${queryList.mkString("&")}"
      }
    // Make URL with query parameters
    s"${dmetaCollectionURL(projectName, collectionName)}$queryStr"
  }
}
