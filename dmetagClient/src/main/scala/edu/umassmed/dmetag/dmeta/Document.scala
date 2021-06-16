package edu.umassmed.dmetag.dmeta

/**
 * "Document" used for queries for entries.
 * @param collectionName entry type name (projectName_collectionName)
 * @param data data values to search for (map of key/value pairs)
 */
case class Document(collectionName: String, data: Map[String, Any])
