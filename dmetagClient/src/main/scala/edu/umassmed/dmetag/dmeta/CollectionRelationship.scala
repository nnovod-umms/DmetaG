package edu.umassmed.dmetag.dmeta

import edu.umassmed.dmetag.dmeta.DMeta.{ColDataEntry, FldDataEntry, ProjDataEntry}
import DMeta.{ColDataEntry, FldDataEntry, ProjDataEntry}

/**
 * Relationship between collections.
 * @param fk foreign key (pointer from child collection to parent)
 * @param pk primary key (parent collection info)
 */
private[dmeta] case class CollectionRelationship(fk: ForeignKey, pk: PrimaryKey)


/**
 * Foreign key from child to parent collection.
 * @param project project of collection
 * @param coll collection of field
 * @param field foreign key field (ObjectID field that points to parent collection entry)
 */
private[dmeta] case class ForeignKey(project: Option[ProjDataEntry], coll: ColDataEntry, field: FldDataEntry)

/**
 * Primary key for a parent collection.
 * @param project project of collection
 * @param coll collection of fields
 * @param fields primary key fields (fields that are used to identify a collection document)
 */
private[dmeta] case class PrimaryKey(project: Option[ProjDataEntry], coll: ColDataEntry, fields: List[FldDataEntry])

