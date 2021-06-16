package edu.umassmed.dmetag.dmeta

import com.raquo.laminar.api._

/**
 * Keep track of meta definitions recorded
 */
object DMetaDefsVar {
  // Remember metadata definitions (none at start)
  private val DMetaDefsVarResponse: L.Var[Option[DMetaDefs]] = L.Var[Option[DMetaDefs]](None)

  /**
   * Get current metadata definitions
   * @return metadata definitions currently set
   */
  def getMDDef: Option[DMetaDefs] = DMetaDefsVarResponse.now()

  /**
   * Are metadata definitions set?
   * @return true if definitions set
   */
  def isMDdefsSet: Boolean = getMDDef.nonEmpty

  /**
   * Set current metadata definitions.
   * @param dmetaDefs metadata definitions to set
   * @return metadata definitions set (same as input)
   */
  def setMDDef(dmetaDefs: DMetaDefs): DMetaDefs = {
    DMetaDefsVarResponse.set(Some(dmetaDefs))
    dmetaDefs
  }

  /**
   * Clear current metadata definitions
   */
  def clearMDDef: Unit = DMetaDefsVarResponse.set(None)
}
