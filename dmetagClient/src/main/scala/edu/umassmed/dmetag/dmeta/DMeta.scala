package edu.umassmed.dmetag.dmeta

import ujson.Value
import upickle.default.{macroRW, ReadWriter => RW, _}

import scala.scalajs.js.Date

/**
 * Classes and methods to (de)serialize Dolphin Meta definitions to/from json.  ujson is used to make easy "RW" macros
 * to do the conversions between case classes and json.
 */
object DMeta {
  import DMetaImplicits._

  /**
   * Status check
   * @param status should be success or fail
   */
  case class Status(status: String) {
    /**
     * Did request fail?
     * @return true if request is failed
     */
    def isNotOK: Boolean = this.status.toLowerCase().contains("fail")

    /**
     * Did request succeed?
     * @return true if request is successful
     */
    def isOK: Boolean = this.status.toLowerCase().contains("success")
  }

  object Status {
    // Make uPickle json <--> object converters
    implicit val rw: RW[Status] = macroRW

    /**
     * Check status of returned json
     * @param json returned json
     * @return error message is status not OK
     */
    def checkJsonStatus(json: String): Option[String] = {
      // Check if success before serializing entire response - if failure then just return error
      val status = read[Status](json)
      if (status.isNotOK) {
        val requestFailure = read[ErrorStatus](json)
        Some(s"${requestFailure.message} (code ${requestFailure.error.statusCode})")
      } else
        None
    }


  }

  /**
   * Failed request status information.
   * @param status should contain fail
   * @param error specifics of error
   * @param message error message
   * @param stack stack trace where error occurred
   */
  case class ErrorStatus(status: String, error: ErrorInfo, message: String, stack: String)
  object ErrorStatus {
    // Make uPickle json <--> object converters
    implicit val rw: RW[ErrorStatus] = macroRW
  }

  /**
   * Specifics for error
   * @param statusCode HTML error code
   * @param status error status
   * @param isOperational operational (run-time problem to be handled) vs non-operational (program bug)
   */
  case class ErrorInfo(statusCode: Int, status: String, isOperational: Boolean)
  object ErrorInfo {
    // Make uPickle json <--> object converters
    implicit val rw: RW[ErrorInfo] = macroRW
  }

  /**
   * Login returned data
   *
   * @param status  return status for request
   * @param token JSON web token to be used for authentication of future requests
   * @param data login information
   */
  case class Login(status: String, token: String, data: LoginData)
  object Login {
    // Make uPickle json <--> object converters
    implicit val rw: RW[Login] = macroRW
  }

  /**
   * Login data
   * @param user user information
   */
  case class LoginData(user: LoginUser)
  object LoginData {
    // Make uPickle json <--> object converters
    implicit val rw: RW[LoginData] = macroRW
  }

  /**
   * Login user information.
   * @param photo photo file
   * @param role user role
   * @param email user e-mail address
   * @param name user name
   * @param username user login name
   * @param scope user scope
   * @param sso_id single sign on id
   */
  case class LoginUser(photo: String, role: String, email: String, name: String, username: String,
                       scope: String, sso_id: String)
  object LoginUser {
    // Make uPickle json <--> object converters
    implicit val rw: RW[LoginUser] = macroRW
  }

  /**
   * Top level for all dmeta returned data including
   * ProjDataEntry via https://dmeta-skin.dolphinnext.com/api/vi/projects/{project_id}
   * @param status  return status for request
   * @param data    returned results
   * @tparam T type of results (covariant to allow FldDataForForm and subclasses)
   */
  case class MetaDataSingle[+T](status: String, data: MetaDataArray[T])

  // Note: a new implicit val is needed for each new type of results
  object MetaDataSingle {
    // Make uPickle json <--> object converters
    implicit val rw: RW[MetaDataSingle[ProjDataEntry]] = macroRW
  }

  /**
   * Top level for all dmeta returned data including
   * ColDataEntry via https://dmeta-skin.dolphinnext.com/api/v1/collections
   * FldDataEntry via https://dmeta-skin.dolphinnext.com/api/v1/fields?collectionID=id
   * ProjDataEntry via https://dmeta-skin.dolphinnext.com/api/vi/projects
   * @param status  return status for request
   * @param results # of results returned
   * @param data    returned results
   * @tparam T type of results (covariant to allow FldDataForForm and subclasses)
   */
  case class MetaData[+T](status: String, results: Int, data: MetaDataArray[T]) {
    /**
     * Add new entries to the metadata field list.
     * @param newData list field entries
     * @tparam U lower bound type based on T (can be T or subclass) - needed for lists
     * @return copy with new metadata fields added
     */
    def amend[U >: T](newData: List[U]): MetaData[U] = {
      val allData = data.data.appendedAll(newData)
      copy(results = this.results + newData.size, data = MetaDataArray(allData))
    }
  }

  // Note: a new implicit val is needed for each new type of results
  object MetaData {
    // Make uPickle json <--> object converters
    implicit val rw: RW[MetaData[ColDataEntry]] = macroRW
    implicit val rw2: RW[MetaData[FldDataEntry]] = macroRW
    implicit val rw3: RW[MetaData[ProjDataEntry]] = macroRW
  }

  /**
   * Array of values returned by dmeta
   *
   * @param data array of results
   * @tparam T type of results (covariant to allow FldDataForForm and subclasses)
   */
  case class MetaDataArray[+T](data: List[T])
  // Note: a new implicit val is needed for each new type of results
  object MetaDataArray {
    // Make uPickle json <--> object converters
    implicit val rw: RW[MetaDataArray[ColDataEntry]] = macroRW
    implicit val rw2: RW[MetaDataArray[FldDataEntry]] = macroRW
    implicit val rw3: RW[MetaDataArray[ProjDataEntry]] = macroRW
  }

  /**
   * Groups given read access to item
   *
   * @param read list of groups given read access
   */
  case class DataReadPermission(read: DataRestrictGroup)
  object DataReadPermission {
    // Make uPickle json <--> object converters
    implicit val rw: RW[DataReadPermission] = macroRW
  }

  /**
   * List of groups given access
   *
   * @param group IDs of groups given access
   */
  case class DataRestrictGroup(group: List[String])
  object DataRestrictGroup {
    // Make uPickle json <--> object converters
    implicit val rw: RW[DataRestrictGroup] = macroRW
  }

  /**
   * Is data required?
   *
   * @param required required value?
   * @param msg      message to display if required value not supplied
   */
  case class DataRequired(required: Boolean, msg: Option[String])
  object DataRequired {
    // Custom upickle read/writer to (de)serialize DataRequired.  Needed because currently the json "required" can
    // either be a simple boolean or it can be an array containing the required boolean and a message to display for
    // why the field is required, for example "required": [true, "We like this field"]
    implicit val requiredReadWrite: RW[DataRequired] =
    readwriter[ujson.Value].bimap[DataRequired](
      {
        case DataRequired(required, None) => ujson.Bool(required)
        case DataRequired(required, Some(msg)) => ujson.Arr(required, msg)
      },
      {
        case j: ujson.Arr if j.arr.nonEmpty =>
          val jArray = j.arr
          if (jArray.length >= 2)
            DataRequired(jArray(0).bool, Some(jArray(1).str))
          else
            DataRequired(jArray(0).bool, None)
        case jBool: ujson.Bool =>
          DataRequired(jBool.bool, None)
        case _ =>
          DataRequired(required = false, msg = None)
      }
    )
  }

  /**
   * Definition of an individual project (a group of collection definitions)
   * @param id                 project id
   * @param name               project name
   * @param label              project label
   * @param slug               URL identifier
   * @param active             is collection active?
   */
  case class ProjDataEntry(id: String, name: String, label: String, slug: String, active: Boolean)
  object ProjDataEntry {
    // Make uPickle json <--> object converters
    implicit val rw: RW[ProjDataEntry] = macroRW
  }

  /**
   * Definition of an individual collection (a group of field definitions).  (Commented out fields we don't need.)
   * @param id                 collection id
   * @param name               collection name
   * @param label              collection label
   * @param slug               URL identifier
   * @param parentCollectionID parent collection id
   * @param version            collection version
   * //@param required           is collection required?
   * @param active             is collection active?
   * //@param restrictTo         groups access is restricted to
   * //@param perms              groups given read permission
   * //@param creationDate       collection creation date
   * //@param lastUpdateDate     collection last update date
   * //@param lastUpdatedUser    last user to update collection
   * //@param owner              owner of collection
   * @param projectID          ID for project collection is a part of
   */
  case class ColDataEntry(id: String, name: String, label: String, slug: String,
                          parentCollectionID: Option[String] = None, version: Int,
//                          required: Option[DataRequired],
                          active: Boolean,
// Silently causes deserialization to fail: restrictTo: Option[DataRestrictGroup] = None,
//                          perms: Option[DataReadPermission],
//                          creationDate: Date,
//                          lastUpdateDate: Date, lastUpdatedUser: Option[String] = None,
//                          owner: Option[String] = None,
                          projectID: Option[String] = None
                         )
  object ColDataEntry {
    // Make uPickle json <--> object converters
    implicit val rw: RW[ColDataEntry] = macroRW
  }

  /**
   * Base for definition entry for an individual fields in a form
   */
  trait FldDataForForm {
    val name: String // field name
    val label: String // field display label
    val `type`: String // field type
    val enum: List[String] // list of enumerations allowed for field - empty if not enumerated field
    val required: DataRequired // is field required?
    val active: Boolean // is field active?
    val unique: Boolean // does field contain unique values?
    val header: Boolean // Is field to be used for header (e.g., primary key)
  }

  /**
   * Definition entry for an individual field that can be used for fields in a form not stored in DMeta.
   * @param name            field name
   * @param label           field display label
   * @param `type`          field type
   * @param enum            list of enumerations allowed for field - empty if not enumerated field
   * @param required        is field required?
   * @param active          is field active?
   * @param unique          does field contain unique values?
   * @param header          is field to be used in header? (e.g., primary key)
   */
  case class FldData(name: String, label: String, `type`: String, enum: List[String] = List.empty,
                     required: DataRequired, active: Boolean,
                     unique: Boolean, header: Boolean = false) extends FldDataForForm

  /**
   * Definition entry for a individual field in a collection.  (Commented out fields we don't need.)
   * @param id field DB ID
   * @param name field name
   * @param label field display label
   * @param `type` field type
   * @param enum list of enumerations allowed for field - empty if not enumerated field
   * @param required is field required?
   * @param active is field active?
   * //@param perms           groups given read permission
   * //@param creationDate    field creation date
   * //@param lastUpdateDate  date field last updated
   * @param collectionID ID for collection field is a part of
   * //@param lastUpdatedUser last user to update field
   * //@param owner           field owner
   * @param ref referenced catalog if type is mongoose.Schema.ObjectID
   * @param unique does field contain unique values?
   * @param header is field to be used in header? (e.g., primary key field)
   * @param parent field was made to represent a parent collection pointer
   * @param hidden field is hidden
   */
  case class FldDataEntry(id: String, name: String, label: String, `type`: String, enum: List[String] = List.empty,
                          required: DataRequired, active: Boolean,
                          //perms: DataReadPermission, creationDate: Date, lastUpdateDate: Date,
                          collectionID: String, //lastUpdatedUser: Option[String] = None, owner: Option[String] = None,
                          ref: Option[String] = None,
                          unique: Boolean = false, header: Boolean = false,
                          parent: Boolean = false, hidden: Boolean = false
                         ) extends FldDataForForm
  object FldDataEntry {
    // Make uPickle json <--> object converters
    implicit val rw: RW[FldDataEntry] = macroRW
  }

  // To get sample data: https://dmeta-skin.dolphinnext.com/api/v1/projects/vitiligo/data/sample

  /**
   * Implicit readers/writers for (de)serializing some non-standard fields
   */
  private object DMetaImplicits {
    // upickle reader/writer for js Date
    implicit val dateReadWrite: RW[Date] =
      readwriter[ujson.Value].bimap[Date](
        date => {
          ujson.Str(date.toISOString())
        },
        json => {
          new Date(json.str)
        }
      )

    // upickle default for option is to have it an array: empty for None, a 1-element array with the value for Some.
    // Here we change that, at least for Option[String], to be either a blank string or the string value.
    implicit val optionStrReadWrite: RW[Option[String]] =
    readwriter[ujson.Value].bimap[Option[String]](
      {
        case Some(s) => ujson.Str(s)
        case None => ujson.Str(null)
      },
      json => {
        if (json.isNull || json.str.isEmpty)
          None
        else
          Some(json.str)
      }
    )

    // upickle default for option is to have it an array: empty for None, a 1-element array with the value for Some.
    // Here we change that, at least for Option[DataRestrictGroup], to be either a blank string or the string value.
    implicit val optionDataRestrictGroupReadWrite: RW[Option[DataRestrictGroup]] =
    genericOption[DataRestrictGroup](DataRestrictGroup.rw)

    // upickle default for option is to have it an array: empty for None, a 1-element array with the value for Some.
    // Here we change that to be either null or the object value.
    private def genericOption[T: RW]: RW[Option[T]] =
      readwriter[ujson.Value].bimap[Option[T]](
        {
          case Some(obj) => ujson.Str(write[T](obj))
          case None => ujson.Str(null)
        },
        json => {
          if (json.isNull || json.str.isEmpty) {
            None
          } else {
            Some(read[T](json.str))
          }
        }
      )
  }

  /**
   * Parse json returned from a DMeta request, with data, into a simple map (key->value).
   * @param json json returned from request
   * @return either left with an error message or right with json parsed into a map
   */
  def parseJson(json: String): Either[String, List[Map[String, Any]]] = {
    // Check if success before serializing entire response - if failure then just return error
    Status.checkJsonStatus(json) match {
      case Some(err) => Left(err)
      case None =>
        // Read json as a whole
        val parsedJson = ujson.read(json)
        // Get "data" region
        parsedJson("data").objOpt match {
          case None => Left(s"No data object found in $json")
          case Some(dataStr) =>
            val parsedData = ujson.read(dataStr)

            /*
             * Get values for map of json values
             * @param values map of input json values
             * @return map of actual values in json
             */
            def getValues(values: Map[String, Value]) = {
              values.map {
                case (k, v) =>
                  k -> (v.arrOpt match {
                    // If an array simply write out what's in the array
                    case Some(arr) =>
                      ujson.write(arr)
                    // Next check if it's a hashMap
                    case None =>
                      v.objOpt match {
                        // If a hashmap (i.e., a mongo document) simply write out what's in the hashmap
                        case Some(hashMap) => ujson.write(hashMap)
                        // Otherwise get value
                        // note: ujson.write for a single value comes out with quotes around it which we don't want
                        case None =>
                          val valValue = v.value
                          if (valValue == null) "" else valValue
                      }
                  })
              }
            }

            // Get "data" array
            parsedData("data").arrOpt match {
              case None =>
                // If no array check for a single value (for single entry requests)
                parsedData("data").objOpt match {
                  case None =>
                    Left(s"No data array found in $dataStr")
                  case Some(entry) =>
                    Right(List(getValues(entry.obj.toMap)))
                }
              case Some(entries) =>
                Right(
                  entries.map(
                    entry => getValues(entry.obj.toMap)
                  ).toList
                )
            }
        }
    }
  }
}