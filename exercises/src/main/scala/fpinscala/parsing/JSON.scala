package fpinscala.parsing

sealed trait JSON
object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON

  def jsonParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
    import P._
    val spaces = char(' ').many.slice
    ???
  }

  object Main extends App {

    val jsonString = """{
    "firstName": "John",
    "lastName": "Smith",
    "isAlive": true,
    "age": 27}"""

//        run(jsonParser(???)(jsonString)) == Right(JObject(Map("firstName" -> JString("John"))))

  }
}
