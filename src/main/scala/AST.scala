import model.{Record, Schema}

import scala.quoted._
import scala.quoted.staging._

sealed trait Query

object Query {
  final case class Scan(source: DataSource) extends Query
  final case class Project(parent: Query, newSchema: Schema) extends Query
  final case class Print(parent: Query) extends Query

  given Toolbox = Toolbox.make(getClass.getClassLoader)

  trait Compiler {
    def compile(schema:Schema, query:Query):Record => Unit
  }


}
