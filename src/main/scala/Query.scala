import model._

import scala.quoted._
import scala.quoted.staging._

sealed trait Query {
  def schema:Schema
}

object Query {
  final case class Scan(source: DataSource) extends Query {
    def schema = source.readSchema
    def project(to:Schema):Project = Project(this, to)
    def filter(predicate:Predicate):Filter = Filter(this, predicate)
    def print: Print = Print(this)
  }
  final case class Project(parent: Query, to: Schema) extends Query {
    def schema = to
    def project(to:Schema):Project = Project(this, to)
    def filter(predicate:Predicate):Filter = Filter(this, predicate)
    def print: Print = Print(this)
  }
  final case class Filter(parent: Query, predicate: Predicate) extends Query {
    def schema = parent.schema 
    def project(to:Schema):Project = Project(this, to)
    def filter(predicate:Predicate):Filter = Filter(this, predicate)
    def print: Print = Print(this)
  }
  final case class Print(parent: Query) extends Query {
    def schema = Schema(Array())
  }
  
}