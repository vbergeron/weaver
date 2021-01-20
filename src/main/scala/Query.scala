import model._

sealed trait Query:
  def schema:Schema

object Query:
  
  final case class Scan(source: DataSource) extends Query:
    def schema = source.readSchema
    def project(to:Schema):Project = Project(this, to)
    def filter(predicate:Predicate):Filter = Filter(this, predicate)
    def print: Print = Print(this)
  
  final case class Project(parent: Query, to: Schema) extends Query:
    def schema = to
    def project(to:Schema):Project = Project(this, to)
    def filter(predicate:Predicate):Filter = Filter(this, predicate)
    def print: Print = Print(this)
  
  final case class Filter(parent: Query, predicate: Predicate) extends Query:
    def schema = parent.schema 
    def project(to:Schema):Project = Project(this, to)
    def filter(predicate:Predicate):Filter = Filter(this, predicate)
    def print: Print = Print(this)

  final case class Join(lhs:Query, rhs:Query, on:Schema) extends Query:
    def schema = Schema(lhs.schema.columns ++ rhs.schema.columns)
    def project(to:Schema):Project = Project(this, to)
    def filter(predicate:Predicate):Filter = Filter(this, predicate)
    def print: Print = Print(this)

  final case class Aggregate(parent:Query, by:Schema) extends Query:
    def schema = by
    def project(to:Schema):Project = Project(this, to)
    def filter(predicate:Predicate):Filter = Filter(this, predicate)
    def print: Print = Print(this)
  
  final case class Print(parent: Query) extends Query:
    def schema = Schema(Array())