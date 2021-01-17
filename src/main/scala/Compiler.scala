import scala.quoted._
import scala.quoted.staging._
import model._

object Compiler {
  given Toolbox = Toolbox.make(getClass.getClassLoader)
  
  def compileM(query:Query, reader: Expr[Reader])(using Quotes): Expr[Unit] = {
    query match {
      case Query.Scan(source) => 
        source.readRecords(reader)
      case Query.Project(parent, to) => 
        compileM(parent, compileProjection(parent.schema, to)(reader))
      case Query.Filter(parent, predicate) => 
        compileM(parent, compileFilter(parent.schema, predicate)(reader))
      case Query.Print(parent) => 
        compileM(parent, compilePrint(parent.schema))
    }
  }
  def compile(query:Query): () => Unit = () => run {
    val it = compileM(query, noReader)
    println(it.show)
    it
  }
}