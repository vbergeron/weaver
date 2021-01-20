import scala.quoted._
import scala.quoted.staging._
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map
import model._

object Compiler:
  given Toolbox = Toolbox.make(getClass.getClassLoader)

  def compileM(query:Query, reader: Expr[Reader])(using Quotes): Expr[Unit] =
    query match
      case node:Query.Scan      => node.source.readRecords(reader)
      case node:Query.Project   => compileM(node.parent, compileProject(node, reader))
      case node:Query.Filter    => compileM(node.parent, compileFilter(node, reader))
      case node:Query.Join      => compileJoin(node, reader)
      case node:Query.Aggregate => compileAggregate(node, reader)
      case node:Query.Print     => compileM(node.parent, compilePrint(node.parent.schema))

  def compile(query:Query): () => Unit = () => run {
    val it = compileM(query, noReader)
    println(it.show)
    it
  }

  def compileOperator(operator:Operator)(using Quotes): Expr[(String, String) => Boolean] =
    operator match
      case Operator.EQ => '{ _ == _ }
      case Operator.NE => '{ _ != _ }

  def compileFilter(filter:Query.Filter, cont:Expr[Reader])(using Quotes):Expr[Reader] = 
    val predicate = compileOperator(filter.predicate.op)
    '{ rec => {
      val lhs:String = ${Ref.deref(filter.schema, filter.predicate.left)('rec)}
      val rhs:String = ${Ref.deref(filter.schema, filter.predicate.right)('rec)}

      if(${Expr.betaReduce('{$predicate(lhs, rhs)})})
        ${Expr.betaReduce('{$cont(rec)})}
    }}
  
  def compileProject(project:Query.Project, cont:Expr[Reader])(using Quotes):Expr[Reader] =
    val indices = project.schema.columns.map(c => project.parent.schema.columns.indexOf(c))
    '{ rec => {
      val acc = new Array[String](${Expr(indices.length)})
      ${Expr.block(
        indices.toList.zipWithIndex.map((index, i) => 
          '{acc(${Expr(i)}) = rec.fields(${Expr(index)})}
        ), 
        '{}
      )}
      val res = Record(acc)
      ${Expr.betaReduce('{$cont(res)})}
    }}

  def compileJoin(join:Query.Join, cont:Expr[Reader])(using Quotes):Expr[Unit] =
    val lhsIndices = join.lhs.schema.columns.map(c => join.on.columns.indexOf(c))
    val rhsIndices = join.rhs.schema.columns.map(c => join.on.columns.indexOf(c))
    '{ 
      val lhs = ListBuffer.empty[Record] 
      val rhs = ListBuffer.empty[Record] 
      ${compileM(join.lhs, '{rec => lhs += rec})}
      ${compileM(join.rhs, '{rec => rhs += rec})}
      val acc = ListBuffer.empty[Record] 
    }

  def compileAggregate(agg:Query.Aggregate, cont:Expr[Reader])(using Quotes):Expr[Unit] =
    val indices = agg.parent.schema.columns.map(c => agg.by.columns.indexOf(c))
    '{ 
      val acc = Map.empty[Array[String], List[Record]] 
      ${compileM(agg.parent, '{rec => acc += Array("yolo") -> rec})}
    }
    
  def compilePrint(schema:Schema)(using Quotes):Expr[Reader] =
    '{rec => println(${Expr(schema.toString)} + rec.toString)}

  def noReader(using Quotes):Expr[Reader] = 
    '{_ => ()}
  

