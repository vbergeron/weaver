import scala.quoted._
import scala.quoted.staging._
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map
import model._
import scala.concurrent.duration._

object Compiler:
  given Toolbox = Toolbox.make(getClass.getClassLoader)

  def compileM(query:Query, reader: Expr[Reader])(using Quotes): Expr[Unit] =
    query match
      case node:Query.Scan      => node.source.readRecords(reader)
      case node:Query.Project   => compileM(node.parent, compileProject(node, reader))
      case node:Query.Filter    => compileM(node.parent, compileFilter(node, reader))
      case node:Query.Join      => compileJoin(node, reader)
      case node:Query.Aggregate => compileAggregate(node, reader)
      case node:Query.Print     => compilePrint(node)

  def compile(ast:Query): () => Unit =
    val start = System.nanoTime
    val program = () => run {
      val query = compileM(ast, compileNoop)
      println(query.show)
      query
    }
    val delta = Duration.fromNanos(System.nanoTime - start)
    println(s"Staging compilation time: ${delta.toMicros} µs")
    program

  def compileOperator(operator:Operator)(using Quotes): Expr[(String, String) => Boolean] =
    operator match
      case Operator.EQ => '{ _ == _ }
      case Operator.NE => '{ _ != _ }

  def compileFilter(filter:Query.Filter, cont:Expr[Reader])(using Quotes):Expr[Reader] = 
    val predicate = compileOperator(filter.predicate.op)
    '{ rec => {
      val lhs = ${Ref.deref(filter.schema, filter.predicate.left)('rec)}
      val rhs = ${Ref.deref(filter.schema, filter.predicate.right)('rec)}

      if(${Expr.betaReduce('{$predicate(lhs, rhs)})})
        ${Expr.betaReduce('{$cont(rec)})}
    }}

  def compileSubset(indices:Array[Int], rec:Expr[Record])(using Quotes):Expr[Record] =
    '{
      val acc = new Array[String](${Expr(indices.length)})
      ${Expr.block(
        indices.toList.zipWithIndex.map((index, i) => 
          '{acc(${Expr(i)}) = ${rec}.fields(${Expr(index)})}
        ), 
        '{Record(acc)}
      )}
    }
  
  def compileProject(project:Query.Project, cont:Expr[Reader])(using Quotes):Expr[Reader] =
    val indices = project.parent.schema.indicesOf(project.to)
    '{ rec => {
      val res = ${compileSubset(indices, 'rec)}
      ${Expr.betaReduce('{$cont(res)})}
    }}

  def compileJoin(join:Query.Join, cont:Expr[Reader])(using Quotes):Expr[Unit] =
    val lhsIndices = join.lhs.schema.indicesOf(join.on)
    val rhsIndices = join.rhs.schema.indicesOf(join.on)
    '{ 
      val acc = Map.empty[Record, Record] 
      
      ${compileM(join.rhs, '{rec => {
        val key = ${compileSubset(rhsIndices, 'rec)}
        acc += new Tuple2(key,rec)
      }})}
      
      ${compileM(join.lhs, '{rec => {
        val key = ${compileSubset(rhsIndices, 'rec)}
        val rhs = acc.apply(key)
        if(rhs != null)
          val res = Record(rec.fields ++ rhs.fields)
          ${Expr.betaReduce('{$cont(res)})}
      }})}
      
    }

  def compileAggregate(agg:Query.Aggregate, cont:Expr[Reader])(using Quotes):Expr[Unit] =
    val indices = agg.parent.schema.indicesOf(agg.by)
    '{ 
      val acc = Map.empty[Array[String], List[Record]] 
      //${compileM(agg.parent, '{rec => acc += Array("yolo") -> rec})}
    }
    
  def compilePrint(print:Query.Print)(using Quotes):Expr[Unit] =
    '{
      val repr = StringBuilder(${Expr(print.parent.schema.toString)} + '\n')
      ${compileM(print.parent, '{rec => repr ++= s"$rec\n"})}
      println(repr.toString)
    }

  def compileNoop(using Quotes):Expr[Reader] = 
    '{_ => ()}
  

