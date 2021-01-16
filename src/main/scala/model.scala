import scala.quoted._

object model {
  
  case class Schema(columns: Array[String]) {
    override def toString: String =
      columns.mkString("[", ",", "]")
  }

  case class Record(fields: Array[String])


  enum Reference {
    case Column(name:String)
    case Constant(value:String)
  }

  object Reference {
    def deref(schema:Schema, ref:Reference)(rec:Expr[Record])(using Quotes):Expr[String] = ref match {
      case Column(name)    => '{$rec.fields(${Expr(schema.columns.indexOf(name))})}
      case Constant(value) => Expr(value)
    }
  }

  final case class Predicate(left:Reference, right:Reference)
  
  def compileFilter(schema:Schema, pred:Predicate)(cont:Expr[Record => Unit])(using Quotes):Expr[Record => Unit] = '{
    rec => {
      val lhs:String = ${Reference.deref(schema, pred.left)('rec)}
      val rhs:String = ${Reference.deref(schema, pred.right)('rec)}
      val test = lhs == rhs
      if(test)
        ${Expr.betaReduce('{$cont.apply(rec)})}
    }
  }
  
  def compileProjection(from:Schema, to:Schema)(cont:Expr[Record => Unit])(using Quotes):Expr[Record => Unit] = {
    val indexes = to.columns.map(c => from.columns.indexOf(c))
    '{ rec => {
      val acc = new Array[String](${Expr(indexes.length)})
      ${Expr.block(
        indexes.toList.zipWithIndex.map((index, i) => 
          '{acc(${Expr(i)}) = rec.fields(${Expr(index)})}
        ), 
        '{}
      )}
      val res = Record(acc)
      ${Expr.betaReduce('{$cont.apply(res)})}
    }}
  }
}
  
