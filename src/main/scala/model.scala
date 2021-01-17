import scala.quoted._

object model {

  type Reader = Record => Unit
  
  case class Schema(columns: Array[String]) {
    override def toString: String =
      columns.mkString("[", ",", "]")
  }

  case class Record(fields: Array[String]) {
    override def toString: String =
      fields.mkString("[", ",", "]")
  }


  enum Reference {
    case Col(name:String)
    case Lit(value:String)

    def ==(that:Reference) =
      Predicate(this, that)
  }

  def col(name:String):Reference = 
    Reference.Col(name)
  
  def lit(value:String):Reference = 
    Reference.Lit(value)

  object Reference {
    def deref(schema:Schema, ref:Reference)(rec:Expr[Record])(using Quotes):Expr[String] = ref match {
      case Col(name)  => '{$rec.fields(${Expr(schema.columns.indexOf(name))})}
      case Lit(value) => Expr(value)
    }
  }

  final case class Predicate(left:Reference, right:Reference)
  
  def compileFilter(schema:Schema, pred:Predicate)(cont:Expr[Reader])(using Quotes):Expr[Reader] = '{
    rec => {
      val lhs:String = ${Reference.deref(schema, pred.left)('rec)}
      val rhs:String = ${Reference.deref(schema, pred.right)('rec)}
      val test = lhs == rhs
      if(test)
        ${Expr.betaReduce('{$cont.apply(rec)})}
    }
  }
  
  def compileProjection(from:Schema, to:Schema)(cont:Expr[Reader])(using Quotes):Expr[Reader] = {
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

  def compilePrint(schema:Schema)(using Quotes):Expr[Reader] =
    '{rec => println(${Expr(schema.toString)} +"," + rec.toString)}

  def noReader(using Quotes):Expr[Reader] = 
    '{_ => ()}

}
  
