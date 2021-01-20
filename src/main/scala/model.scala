import scala.quoted._

object model:

  type Reader = Record => Unit
  
  case class Schema(columns: Array[String]):
    override def toString: String =
      columns.mkString("[", ",", "]")

  case class Record(fields: Array[String]):
    override def toString: String =
      fields.mkString("[", ",", "]")

  enum Ref:
    case Col(name:String)
    case Lit(value:String)

    def ==(that:Ref) =
      Predicate(this, that, Operator.EQ)
    
    def !=(that:Ref) =
      Predicate(this, that, Operator.NE)

  def col(name:String):Ref = 
    Ref.Col(name)
  
  def lit(value:String):Ref = 
    Ref.Lit(value)

  object Ref:
    def deref(schema:Schema, ref:Ref)(rec:Expr[Record])(using Quotes):Expr[String] = 
      ref match 
        case Col(name)  => '{$rec.fields(${Expr(schema.columns.indexOf(name))})}
        case Lit(value) => Expr(value)

  final case class Predicate(left:Ref, right:Ref, op:Operator)
  

