import scala.quoted.staging._
import scala.quoted._

import model._

object Main {
    
  given Toolbox = Toolbox.make(getClass.getClassLoader)
      

  def main(args: Array[String]): Unit = {
    val base = Schema(Array("a", "b", "c"))
    val proj = Schema(Array("c", "b"))

    val fullFilter = run {
      val projection = compileProjection(base, proj)('{println})
      val filter = compileFilter(base, Predicate(Reference.Column("a"), Reference.Constant("b")))(projection)
      println(filter.show)
      filter
    }
    
    //val fullProjection = run {
    //  val projection = model.compileFullProjection(base, proj)('{println})
    //  println(projection.show)
    //  projection
    //}

  }

}
