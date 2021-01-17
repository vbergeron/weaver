import scala.quoted.staging._
import scala.quoted._

import model._

object Main {
    
  given Toolbox = Toolbox.make(getClass.getClassLoader)
      

  def main(args: Array[String]): Unit = {
    val base = Schema(Array("a", "b", "c"))
    
    val source = DataSource.CSV("records.csv")

    val query = source
      .query
      .project(Schema(Array("name")))
      .filter(col("name") == lit("Valentin"))
      .print

    val task = Compiler.compile(query)

    task()

  }

}
