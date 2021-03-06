import scala.quoted.staging._
import scala.quoted._

import model._
import Query._

object Main {
    
  given Toolbox = Toolbox.make(getClass.getClassLoader)
      

  def main(args: Array[String]): Unit = {
    val query = DataSource.CSV("records.csv")
      .query
      .filter(col("name") != lit("Valentin"))
      .project(Schema(Array("name")))

    val task = Compiler.compile(Query.Join(query, query, Schema(Array("name"))))

  }

}
