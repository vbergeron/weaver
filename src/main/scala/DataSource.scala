import model.{Record, Schema}

import scala.io.Source
import scala.quoted.Expr
import scala.quoted.staging._

sealed trait DataSource {
  def readRecords(reader: Schema => Expr[Record => Unit]): Unit
}

object DataSource {

  given Toolbox = Toolbox.make(getClass.getClassLoader)

  def compileReader(schema:Schema, reader: Schema => Expr[Record => Unit]):Record => Unit =
    run {
      val stagedReader: Expr[Record => Unit] = reader(schema)
      println(stagedReader.show)
      stagedReader
    }


  case class CSV(name: String) extends DataSource {
    def readRecords(reader: Schema => Expr[Record => Unit]): Unit = {
      //val source = Source.fromFile(name)
      //val lines = source.getLines()
      //val schema = Schema(lines.next().split(","))

      //val compiledReader = compileReader(schema,reader)

      ////lines.foreach(line => compiledReader.apply(Record(schema, line.split(","))))
      //source.close()
    }
  }

}
