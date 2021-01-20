import model.{Record, Schema}

import scala.io.Source
import scala.quoted.Expr
import scala.quoted.staging._

import model._
import scala.quoted._

sealed trait DataSource:
  def readSchema:Schema
  def readRecords(reader:Expr[Reader])(using Quotes): Expr[Unit]

  def query:Query.Scan = Query.Scan(this)


object DataSource:

  case class CSV(name: String) extends DataSource:

    def readSchema: Schema =
      val source = Source.fromFile(name)
      val lines = source.getLines()
      val schema = Schema(lines.next().split(","))
      source.close()
      schema
    
    def readRecordsRaw(reader:Reader): Unit =
      val source = Source.fromFile(${Expr(name)})
      val lines = source.getLines()
      lines.next() // skip header
      lines.foreach(line => reader(Record(line.split(","))))
      source.close()
  
  
    def readRecords(reader:Expr[Reader])(using Quotes): Expr[Unit] = '{
      val rowReader = $reader
      val source = Source.fromFile(${Expr(name)})
      val lines = source.getLines()
      lines.next() // skip header
      lines.foreach(line => rowReader(Record(line.split(","))))
      source.close()
    }