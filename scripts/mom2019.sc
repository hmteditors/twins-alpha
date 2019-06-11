import edu.holycross.shot.mid.validator._
import edu.holycross.shot.cite._
import edu.holycross.shot.ohco2._
import scala.io.Source
import java.io.PrintWriter

import org.homermultitext.edmodel._
import edu.holycross.shot.greek._

val readerMap =   Map(
  "DiplomaticReader" ->   Vector(DiplomaticReader)
)

val orthoMap = Map(
  "LiteraryGreekString" -> LiteraryGreekString
)

def readersForString(readerName: String): Vector[MidMarkupReader] = {
  if (readerMap.keySet.contains(readerName)){
    readerMap(readerName)
  } else {
    throw (new Exception(s"${readerName} is not a recognized MidReader in this project."))
  }
}

def orthoForString(orthoName: String): MidOrthography = {
  if (orthoMap.keySet.contains(orthoName)){
    orthoMap(orthoName)
  } else {
    throw (new Exception(s"${orthoName} is not a recognized Orthography in this project."))
  }
}


def readerMappings(csvSource : String = "editions/readers.csv") = {
  // drop header row:
  val csvRows = Source.fromFile(csvSource).getLines.toVector.tail
  val pairs = for (row <- csvRows) yield {
    val parts = row.split(",").toVector
    ReadersPairing(CtsUrn(parts(0)), readersForString(parts(1)))
  }
  pairs.toVector
}

def orthoMappings(csvSource : String = "editions/orthographies.csv") = {
  val csvRows = Source.fromFile(csvSource).getLines.toVector.tail
  val pairs = for (row <- csvRows) yield {
    val parts = row.split(",").toVector
    OrthoPairing(CtsUrn(parts(0)), orthoForString(parts(1)))
  }
  pairs.toVector
}


val repo = EditorsRepo(".")
val midValidator = Validator(repo, readerMappings(), orthoMappings())
val reporter = ValidationReporter(midValidator)



def validate(uString : String) = {
  reporter.validate(uString)
}



println("\n\nValidate editorial work for a given page:")
println("\n\tvalidate(\"PAGEURN\")\n\n")
