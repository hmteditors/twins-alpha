import edu.holycross.shot.mid.validator._
import edu.holycross.shot.cite._
import edu.holycross.shot.ohco2._
import edu.holycross.shot.cex._
//import scala.io.Source
import java.io.PrintWriter

import org.homermultitext.edmodel._
import edu.holycross.shot.greek._

import edu.holycross.shot.citebinaryimage._

import better.files._
import File._
import java.io.{File => JFile}
import better.files.Dsl._

import scala.xml._

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
  val csvRows = scala.io.Source.fromFile(csvSource).getLines.toVector.tail
  val pairs = for (row <- csvRows) yield {
    val parts = row.split(",").toVector
    ReadersPairing(CtsUrn(parts(0)), readersForString(parts(1)))
  }
  pairs.toVector
}

def orthoMappings(csvSource : String = "editions/orthographies.csv") = {
  val csvRows = scala.io.Source.fromFile(csvSource).getLines.toVector.tail
  val pairs = for (row <- csvRows) yield {
    val parts = row.split(",").toVector
    OrthoPairing(CtsUrn(parts(0)), orthoForString(parts(1)))
  }
  pairs.toVector
}


val repo = EditorsRepo(".")
val midValidator = Validator(repo, readerMappings(), orthoMappings())
val dse = midValidator.dse
val reporter = ValidationReporter(midValidator)

def imagePathForMSCollection(msColl: String): String = {
  msColl match {
    case "msA" => "/project/homer/pyramidal/deepzoom/hmt/vaimg/2017a/"
    case "msB" => "/project/homer/pyramidal/deepzoom/hmt/vbbifolio/v1/"
    case "e3" => "/project/homer/pyramidal/deepzoom/hmt/e3bifolio/v1/"
  }
}



// Map of Perosnal names URNs to string labels
def namesAuthority :  Map[Cite2Urn, String]= {
  val lines = scala.io.Source.fromURL("https://raw.githubusercontent.com/homermultitext/hmt-authlists/master/data/hmtnames.cex").getLines.toVector.drop(2)

  val auths = for (ln <- lines) yield {
    val cols = ln.split("#")
    (Cite2Urn(cols(0)) -> cols(3))
  }
  auths.toMap
}


// Map of place name URNs to string labels
def placesAuthority :  Map[Cite2Urn, String]= {
  val lines = scala.io.Source.fromURL("https://raw.githubusercontent.com/homermultitext/hmt-authlists/master/data/hmtplaces.cex").getLines.toVector.drop(2)

  val auths = for (ln <- lines) yield {
    val cols = ln.split("#")
    (Cite2Urn(cols(0)) -> cols(1))
  }
  auths.toMap
}


// Write report for named entity validation.
def validatePNs(uString: String) = {
  val pg = Cite2Urn(uString)

  val corpus = reporter.corpusForPage(pg)
  val rept = StringBuilder.newBuilder
  rept.append("# Named entity verification for people: " + pg + "\n\n")
  val allPeople = for (nd <- corpus.nodes) yield {
    val n = XML.loadString(nd.text)
    val settings = TokenSettings(nd.urn, LexicalToken)
    val tokens = TeiReader.collectTokens(n, settings)
    val people = tokens.filter(_.lexicalDisambiguation ~~ Cite2Urn("urn:cite2:hmt:pers:"))
    people
  }
  val peopleList = allPeople.flatten
  println(peopleList.size + " Name tokens ")
  val persAuth = namesAuthority
  val persUrns = peopleList.map(_.lexicalDisambiguation)

  val ok = persUrns.filter(persUrns.contains(_))
  if (ok.size != persUrns.size) {
    rept.append("## Errors\n\n")
    rept.append("There were errors in personal name identifiers.\n\n")
    val badList = persUrns.filterNot(persUrns.contains(_))
    for (bad <- badList) {
      rept.append("-  " + bad + " not found in authority list.\n")
    }
    rept.append("\n")

  } else {
    rept.append("All personal name identifiers were found in authority list.\n\n")
  }

  rept.append("## Verification\n\n")
  for (u <- persUrns.distinct) {
    rept.append(s"### ${persAuth(u)} (*${u.objectComponent}*) \n\n")
    val matches = peopleList.filter(_.lexicalDisambiguation == u)
    for (tkn <- matches) {
      rept.append("-  " + tkn.editionUrn + " " + tkn.leidenDiplomatic + "\n")
    }
    rept.append("\n\n")
  }

  val baseDir = File(s"validation/${pg.collection}-${pg.objectComponent}")

  val reptName = "personal-names.md"
  val outFile = baseDir/reptName
  outFile.overwrite(rept.toString)
}

// Write report for validation of place names.
def validatePlaces(uString: String) = {
  val pg = Cite2Urn(uString)
  val rept = StringBuilder.newBuilder
  rept.append("# Named entity verification for places: " + pg + "\n\n")

  val corpus = reporter.corpusForPage(pg)
  val allPlaces = for (nd <- corpus.nodes) yield {
    val n = XML.loadString(nd.text)
    val settings = TokenSettings(nd.urn, LexicalToken)
    val tokens = TeiReader.collectTokens(n, settings)
    val places = tokens.filter(_.lexicalDisambiguation ~~ Cite2Urn("urn:cite2:hmt:place:"))
    places
  }
  val placeList = allPlaces.flatten
  val placeUrns = placeList.map(_.lexicalDisambiguation)
  println(placeList.size + " Place tokens ")
  val placeAuth = placesAuthority
  //val placeUrns = placeList.map(_.lexicalDisambiguation)

  val ok = placeUrns.filter(placeUrns.contains(_))
  if (ok.size != placeUrns.size) {
    rept.append("## Errors\n\n")
    rept.append("There were errors in place name identifiers.\n\n")
    val badList = placeUrns.filterNot(placeUrns.contains(_))
    for (bad <- badList) {
      rept.append("-  " + bad + " not found in authority list.\n")
    }
    rept.append("\n")

  } else {
    rept.append("All place name identifiers were found in authority list.\n\n")
  }

  rept.append("## Verification\n\n")
  for (u <- placeUrns.distinct) {
    //println(s"${u} ${persAuth(u)}")
    rept.append(s"### ${placeAuth(u)} (*${u.objectComponent}*) \n\n")
    val matches = placeList.filter(_.lexicalDisambiguation == u)
    for (tkn <- matches) {
      rept.append("-  " + tkn.editionUrn + " " + tkn.leidenDiplomatic + "\n")
      //println("\t" + tkn.editionUrn + " " + tkn.leidenDiplomatic)
    }
    rept.append("\n\n")
  }

  val baseDir = File(s"validation/${pg.collection}-${pg.objectComponent}")
  //val fName = e3urn.collection + "-" + e3urn.objectComponent + "-" + msBurn.collection+ "-" + msBurn.objectComponent + ".md"
  val reptName = "place-names.md"
  val outFile = baseDir/reptName
  outFile.overwrite(rept.toString)
}


// Validate titles
// validate urns in  scholia references




//////////// BEGIN FUNCTIONS SPECIFIC TO TWINS ////////////////////////////////

def scholionMarkers(uString: String) = {
  val baseUrl = "http://www.homermultitext.org/iipsrv?"

  val pg = Cite2Urn(uString)
  val txts = dse.textsForTbs(pg)
  //println("TEXTS:")
  //println(txts.mkString("\n"))
  //println("\n\n")
  val lines = DataCollector.compositeFiles("scholia-markers", "cex", 1).split("\n").filter(_.nonEmpty).filterNot(_.contains("reading#image#scholion#"))

  val mdLines = for (ln <- lines) yield {
    //reading#image#scholion#linked text

    val cols = ln.split("#")
    val scholion = CtsUrn(cols(2))
    //println("LOOK SCHOLION " + scholion)

    if (!txts.contains(scholion)) {
      ""
    } else {

      val img = Cite2Urn(cols(1))
      val imagePath = imagePathForMSCollection(pg.collection)
      val bis = IIIFApi(baseUrl,imagePath)

      val reading = cols(0)


      val txt = cols(3)
      "| " +  bis.linkedHtmlImage(img) + s" | ${reading} | ${scholion} | ${txt} |"
    }
  }

  val hdr = "| Image | Reading | Attached to scholion | Comments on |\n|:-----------|:-----------|:-----------|:-----------|\n"

  val md = hdr + mdLines.filter(_.nonEmpty).mkString("\n")

  val baseDir = File(s"validation/${pg.collection}-${pg.objectComponent}")
  val reptName = "scholion-markers.md"
  val outFile = baseDir/reptName
  outFile.overwrite(md)
}

// Map of scholia to Iliad lines
def indexComments = {
  val scholia = TextRepositorySource.fromFiles("editions/catalog-scholia-only.cex", "editions/citation-scholia-full.cex", "editions/scholia").corpus
  val xreff = scholia.nodes.filter(_.urn.passageComponent.contains("ref"))

  val urnMap = for(ref <- xreff) yield {
    val scholion = ref.urn.collapsePassageBy(1)
    try {
      val iliadString = XML.loadString(ref.text).text.trim
      val iliad = CtsUrn(iliadString)
      Some(scholion -> iliad.dropVersion)
    } catch {
      case t : Throwable => {
        println("FAILED ON REF " + ref)
        println("SCHOLION WAS " + scholion)
        println("Iliad was " + XML.loadString(ref.text).text.trim)
        None
      }
    }
  }
  urnMap.flatten.toMap
}

// Map of Iliad lines to pages of VA MS.
def vaIliadIndex = {
  val vaLines = scala.io.Source.fromFile("scripts/va-il-lines-idx.txt").getLines.toVector
  val vaIdx = for (ln <- vaLines) yield {
    val cols = ln.split("#")
    CtsUrn(cols(0)).dropVersion -> Cite2Urn(cols(1))
  }
  vaIdx.toMap
}


// Markdown linking to facsimile edition of Venetus A
def vaPageMd(pg: Cite2Urn) : String = {
  val baseUrl = "https://homermultitext.github.io/facsimiles/venetus-a/"
  val link = s"${baseUrl}${pg.objectComponent}/"
  s"[${pg.objectComponent}](${link})"
}


// Markdown for cross reference in twinsScholia output to Venetus A
def xrefMd(
  urns: Vector[String],
  lnsIndex : Map[CtsUrn,CtsUrn],
  pgIndex: Map[CtsUrn,Cite2Urn]) :  String = {

  val iliadLines = urns.size match {
      case 1 => {

        try {
          val u = CtsUrn(urns(0))
          "Commenting on *Iliad*" + lnsIndex(u).passageComponent

        } catch {
          case t: Throwable => {

            "Commenting on *Iliad*" + urns(0)
          }
        }

      }
      case 2 => {
        val lines = Set(lnsIndex(CtsUrn(urns(0))), lnsIndex(CtsUrn(urns(1))))
        lines.size match {
          case 1 => {
            val scholUrn = CtsUrn(urns(0))
            val ilUrn = lnsIndex(scholUrn)
            val vaPage = try {
              val pg = pgIndex(ilUrn)
              s" (see Venetus A, page ${vaPageMd(pg)})"
            } catch {
              case t: Throwable =>   ""
            }
            "Commenting on *Iliad*" + ilUrn.passageComponent + vaPage
          }

          case 2 => {
            "Commenting on *Iliad* " + lines.toVector.mkString(" and ")
          }
        }
      }
    }

    iliadLines.toString
}

// Unspeakable kludge of a script, but the output is pretty.
def twinScholia(e3String: String, msBString: String) = {
  println("==>Making comparison of two pages...")

  val linesIndex = indexComments
  val vaPageIndex = vaIliadIndex

  val msBurn = Cite2Urn(msBString)
  val msBcorpus = reporter.corpusForPage(msBurn)
  val msBurns = msBcorpus.nodes.map(_.urn)



  val e3urn = Cite2Urn(e3String)
  val e3corpus = reporter.corpusForPage(e3urn)
  val e3urns = e3corpus.nodes.map(_.urn)

  val e3DseReporter =  DseReporter(e3urn, dse, e3corpus, midValidator.readers)
  val msBDseReporter =  DseReporter(msBurn, dse, reporter.corpusForPage(msBurn), midValidator.readers)

  val baseDir = File("validation")
  val fName = e3urn.collection + "-" + e3urn.objectComponent + "-" + msBurn.collection+ "-" + msBurn.objectComponent + ".md"
  val outFile = baseDir/fName



  val pairings=  DataCollector.compositeFiles("relations", "cex", 1).split("\n").filter(_.nonEmpty)


  val rows = for (pr <- pairings) yield {
    val urns = pr.split("#").toVector

    val e3Str = if (urns(0).isEmpty) {
        ""
      } else {

        val scholion = CtsUrn(urns(0))
        if (e3urns.contains(scholion)) {
          e3DseReporter.passageMarkdown(scholion)
        } else {
          ""
        }

      }
    val msB = if (urns.size == 1) {
      ""
    } else {

      val scholion = CtsUrn(urns(1))
      if (msBurns.contains(scholion)) {
        msBDseReporter.passageMarkdown(scholion)
      } else {
        ""
      }
    }



    if ((msB + e3Str).isEmpty) {
      ""
    } else {

      val seeAlso =  xrefMd(urns,linesIndex, vaPageIndex )


      "| " + e3Str + " | " + msB + " | " + seeAlso + " |"
    }
  }
  val hdr = "| Upsilon 1.1 | Venetus B | See also |\n|:-----------|:-----------|:-----------|\n"
  val md = hdr + rows.filter(_.nonEmpty).mkString("\n")

  outFile.overwrite(md)
}
//////////// END FUNCTIONS SPECIFIC TO TWINS ////////////////////////////////

def validate(uString : String) = {
  reporter.validate(uString)
  validatePNs(uString)
  validatePlaces(uString)
  scholionMarkers(uString)
}


def validatePair(e3String : String, msBString: String) = {
  reporter.validate(e3String)
  reporter.validate(msBString)
  twinScholia(e3String,msBString)
}


println("\n\nValidate editorial work for a given page:")
println("\n\tvalidate(\"PAGEURN\")\n\n")
println("\n\nValidate editorial work for a related pair of pages\nin Upsilon 1.1 and Venetus B:")
println("\n\tvalidatePair(\"UPSILON-URN\", \"VB-URN\")\n\n")
