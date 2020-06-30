// Mandatory Ongoing Maintenance (HMT MOM)
import edu.holycross.shot.mid.validator._
import edu.holycross.shot.citevalidator._
import edu.holycross.shot.mid.markupreader._
import edu.holycross.shot.cite._
import edu.holycross.shot.ohco2._
import edu.holycross.shot.scm._
import edu.holycross.shot.dse._
import org.homermultitext.edmodel._
import edu.holycross.shot.greek._
import java.io.File
import java.io.PrintWriter


def readersMap : Map[String, Vector[MidMarkupReader]] = Map(
  "DiplomaticReader" ->   Vector(DiplomaticReader)
)

// Create a library object from a repository with
// directories organized according to MID conventions.
def loadLibrary: CiteLibrary =  {
  val lib = EditorsRepo(".", readersMap).library
  val newCorpus = Corpus(lib.textRepository.get.corpus.nodes.map(cn => CitableNode(cn.urn,cn.text.replaceAll("~",":"))))
  val newTextRepo = TextRepository(newCorpus, lib.textRepository.get.catalog)

  CiteLibrary(
    lib.name,
    lib.urn,
    lib.license,
    lib.namespaces,
    Some(newTextRepo),
    lib.collectionRepository,
    lib.relationSet,
    lib.dataModels
  )
}

lazy val corpus = loadLibrary.textRepository.get.corpus

def reportsDir(pgUrn: Cite2Urn): File = {
  val f = new File(s"validation/${pgUrn.collection}-${pgUrn.objectComponent}")
  if (! f.exists()) { f.mkdir()}
  f
}


def indexPage(pgUrn: Cite2Urn, validators: Vector[CiteValidator[Any]], results: Vector[TestResult[Any]]): String = {
  "# Validation results\n\n" +
  s"Results for page **${pgUrn.objectComponent}** (`${pgUrn}`)\n\n" +
  "Applied the following validators:\n\n" +
  validators.map(v => "- " + v.label).mkString("\n") + "\n\n" +
  s"Totals (success/total): **${results.filter(_.success).size} / ${results.size}**\n\nFor details, see validation and verification reports for each validator."

}

// Create new library including only texts matchng a givn URN
def libForTexts(lib: CiteLibrary, ctsUrn: CtsUrn) : CiteLibrary = {
  val newTextRepo = lib.textRepository.get ~~ ctsUrn
  CiteLibrary(
    lib.name,
    lib.urn,
    lib.license,
    lib.namespaces,
    Some(newTextRepo),
    lib.collectionRepository,
    lib.relationSet,
    lib.dataModels
  )
}

// Create new corpus with scholia omitting ref units
def scholia(c: Corpus = corpus) : Corpus = {
  val scholCorpus = c ~~  CtsUrn("urn:cts:greekLit:tlg5026:")
  val noReff = scholCorpus.nodes.filterNot(n => n.urn.passageComponent.endsWith("ref"))
  Corpus(noReff)
}
// Validate one page of editorial work.
def validate(page: String, lib: CiteLibrary = loadLibrary) : Vector[TestResult[Any]] = {
  val pgUrn = Cite2Urn(page)
  val dir = reportsDir(pgUrn)
  //val lib = loadLibrary
  val iliadLib = libForTexts(lib, CtsUrn("urn:cts:greekLit:tlg0012.tlg001:"))


  val dseValidator = DseValidator(lib)
  val litGreekValidator = LGSValidator(iliadLib)
  val allValidators = Vector(dseValidator, litGreekValidator)
  val total = LibraryValidator.validate(pgUrn, allValidators)
  new PrintWriter(s"${dir}/index.md"){write(indexPage(pgUrn, allValidators, total)); close;}

  val litGreekResults = TestResultGroup(
    s"Validation of LiteryGreekStrings for ${pgUrn.collection}, page ${pgUrn.objectComponent}",
    LibraryValidator.validate(pgUrn,Vector(litGreekValidator)))
  new PrintWriter(s"${dir}/litgreek-validation.md"){write(litGreekResults.markdown); close;}
  val litGreekVerify = litGreekValidator.verify(pgUrn)
  new PrintWriter(s"${dir}/litgreek-verification.md"){write(litGreekVerify); close;}


  val dseResults = TestResultGroup(
    s"DSE validation for ${pgUrn.collection}, page ${pgUrn.objectComponent}",
    LibraryValidator.validate(pgUrn,Vector(dseValidator)))
  new PrintWriter(s"${dir}/dse-validation.md"){write(dseResults.markdown); close;}
  val dseVerify = dseValidator.verify(pgUrn)
  new PrintWriter(s"${dir}/dse-verification.md"){write(dseVerify); close;}
  total
}

// recursively collect
def sumResults(src: Vector[Vector[TestResult[Any]]], total: Vector[TestResult[Any]] = Vector.empty[TestResult[Any]]) : TestResultGroup = {
  if (src.isEmpty) {
    TestResultGroup("Results of validating all pages in repository", total)
  } else {
    sumResults(src.tail, total ++ src.head)
  }
}

def validateAll : Unit = {
  val lib = loadLibrary
  val dsev = DseVector.fromCiteLibrary(lib)
  val surfaces = dsev.tbs.toVector
  println("Validating " + surfaces.size + " pages.")
  val rslts = for (pg <- surfaces) yield {
    validate(pg.toString, lib)
  }

  val summed = sumResults(rslts)
  val summary = "# Validation results\n\n" +
    s"Results for **${surfaces.size}** pages\n\n" +
    s"Totals (success/total): **${summed.results.filter(_.success).size} / ${summed.results.size}**\n\nFor details, see validation and verification reports for each page."
  new PrintWriter("validation/overview.md"){write(summary);close;}
  summed.write("validation/default-summary.md")
}


// Tell them how to use the script.
def usage: Unit = {
  println("\n\nTo validate a page:\n\tvalidate(\"PAGE_URN\")\n")
  println("Example:\n\tvalidate(\"urn:cite2:hmt:msB.v1:303v\")\n")
  println("Results will be written as markdown files in the `validation` directory.")
  println("\nTo test if you can load a valid library from your repository:")
  println("\tloadLibrary\n")
  println("You should run `loadLibary` before pushing to github.")
}

usage
