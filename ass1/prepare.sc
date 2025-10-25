import scala.annotation.tailrec
import scala.util.Success
import scala.util.Failure
import java.io.{File, FilenameFilter, PrintWriter}
import scala.io.Source
import scala.util.Using

object Preparer:
  val I = """/\* BEGIN \*/(.|\n)*?/\* END \*/""".r
  val R = """/\* BEGIN REMOVAL \*/(.|\n)*?/\* END REMOVAL \*/""".r
  val isScalaFile: FilenameFilter = (_, name) => name.endsWith(".scala")

  def main(args: Array[String]): Unit =
    new File("./src/main/scala/ass1").listFiles(isScalaFile).foreach(prepare)
    new File("./src/test/scala/ass1").listFiles(isScalaFile).foreach(prepare)
  
  def prepare(file: File): Unit =
    Using(Source.fromFile(file)): source =>
      val content = remove(I.replaceAllIn(source.getLines().mkString("\n"), "???"), 0, "")
      Using(new PrintWriter(file)): writer =>
        writer.write(content)
        if !content.endsWith("\n") then writer.write("\n")
      match
        case Failure(error) => 
          println(s"Error writing to file ${file.getName}: $error")
        case Success(_) =>
          println(s"Successfully processed file: ${file.getName}")
    .getOrElse:
      println(s"Error reading file: ${file.getName}")

  val BEGIN_MARK = "/* BEGIN REMOVAL */"
  val END_MARK = "/* END REMOVAL */"

  @tailrec def remove(source: String, at: Int, acc: String): String =
    if at >= source.length then acc else
      val begin = source.indexOf(BEGIN_MARK, at)
      if begin == -1 then acc + source.substring(at)
      else
        val end = source.indexOf(END_MARK, begin + BEGIN_MARK.length)
        if end == -1 then acc + source.substring(at)
        else remove(source, end + END_MARK.length, acc + source.substring(at, begin))
end Preparer
