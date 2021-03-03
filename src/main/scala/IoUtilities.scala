import java.io.FileNotFoundException

import scala.io.{BufferedSource, Source}

  object IoUtilities {
    def readLinesToListInt(directory: String): Option[List[Int]] = {
      readLines(directory) {
        (iterator: Iterator[String]) =>
          iterator.map((number: String) => number.toInt).toList
      }
    }

    private def readLines[A]
      (directory: String) (transform: Iterator[String] => A): Option[A] = {
        try {
          val source: BufferedSource = Source.fromFile(directory)
          try {
            Some(transform(source.getLines()))
          } catch {
            case _: FileNotFoundException => None
            case e =>
              println(e)
              None
          }
        } catch {
          case _: FileNotFoundException => None
          case e =>
            println(e)
            None
        }
      }

}
