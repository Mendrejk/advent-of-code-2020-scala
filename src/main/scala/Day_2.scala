import IoUtilities.readLinesToListString

case class Password
  (occurrencesMin: Int, occurrencesMax: Int, symbol: Char, password: String)

object Day_2 {
  def main(args: Array[String]): Unit = {
    val directory: String = "resources/DataDay_2.txt"
    val data: Option[List[String]] = readLinesToListString(directory)

    val validatedCount: Option[Int] = data match {
      case Some(passwordStrings: List[String]) =>
        Some(passwordStrings.count { (passwordString: String) =>
          parsePassword(parseToList(passwordString)) match {
            case Some(pass: Password) =>
              validatePassword(pass)
            case None => false
          }
        })
      case None => None
    }

    validatedCount match {
      case Some(count: Int) => print(validatedCount)
      case None =>
        println("Data reading error... Please check the given directory.")
    }
  }

  def parseToList(passwordString: String): List[String] =
    passwordString.strip()
      // splits by either "-", " ", or ":"
      .split("[- :]")
      .filter(_.nonEmpty)
      .toList

  def parsePassword(passwordAsList: List[String]): Option[Password] = {
    val toCharOption: String => Option[Char] = charCandidate =>
      if (charCandidate.length == 1) Some(charCandidate.head) else None

    passwordAsList match {
      case min :: max :: symbol :: password :: Nil =>
        val minOption: Option[Int] = min.toIntOption
        val maxOption: Option[Int] = max.toIntOption
        val symbolOption: Option[Char] = toCharOption(symbol)
        (minOption, maxOption, symbolOption) match {
          case (Some(minInt), Some(maxInt), Some(symbolChar)) =>
            Some(Password(minInt, maxInt, symbolChar, password))
          case _ => None
        }
      case _ => None
    }
  }

  def validatePassword(password: Password): Boolean = {
    password.occurrencesMin to password.occurrencesMax contains
    password.password.count((char: Char) => char == password.symbol)
  }

}


