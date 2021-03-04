import IoUtilities.readLinesToListString

case class Password
  (occurrencesMin: Int, occurrencesMax: Int, symbol: Char, pass: String)

object Day_2 {
  def main(args: Array[String]): Unit = {
    val directory: String = "resources/DataDay_2.txt"
    val data: Option[List[String]] = readLinesToListString(directory)
    val passwords: Option[List[Password]] = data match {
      case Some(passStrings: List[String]) => Some(parsePasswords(passStrings))
      case None => None
    }

    val validatedByOccurrenceCount: Option[Int] = passwords match {
      case Some(passwords: List[Password]) =>
        Some(countValidPasswordsByOccurrence(passwords))
      case None => None
    }

    val validatedByPositionCount: Option[Int] = passwords match {
      case Some(passwords: List[Password]) =>
        Some(countValidPasswordsByPosition(passwords))
      case None => None
    }

    (validatedByOccurrenceCount, validatedByPositionCount) match {
      case (Some(countByOccurrences: Int), Some(countByPosition: Int))
        => print(s"by occurences: $countByOccurrences" +
                 s", by position: $countByPosition")
      case _ =>
        println("Data reading error... Please check the given directory.")
    }
  }

  def countValidPasswordsByOccurrence(passwords: List[Password]): Int = {
    countValidPasswordsBy(passwords) { (pass: Password) =>
      validatePasswordByOccurrences(pass)
    }
  }

  def countValidPasswordsByPosition(passwords: List[Password]): Int = {
    countValidPasswordsBy(passwords) { (pass: Password) =>
      validatePasswordByPosition(pass)
    }
  }

  private def countValidPasswordsBy
    (passwords: List[Password]) (predicate: Password => Boolean): Int = {
      passwords.count { (pass: Password) => predicate(pass) }
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

  def parsePasswords(passwordStrings: List[String]): List[Password] = {
    passwordStrings
      // flatten discards "None" Passwords
      .flatMap((passString: String) => parsePassword(parseToList(passString)))
  }

  def validatePasswordByOccurrences(password: Password): Boolean = {
    password.occurrencesMin to password.occurrencesMax contains
    password.pass.count((char: Char) => char == password.symbol)
  }

  def validatePasswordByPosition(password: Password): Boolean = {
    password match {
      case Password(low, high, char, pass) =>
        // XOR operator
        // counting from 1 rather than 0
        pass.charAt(low - 1) == char ^ pass.charAt(high - 1) == char
        // same as
//        val lowTrue: Boolean = pass.charAt(low - 1) == char
//        val highTrue: Boolean = pass.charAt(high - 1) == char
//        (lowTrue && !highTrue) || (highTrue && !lowTrue)
      case _ => false
    }
  }
}


