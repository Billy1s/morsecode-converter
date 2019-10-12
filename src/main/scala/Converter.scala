import scala.annotation.tailrec

object Converter {
  val SPACE = " "
  def run(morseToEnglish: Boolean, textToTranslate: String): String = {
  val translated = if (morseToEnglish) convertToMorse(textToTranslate.toLowerCase())
    else if (!morseToEnglish) convertToEnglish(textToTranslate.toLowerCase())
    translated.toString.capitalize
  }

  def MorseToenglish(morsecode: String): String = {
   val english = morsecode match {
     case ".-"     => "a"
     case "-..."   => "b"
     case "-.-."   => "c"
     case "-.."    => "d"
     case "."      => "e"
     case "..-."   => "f"
     case "--."    => "g"
     case "...."   => "h"
     case ".."     => "i"
     case ".---"   => "j"
     case "-.-"    => "k"
     case ".-.."   => "l"
     case "--"     => "m"
     case "-."     => "n"
     case "---"    => "o"
     case ".--."   => "p"
     case "--.-"   => "q"
     case ".-."    => "r"
     case "..."    => "s"
     case "-"      => "t"
     case "..-"    => "u"
     case "...-"   => "v"
     case ".--"    => "w"
     case "-..-"   => "x"
     case "-.--"   => "y"
     case "--.."   => "z"
     case ".----"  => "1"
     case "..---"  => "2"
     case "...--"  => "3"
     case "....-"  => "4"
     case "....."  => "5"
     case "-...."  => "6"
     case "--..."  => "7"
     case "---.."  => "8"
     case "----."  => "9"
     case "-----"  => "0"
     case ".-.-.-" => "."
     case "|"    => " "
     //case "" => " "
     case _ => "error invalid input character"
   }
    english
  }

  def convertToEnglish(inputmorse: String): String = {
    val input_split = inputmorse.replace("   "," | ").split("[ ]")
    @tailrec
    def converthelper(words: Array[String], morse: String = ""): String = {
      if (words.isEmpty) morse
      else converthelper(words.tail, morse +  MorseToenglish(words.head))
    }
    converthelper(input_split)
  }

  def EnglishToMorse(english: Char): String = {
    val morsecode = english match {
      case 'a' => ".-"     + SPACE
      case 'b' => "-..."   + SPACE
      case 'c' => "-.-."   + SPACE
      case 'd' => "-.."    + SPACE
      case 'e' => "."      + SPACE
      case 'f' => "..-."   + SPACE
      case 'g' => "--."    + SPACE
      case 'h' => "...."   + SPACE
      case 'i' => ".."     + SPACE
      case 'j' => ".---"   + SPACE
      case 'k' => "-.-"    + SPACE
      case 'l' => ".-.."   + SPACE
      case 'm' => "--"     + SPACE
      case 'n' => "-."     + SPACE
      case 'o' => "---"    + SPACE
      case 'p' => ".--."   + SPACE
      case 'q' => "--.-"   + SPACE
      case 'r' => ".-."    + SPACE
      case 's' => "..."    + SPACE
      case 't' => "-"      + SPACE
      case 'u' => "..-"    + SPACE
      case 'v' => "...-"   + SPACE
      case 'w' => ".--"    + SPACE
      case 'x' => "-..-"   + SPACE
      case 'y' => "-.--"   + SPACE
      case 'z' => "--.."   + SPACE
      case '1' => ".----"  + SPACE
      case '2' => "..---"  + SPACE
      case '3' => "...--"  + SPACE
      case '4' => "....-"  + SPACE
      case '5' => "....."  + SPACE
      case '6' => "-...."  + SPACE
      case '7' => "--..."  + SPACE
      case '8' => "---.."  + SPACE
      case '9' => "----."  + SPACE
      case '0' => "-----"  + SPACE
      case '.' => ".-.-.-"
      case ' ' => "  "
      case _ => "error invalid input character"
    }
    morsecode
  }

  def convertToMorse(inputword: String): String = {
    @tailrec
    def converthelper(word: String, morse: String = ""): String = {
      if (word.isEmpty) morse
      else converthelper(word.tail, morse +  EnglishToMorse(word.head))
    }
    converthelper(inputword)
  }

}