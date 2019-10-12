import org.scalatest.{FlatSpec, Matchers}

class ConverterSpec extends FlatSpec with Matchers {

  "Run function: english to morse" should "return the correct output" in {
  	val s = Converter

  	s.run(morseToEnglish = true, "The wizard quickly jinxed the gnomes before they vaporized."
    ) should be ("- .... .   .-- .. --.. .- .-. -..   --.- ..- .. -.-. -.- .-.. -.--   .--- .. -. -..- . -..   - .... .   --. -. --- -- . ...   -... . ..-. --- .-. .   - .... . -.--   ...- .- .--. --- .-. .. --.. . -.. .-.-.-")
  }

  "Run function : morse to english" should "return the correct output" in {
    val s = Converter

    s.run(morseToEnglish = false,"- .... .   .-- .. --.. .- .-. -..   --.- ..- .. -.-. -.- .-.. -.--   .--- .. -. -..- . -..   - .... .   --. -. --- -- . ...   -... . ..-. --- .-. .   - .... . -.--   ...- .- .--. --- .-. .. --.. . -.. .-.-.-"
    ) should be ("The wizard quickly jinxed the gnomes before they vaporized.")
  }



}