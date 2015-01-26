package webapp

import scala.scalajs.js.JSApp

import org.scalajs.dom
import scala.scalajs.js
import org.scalajs.jquery._

object HelloWorld extends JSApp {
  def setupUI(): Unit = {
    // dynamically add button
    jQuery("body").append("<button id=\"click-me-button\" type=\"button\">Click me!</button>")

    // register callback for the button
    jQuery("#click-me-button").click( () => 
      jQuery("body").append("<p>You clicked the button!</p>")  )

    // output Hello World string
    jQuery("body").append("<p>Hello World</p>")
  }

  def main(): Unit = {
    jQuery(setupUI _)
  }
}
