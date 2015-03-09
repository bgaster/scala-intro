/*
 * The following code implements a very simple 2D graphics library.
 * The screen is represented as a 2D array, supporting four different
 * colors, black, red, green, and blue. 
 *
 * Currently points are represented as to integers, x and y. 
 * 
 *  The library consists of 1 base object
 * 
 *      Color object represents colors
 *       ColorT - is the type used to represent colors
 *       black  - is the color black
 *       red    - is the color red
 *       green  - is the color green
 *       blue   - is the color blue
 * 
 *  The library consists of 3 base classes:
 * 
 *  
 *     Screen class represents our screen (in this case the console)
 *        primary constructor - takes size of screen in width and height
 *        on                  - method returns true if point is within screens coordiates
 *        setBackground       - sets the background color
 *        getBackground       - gets the background color
 *        getWidth            - gets the width of the screen
 *        getHeight           - gets the height of the screen
 *        setPoint            - sets a given point on the screen to a specified color
 *        clear               - sets each point in the screen to the current backgound color
 * 
 *      Shape abstract class resentents the set of all shapes that can be drawn
 *        draw                - method implemented by subclasses to draw a particular shape
 * 
 *     Scene class represents a scene to be drawn, i.e. it is a collection of shapes
 *        push                - add a shape to the scene. Shapes are drawn in newest first, i.e.
 *                              last pushed is drawn first and earlier ones drawn over top, if they 
 *                              overlap
 *        render              - draw scene on a given screen
 * 
 *   The library has two subclasses that implement different shapes
 * 
 *      Line                  - a line
 *        primary constructor - takes coordiates x0,y0 and x1,y1 and a color
 *        draw                - given a screen implements drawing a line between the two points, using
 *                              an integer variant of Bresenham's algorithm 
 * 
 *      Square - a square
 *        primary constructor - takes an x,y coordiate, size, and a color
 *        draw                - given a screen implements drawing the sqaure on screen, if it fits
 * 
 *      Rectangle - a rectangle
 *        primary constructor - takes an x,y coordiate, width, height, and a color
 *        draw                - given a screen implements drawing the rectangle on screen, if it fits
 *                              NOTE: assumes back to front rednering, i.e. the sequence of pushed shapes 
 *                                    is submitted such that the last push will appear on top
 * 
 * NOTE: the origin (0,0) is assumed to be at the top left corner of screen
 *
 * Exercises: 
 *
 *   1. implement a method to draw a rectangle of a given length and height at point x,y
 * 
 *   2. reimplement the library to use a class to represent points
 * 
 *   3. reimplement the draw method for square and rectangle interms of Line
 *      what do you notice about this approach?
 * 
 *   4. modify the Screen class to use the length method of the screen
 *      array rather than width and height
 * 
 *   6. Extend the library to support displaying text
 * 
 */
import Array._

package console2D {

  object Color {
    // We hack into the Console library to use background colors

    type ColorT = String

    // attributes

    // As we are simply changing the background color, just print a space
    private val char : String = " "

    val black : String = Console.BLACK_B + char
    val red : String   = Console.RED_B + char
    val green : String = Console.GREEN_B + char
    val blue : String  = Console.BLUE_B + char

    // constructors

    // methods
  }

  class Screen ( val width : Int, val height : Int ) {

    private type ScreenT = Array[Array[Color.ColorT]]

    // attributes

    private var screen : ScreenT = ofDim[Color.ColorT](width, height)

    private var background : Color.ColorT = Color.black

    // constuructors
    clear() // clear the screen on construction

    // methods

    // return true if point sits on screen, otherwise false
    def on( x : Int, y : Int ) : Boolean =
      x >= 0 && x < width && y >= 0 && y < height

    def setBackground ( c : Color.ColorT ) : Unit =
      background = c

    def getBackground() : Color.ColorT = background

    def getSizeX() : Int = width
    def getSizeY() : Int = height

    def setPoint( x : Int, y : Int , c : Color.ColorT ) =
      screen(x)(y) = c

    // clear the screen with current background color
    def clear() : Unit =
      for {
        y <- 0 until height
        x <- 0 until width
      } screen(x)(y) = background

    // display the screen's contents to the console window
    def display() : Unit = {
      // send clear console sequence
      print(background + "\033[2J")

      for (y <- 0 until height) {
        for {
          x <- 0 until width
        } print( screen(x)(y) ) //+ Color.char)

        println("")
      }

      println("")
    }
  }

  abstract class Shape {
    def draw( screen : Screen ) : Unit
  }

  class Line (
    x0 : Int, y0 : Int,
    x1 : Int, y1 : Int,
    c : Color.ColorT ) extends Shape {

    def draw( screen : Screen ) : Unit = {
      val dx = x1 - x0
      val dy = y1 - y0

      var dir = 2*dy - dx
      screen.setPoint(x0,y0,c)
      var y = y0

      for (x <- x0+1 until x1) {
        if (dir > 0) {
          y = y + 1
          screen.setPoint(x,y,c)
          dir = dir + (2*dy - 2*dx)
        }
        else {
          screen.setPoint(x,y,c)
          dir = dir + (2*dy)
        }
      }
    }
  }

  class Square ( x : Int, y : Int, size : Int, c : Color.ColorT ) extends Shape {
    def draw( screen : Screen ) : Unit = {
      if (screen.on(x,y) && screen.on(x+size, y+size))
        for {
          yy <- y until y + size
          xx <- x until x + size
        } screen.setPoint(xx,yy,c)
      else
        ()
    }
  }

  class Rectangle (
    x : Int,
    y : Int,
    width : Int,
    height : Int,
    c : Color.ColorT) extends Shape {

    def draw(  screen : Screen ) : Unit = {
      // exercise to implement
    }
  }

  /*
   *  A scene consists of a list of shapes to be rendered
   */
  class Scene {
    private var objects : List[Shape] = Nil

    def push( s : Shape ) =
      objects = s :: objects

    def render( screen : Screen ) : Unit = {
      screen.clear()
      objects.foldRight(()) ((shape,a) => shape.draw(screen))
    }
  }

} // package console2D

object Main {
  def main(args: Array[String]) {
    import console2D._

    // Setup the display
    val screen = new Screen(20,20)
    screen.setBackground(Color.black)

    // Draw a simple scene, consisting of 2 squares and a (yet to be
    // implemented) rectangle, and a line

    var scene = new Scene()

    /*
     *  what happens if we reverse the order of pushing the two squares?
     *  try it!
     */

    // draw a couple of overlapping squares
    scene.push(new Square(2,2,3,Color.red))
    scene.push(new Square(4,1,2,Color.green))

    // draw a line
    scene.push(new Line(6,10,15,20, Color.blue))

    // draw a rectangle
    scene.push(new Rectangle(1,6,7,2,Color.blue))

    scene.render(screen)

    // Finally make the screen visible
    screen.display()
  }
}
