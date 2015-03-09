/*
 * The following code implements a very simple 2D graphics library.
 * The screen is represented as a 2D array of characters, supporting 
 * four different colors, black, red, green, and blue.
 *
 * Currently points are represented as to integers, x and y. 
 * 
 *  The library consists of 1 base object
 * 
 *      Color object represents colors
 *       ColorT - is the type used to represent colors
 *       black - is the color black
 *       red - is the color red
 *       green - is the color green
 *       blue - is the color blue
 * 
 *  The library consists of 3 base classes:
 * 
 *  
 *     Screen class represents our screen (in this case the console)
 *        primary constructor - takes size of screen in width and height
 *        on - method returns true if point is within screens coordiates
 *        setBackground - sets the background color
 *        getBackground - gets the background color
 *        getWidth - gets the width of the screen
 *        getHeight - gets the height of the screen
 *        setPoint - sets a given point on the screen to a specified color
 *        clear - sets each point in the screen to the current backgound color
 * 
 *      Shape abstract class resentents the set of all shapes that can be drawn
 *        draw - method implemented by subclasses to draw a particular shape
 * 
 *     Scene class represents a scene to be drawn, i.e. it is a collection of shapes
 *        push - add a shape to the scene. Shapes are drawn in newest first, i.e.
 *               last pushed is drawn first and earlier ones drawn over top, if they 
 *               overlap
 *        render - draw scene on a given screen
 * 
 *   The library has two subclasses that implement different shapes
 * 
 *      Square - a square
 *      primary constructor - takes an x,y coordiate, size, and a color
 *      draw - given a screen implements drawing the sqaure on screen, if it fits
 * 
 *      Rectangle - a rectangle
 *      primary constructor - takes an x,y coordiate, width, height, and a color
 *      draw - given a screen implements drawing the rectangle on screen, if it fits
 *      NOTE: this implementation of draw is not given, it is your job to do so
 *
 * Note: the origin (0,0) is assumed to be at the top left corner of screen
 *
 * Exercises: 
 *
 *   - implement a method to draw a rectangle of a given length and height at point x,y
 *   - reimplement the library to use a class to represent points
 */

import Array._

object Color {
  type ColorT = Char

  // attributes

  val black : ColorT = ' '
  val red   : ColorT = 'R'
  val green : ColorT = 'G'
  val blue  : ColorT = 'B'

  // constructors

  // methods
}

class Screen ( sizeX : Int, sizeY : Int ) {

  type ScreenT = Array[Array[Color.ColorT]]

  // attributes

  private var screen : ScreenT = ofDim[Color.ColorT](sizeX, sizeY)

  private var background : Color.ColorT = Color.black

  // constuructors
  clear() // clear the screen on construction

  // methods

  // return true if point sits on screen, otherwise false
  def on( x : Int, y : Int ) : Boolean =
    x >= 0 && x < sizeX && y >= 0 && y < sizeY

  def setBackground ( c : Color.ColorT ) : Unit =
    background = c

  def getBackground() : Color.ColorT = background

  def getSizeX() : Int = sizeX
  def getSizeY() : Int = sizeY

  def setPoint( x : Int, y : Int , c : Color.ColorT ) =
    screen(x)(y) = c

  // clear the screen with current background color
  def clear() : Unit =
    for {
      y <- 0 until sizeY
      x <- 0 until sizeX
    } screen(x)(y) = background

  // display the screen's contents to the console window
  def display() : Unit = {
    for {
      x <- 0 until sizeX+2
    } print("_")

    for (y <- 0 until sizeY) {
      print("\n|")

      for {
        x <- 0 until sizeX
      } print( screen(x)(y) )

      print("|")
    }

    println("")
    for {
      x <- 0 until sizeX+2
    } print("_")

    println("")
  }
}

abstract class Shape {
  def draw( screen : Screen ) : Unit
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
    objects.foldLeft(()) ((a,shape) => shape.draw(screen))
  }
}

object Console2D {
  def main(args: Array[String]) {

    // Setup the display
    val screen = new Screen(10,10)
    screen.setBackground(Color.black)

    // Draw a simple scene, consisting of 2 squares and a (yet to be
    // implemented) rectangle

    var scene = new Scene()

    /*
     *  what happens if we reverse the order of pushing the two squares?
     *  try it!
     */

    // draw a couple of overlapping squares
    scene.push(new Square(2,2,3,Color.red))
    scene.push(new Square(4,1,2,Color.green))

    // draw a rectangle
    scene.push(new Rectangle(1,6,7,2,Color.blue))

    scene.render(screen)

    // Finally make the screen visible
    screen.display()
  }
}
