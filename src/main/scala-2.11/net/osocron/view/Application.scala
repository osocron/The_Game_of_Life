package net.osocron.view

import javafx.concurrent.Task

import net.osocron.model.Cell

import scala.concurrent.Future
import scalafx.application.{JFXApp, Platform}
import scalafx.scene._
import scalafx.scene.input.{KeyCode, KeyEvent, MouseEvent}
import scalafx.scene.paint._
import scalafx.Includes._
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * This is a Scala implementation of Conway's game of life.
  *
  * Controls:
  * Enter -> Go to next generation
  * S -> Loop through generation or stop the loop
  * Q -> Erase the board
  * Up, Down, Left, Right -> Navigate through the board
  * Space -> Select or unselect a cell
  * P -> Go back in time one generation
  *
  */

object Application extends JFXApp{

  val group = new Group
  val currentCells = createCells
  val nextCells = createCells
  val loopThread = new Thread(createTask)
  var timeLine = List(createSnapShot)
  var xp = 25
  var yp = 25
  var execLoop = false

  stage = new JFXApp.PrimaryStage {
    title.value = "Game of Life"
    width = 500
    height = 530
    scene = new Scene {
      currentCells.foreach(array => array.foreach(getChildren.add(_)))
      onKeyPressed = (ke: KeyEvent) => {
        ke.code match {
          case KeyCode.Enter => sync(currentCells, nextCells)
          case KeyCode.S => execLoop = !execLoop; if (execLoop) loop
          case KeyCode.Q => currentCells.foreach(array => array.foreach(_.setLife(false))); timeLine = List(createSnapShot)
          case KeyCode.P => if (timeLine.nonEmpty) {reverseState(currentCells, timeLine.last); timeLine = timeLine.dropRight(1)}
          case KeyCode.Down => if (yp != 49) {currentCells(xp)(yp).unSelect(); currentCells(xp)(yp+1).select(); yp = yp + 1}
          case KeyCode.Up => if (yp != 0) {currentCells(xp)(yp).unSelect(); currentCells(xp)(yp-1).select(); yp = yp - 1}
          case KeyCode.Right => if (xp != 49) {currentCells(xp)(yp).unSelect(); currentCells(xp+1)(yp).select(); xp = xp + 1}
          case KeyCode.Left => if (xp != 0) {currentCells(xp)(yp).unSelect(); currentCells(xp-1)(yp).select(); xp = xp - 1}
          case KeyCode.Space => if (currentCells(xp)(yp).isAlive) currentCells(xp)(yp).setLife(false) else currentCells(xp)(yp).setLife(true)
          case _ =>
        }
      }
    }
  }

  def loop: Future[Unit] = Future {
    createTask.run()
  }

  def createTask: Task[Unit] = new Task[Unit]() {
    override def call(): Unit = {
      while (execLoop) {
        Platform.runLater(sync(currentCells, nextCells))
        Thread.sleep(500)
      }
    }
  }

  def createSnapShot = {
    val array = Array.ofDim[Int](50,50)
    for(
      i <- 0 to 49;
      j <- 0 to 49
    ){
      array(i)(j) = 0
    }
    array
  }

  def createCells = {
    val cells = Array.ofDim[Cell](50,50)
    for(
      i <- 0 to 49;
      j <- 0 to 49
    ){
      cells(i)(j) = new Cell(i,j,List()) {
        width = 10
        height = 10
        x = i * 10
        y = j * 10
        stroke = Color.Gray
        fill = Color.White
        onMouseClicked = (me: MouseEvent) => {
          if (isAlive) setLife(false)
          else setLife(true)
        }
      }
    }
    fillNeighbors(cells)
    cells
  }

  def addToTimeLine(currentCellArray: Array[Array[Cell]]) =
    timeLine = timeLine :+ createArrayFromCellArray(currentCellArray)

  def createArrayFromCellArray(currentCellArray: Array[Array[Cell]]) = {
    val array = createSnapShot
    currentCellArray.foreach(_.foreach(c => if (c.isAlive) array(c.px)(c.py) = 1 else array(c.px)(c.py) = 0))
    array
  }

  def sync(currentCellArray: Array[Array[Cell]], nextCellArray: Array[Array[Cell]]) {
    addToTimeLine(currentCellArray)
    copyState(nextCellArray,currentCellArray)
    ruleOfLife(currentCellArray, nextCellArray, c => c.neighbors.count(_.isAlive) < 2 || c.neighbors.count(_.isAlive) > 3, l = false)
    ruleOfLife(currentCellArray, nextCellArray, c => c.neighbors.count(_.isAlive) == 3, l = true)
    copyState(currentCellArray,nextCellArray)
  }

  def copyState(originalArray: Array[Array[Cell]], changedArray: Array[Array[Cell]]) =
    originalArray.foreach(_.foreach(c => c.setLife(changedArray(c.px)(c.py).isAlive)))

  def ruleOfLife(currentCells: Array[Array[Cell]], nextCells: Array[Array[Cell]], p: Cell => Boolean, l: Boolean) =
    currentCells.foreach(_.foreach(c => if (p(c)) nextCells(c.px)(c.py).setLife(l)))

  def reverseState(cellArray: Array[Array[Cell]], snapShot: Array[Array[Int]]) =
    cellArray.foreach(_.foreach(c => if (snapShot(c.px)(c.py) == 1) c.setLife(true) else c.setLife(false)))

  def fillNeighbors(cells: Array[Array[Cell]]) {
    for (
      x <- 0 to 49;
      y <- 0 to 49;
      z <- 0 to 7
    ) {
      z match {
        case 0 =>
          if (validateZero(x) && validateZero(y)) cells(x)(y).neighbors = cells(x)(y).neighbors :+ cells(x-1)(y-1)
          else cells(x)(y).neighbors = cells(x)(y).neighbors :+ Cell(x-1,y-1,List())
        case 1 =>
          if (validateZero(y)) cells(x)(y).neighbors = cells(x)(y).neighbors :+ cells(x)(y-1)
          else cells(x)(y).neighbors = cells(x)(y).neighbors :+ Cell(x,y-1,List())
        case 2 =>
          if (validateLimit(x) && validateZero(y)) cells(x)(y).neighbors = cells(x)(y).neighbors :+ cells(x+1)(y-1)
          else cells(x)(y).neighbors = cells(x)(y).neighbors :+ Cell(x+1,y-1,List())
        case 3 =>
          if (validateZero(x)) cells(x)(y).neighbors = cells(x)(y).neighbors :+ cells(x-1)(y)
          else cells(x)(y).neighbors = cells(x)(y).neighbors :+ Cell(x-1,y,List())
        case 4 =>
          if (validateLimit(x)) cells(x)(y).neighbors = cells(x)(y).neighbors :+ cells(x+1)(y)
          else cells(x)(y).neighbors = cells(x)(y).neighbors :+ Cell(x+1,y,List())
        case 5 =>
          if (validateZero(x) && validateLimit(y)) cells(x)(y).neighbors = cells(x)(y).neighbors :+ cells(x-1)(y+1)
          else cells(x)(y).neighbors = cells(x)(y).neighbors :+ Cell(x-1,y+1,List())
        case 6 =>
          if (validateLimit(y)) cells(x)(y).neighbors = cells(x)(y).neighbors :+ cells(x)(y+1)
          else cells(x)(y).neighbors = cells(x)(y).neighbors :+ Cell(x,y+1,List())
        case 7 =>
          if (validateLimit(x) && validateLimit(y)) cells(x)(y).neighbors = cells(x)(y).neighbors :+ cells(x+1)(y+1)
          else cells(x)(y).neighbors = cells(x)(y).neighbors :+ Cell(x+1,y+1,List())
      }
    }
  }

  def validateZero(n: Int): Boolean = if (n == 0) false else true
  def validateLimit(n: Int): Boolean = if (n == 49) false else true

}
