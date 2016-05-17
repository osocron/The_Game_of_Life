package net.osocron.model

import scalafx.scene.paint.Color
import scalafx.scene.shape.Rectangle

/**
  * Cell class. This represents a single cell in the game of life. Each cell can either be dead or alive.
  * It can also be selected to help the user navigate trough the cell grid.
  */

case class Cell(px: Int, py: Int, var neighbors: List[Cell]) extends Rectangle{
  var life = false
  def isAlive = life
  def setLife(l: Boolean) = {
    life = l
    if (l) fill = Color.Black
    else fill =  Color.White
  }
  def select() = {
    if (!isAlive) fill = Color.Yellow
  }
  def unSelect() = {
    if (!isAlive) fill = Color.White
  }
}