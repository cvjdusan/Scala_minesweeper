package operations

final case class Sector(topLeftRow: Int, topLeftCol: Int, bottomRightRow: Int, bottomRightCol: Int) {

  def isValid: Boolean = {
    topLeftRow >= 0 && topLeftCol >= 0 &&
    bottomRightRow >= topLeftRow && bottomRightCol >= topLeftCol
  }

  def width: Int = bottomRightCol - topLeftCol + 1
  def height: Int = bottomRightRow - topLeftRow + 1

  def contains(row: Int, col: Int): Boolean = {
    row >= topLeftRow && row <= bottomRightRow &&
    col >= topLeftCol && col <= bottomRightCol
  }

} 