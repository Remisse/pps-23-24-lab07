package ex4

object Math:
  inline def clamp(v: Int, min: Int, max: Int): Int =
    if v < min then min
    else if v > max then max
    else v