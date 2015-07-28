/**
 * Created by sameer on 7/28/15.
 */

object PriorityQueue {
  def create[T <% Ordered[T]](v: List[T]): PriorityQueue[T] = {
    def createAux(v: List[T], q: PriorityQueue[T]): PriorityQueue[T] = v match {
      case Nil => q
      case x :: xs => createAux(xs, q.insert(x))
    }
    createAux(v, new PriorityQueue[T](Vector.empty[T]))
  }
}

class PriorityQueue[T <% Ordered[T]](queue: Vector[T]) {
  def parent(i: Int): Int = i match {
    case 0 => -1
    case _ => (i + 1) / 2 - 1
  }

  def verify(i: Int): Boolean = {
    if (youngChild(i) < queue.size)
      if (queue(i) > queue(youngChild(i)))
        false
      else if (youngChild(i) + 1 < queue.size)
        if (queue(i) > queue(youngChild(i) + 1))
          false
        else
          true
      else
        true
    else
      true
  }

  lazy val verified = (for(x <- 0 until queue.size) yield verify(x)).forall(_ == true)

  def youngChild(i: Int): Int = (2 * (i + 1)) - 1

  def swap(v: Vector[T], x: Int, y: Int) = {
    val less = if (x < y) x else y
    val more = if (y < x) x else y
    ((v.take(less) :+ v(more)) ++ v.slice(less + 1, more) :+ v(less)) ++ v.drop(more + 1)

  }

  def bubbleUp(queue: Vector[T], i: Int): Vector[T] = {
    parent(i) match {
      case -1 => queue
      case parent if queue(i) < queue(parent) => bubbleUp(swap(queue, i, parent), parent)
      case _ => queue
    }
  }

  def insert(item: T): PriorityQueue[T] = {
    new PriorityQueue(bubbleUp(queue :+ item, queue.size))
  }

  def bubbleDown(queue: Vector[T], i: Int): Vector[T] = {
    val children = Vector(i, youngChild(i), youngChild(i) + 1).filter(_ < queue.size)
    val minIndex = children.map(i => (i, queue(i))).minBy(_._2)._1
    if (minIndex == i)
      queue
    else
      bubbleDown(swap(queue, i, minIndex), minIndex)
  }

  def isEmpty = queue.isEmpty

  def extractMin: (T, PriorityQueue[T]) = {
    if (queue.isEmpty) throw new NoSuchElementException
    (queue.head,
      if (queue.tail.isEmpty)
        new PriorityQueue[T](Vector.empty[T])
      else
        new PriorityQueue[T](bubbleDown(queue.tail.last +: queue.tail.dropRight(1), 0)))
  }
}
