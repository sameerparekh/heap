import org.scalatest._
import scala.math.Ordering._

class PriorityQueueSpec extends FlatSpec with prop.PropertyChecks {
  it should "take in values and the extractMin should be the min" in {
    forAll { (l: List[Int]) =>
      val heap = PriorityQueue.create(l)
      assert(heap.verified == true, "heap is properly formed")
      if(!heap.isEmpty) {
        val (min, _) = heap.extractMin
        assert(l.min == min)
      }
    }
  }
  it should "give me my list sorted when I repeatedly extractMin" in {
    forAll { (l: List[Int]) =>
      val heap = PriorityQueue.create(l)
      assert(heap.verified == true, "heap is properly formed")
      val sortedList = l.sorted
      def heapSort(l: List[Int]): List[Int] = {
        val heap = PriorityQueue.create(l)
        def hsAux(h: PriorityQueue[Int], sorted: List[Int]): List[Int] = {
          assert(h.verified == true, "heap is properly formed")
          if (h.isEmpty)
            sorted
          else {
            val (min, newHeap) = h.extractMin
            hsAux(newHeap, sorted :+ min)
          }
        }
        hsAux(heap, List.empty[Int])
      }
    assert(heapSort(l) == sortedList)
    }
  }
}
