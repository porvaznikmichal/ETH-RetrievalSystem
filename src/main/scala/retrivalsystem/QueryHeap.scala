package retrivalsystem

import ch.ethz.dal.tinyir.processing.Document
import scala.collection.mutable.PriorityQueue
import scala.math.Ordering.Implicits._

/**
 * A query heap keeps top n documents for that query.
 * q: Query
 * s: Scorer
 * n: Number of results
 */
class QueryHeap(q: Query, n: Int) {
    // number of top results to store
    def num   = n
    def query = q


    // "Reverse" queue, with smallest score on top, so we can easily remove it with dequeue.
    private val heap = new PriorityQueue[DocumentScore]()(Ordering.by(-_.score))

    // Returns the current result
    def results = heap.toList.sortBy(ds => -ds.score)

    // Adds a DocumentScore to the list if it has a high enough score.
    def add(ds: DocumentScore) : Boolean = {

        if(heap.size < num) {
            heap += ds
            true
        } else if(heap.head.score < ds.score) {
            // println("Drops " + heap.head.score + " to " + ds.score)
            heap.dequeue
            heap += ds
            true
        } else {
            false
        }
    }

    override def toString(): String = {
        "\n" + query.toString + "\n" + this.results.mkString("\n")
    }
}
