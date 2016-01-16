package retrivalsystem

import scala.io;
import scala.collection.mutable.ListBuffer

class Query(i: Int, q: String) {
    def id    = i
    def query = q

    val tokens = Util.tokenize(query).distinct.map(_.toLowerCase().replaceAll("[^a-z0-9]", ""))
    // println(tokens)
    override def toString(): String = "[" + id + "] '" + query + "')";
}

object Query {
    /**
     * Read queries from file, in the format "ID,Query"
     */
    def readCSV(path: String) : List[Query] = {
        val q = new ListBuffer[Query]
        val file = io.Source.fromFile(path)
        for(line <- file.getLines) {
            val cols = line.split(";").map(_.trim)
            q += new Query(cols(0).toInt, cols(1))
        }
        file.close
        return q.toList
    }
}
