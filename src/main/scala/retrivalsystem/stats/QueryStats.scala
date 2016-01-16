package retrivalsystem

import ch.ethz.dal.tinyir.processing.Document

/**
 * QueryStats make all useful statistics about a query available
 * and precomputed for all possible scoring models
 */
class QueryStats(_tokens: Seq[String], _tf: Map[String, Int]) {

    val tokens = _tokens

    // Term frequency of content
    def tf(term: String) = _tf.getOrElse(term,0)

    // Sum of content tf
    val tfSum = _tf.values.sum

    // Max tf of query terms
    val tfMax = _tf.values.max

}

object QueryStats {
    def apply(q: Query) = {

        // Precomputes term frequencise
        val tf = Util.calculateTermFreqFromTokens(q.tokens)

        // Return new DocumentStat instance
        new QueryStats(q.tokens, tf)
    }
}
