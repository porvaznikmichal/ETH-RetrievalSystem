package retrivalsystem

import ch.ethz.dal.tinyir.processing.Document

/**
 * DocumentStats make all useful statistics about a document available
 * and precomputed for all possible scoring models
 */
class DocumentStats(_tf: Map[String, Int], _headTf: Map[String, Int]) {

    // Term frequency of head text (e.g. title and headlines)
    def headTf(term: String) = _headTf.getOrElse(term,0)

    // Term frequency of content
    def tf(term: String) = _tf.getOrElse(term,0)

    // Sum of content tf
    val tfSum = _tf.values.sum

    // Log term frequency
    def ltf(term: String) = Util.log2(_tf.getOrElse(term,0) + 1)

    // Max tf in document
    val tfMax = _tf.values.max

    // Euclidian norm, TODO: for headTf also
    val tfNorm = Math.sqrt(_tf.values.map(x => x*x).sum)

    val headTfNorm = Math.sqrt(_headTf.values.map(x => x*x).sum)
    // TODO: Include headTf

}

object DocumentStats {
    def apply(doc: Document) = {

        // Precomputes term frequencise
        val tf = Util.calculateTermFreqFromTokens(doc.tokens)
        val headTf = Util.calculateTermFreqFromTokens(Util.tokenize(doc.title))

        // Return new DocumentStat instance
        new DocumentStats(tf, headTf)
    }
}
