package retrivalsystem

/**
 *
 * @param _ts Map with term as key and its TermStats as value
 * @param _n  Total number of document in collection
 */
class CollectionStats(_ts: Map[String, TermStats], _n: Int) {

    // Collection frequency
    def cf(term: String) = _ts.getOrElse(term,TermStats(0,0)).cf

    // Document frequency
    def df(term: String) = _ts.getOrElse(term,TermStats(0,0)).df

    // Inverse document frequency, TODO: Handle df=0
    def idf(term: String) = Util.log2(_n) - Util.log2(df(term))
    
    def idf2(term: String) = Util.log2(_n - df(term) + 0.5) - Util.log2(df(term) + 0.5)


    // Sum of all collection frequencies
    val cfSum: Int = _ts.map{ case(t,ts) => ts.cf }.sum

    // Average length of documents
    val avgLen = cfSum / _n

}


object CollectionStats {
    def apply(ts: Map[String, TermStats], n: Int) = {
        // Return new CollectionStats instance
        new CollectionStats(ts, n)
    }
}
