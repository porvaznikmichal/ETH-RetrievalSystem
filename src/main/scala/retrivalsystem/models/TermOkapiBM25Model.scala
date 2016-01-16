package retrivalsystem

// https://en.wikipedia.org/wiki/Okapi_BM25
object TermOkapiBM25Model extends Model {

    val k1 = 1.5
    val b  = 0.75

    def score(q: QueryStats, d: DocumentStats, c: CollectionStats) = {
        // test score: 0.4403730398954475
        q.tokens.map(
            t => c.idf2(t) *
                 d.tf(t)  * (k1 + 1) /
                 (
                     d.tf(t) +
                     k1 * (1 - b + b * (d.tfSum / c.avgLen))
                 )
        ).sum
    }


}
