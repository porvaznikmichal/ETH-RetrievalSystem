package retrivalsystem

object LangModel extends Model {

    def score(q: QueryStats, d: DocumentStats, c: CollectionStats) = {

        var lambda = 0.2

        val wordProb = q.tokens.map(
            t =>  (1 - lambda) * (d.tf(t).toDouble / d.tfSum) +
                  lambda * (c.cf(t).toDouble / c.cfSum)
        )
        wordProb.map(Util.log2(_)).sum
    }


}
