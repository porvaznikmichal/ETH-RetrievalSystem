package retrivalsystem

object TermOverlapModel extends Model {

    def score(q: QueryStats, d: DocumentStats, c: CollectionStats) = {

        val qtf  = q.tokens.map(q => d.tf(q)).filter(_ > 0)
        val hqtf  = q.tokens.map(q => d.headTf(q)).filter(_ > 0)
        val qlen = Math.sqrt(q.tokens.length)

        // Terms incommon + term overlap
        //hqtf.length + hqtf.sum.toDouble / (d.headTfNorm * qlen) +
        qtf.length + qtf.sum.toDouble / (d.tfNorm * qlen)
    }


}
