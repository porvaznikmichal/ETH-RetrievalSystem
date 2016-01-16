package retrivalsystem

object TermTfIdfModel extends Model {

    def score(q: QueryStats, d: DocumentStats, c: CollectionStats) = {
        // Term frequency * Inverse document frequency
        q.tokens.map(t => d.ltf(t) * c.idf(t)).sum

        // Query weight * Term freq. * Inverse document freq. : ~0.15
        //q.tokens.map(
        //    t => (0.5 + 0.5 * q.tf(t) / q.tfMax) * ( d.tf(t) * c.idf(t) )
        //).sum

        // Augmented frequency * Inversed document frequency : 0.27971881256346903
        //q.tokens.map(
        //    t => (0.5 + 0.5 * d.tf(t) / d.tfMax) * c.idf(t)
        //).sum

        // Query weight * Augmented frequency * Inversed document frequency : 0.27971881256346903
        // q.tokens.map(
        //     t => (0.5 + 0.5 * q.tf(t) / q.tfMax) *
        //          (0.5 + 0.5 * d.tf(t) / d.tfMax) *
        //          c.idf(t)
        // ).sum
    }


}
