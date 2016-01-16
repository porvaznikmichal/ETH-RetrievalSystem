package retrivalsystem

/**
 * Case Class for counting collection frequency and document frequency for
 * a term, created to be used in a map where we could use the '+'-operator
 *
 * cf: Collection frequency, total number of occurances in all documents
 * df: Document frequency, total number of document it occurs in
 */
case class TermStats(cf: Int, df: Int) {
    def +(ts: TermStats) = TermStats(this.cf + ts.cf, this.df + ts.df)
}
