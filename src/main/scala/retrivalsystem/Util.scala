package retrivalsystem

import java.io._
import scala.io.Source
import ch.ethz.dal.tinyir.processing.TipsterCorpusIterator
import com.github.aztek.porterstemmer.PorterStemmer
import ch.ethz.dal.tinyir.processing.StopWords

object Util {

    // Preprocess the collection to calculate collection freq. and document
    // freq. for each term, also to count number of documents in corpus.
    def preprocess(zipsdir : String, termCache: String, sizeCache: String) {

        // Iterator of corpus
        val iter = new TipsterCorpusIterator(zipsdir)

        // Mutable map to be filled with terms from collection
        val termstats = scala.collection.mutable.Map[String, TermStats]()

        var count = 0;
        for(doc <- iter) {

            // Term freq
            val tf = calculateTermFreqFromTokens(doc.tokens)

            // Adds documents tf to collection term stats (both cf and df)
            termstats ++= tf.map {
                case(t,f) => t -> (
                    TermStats(f,1) + termstats.getOrElse(t, TermStats(0,0))
                )
            }

            if(count % 20000 == 0) {
                println("iter: " + count)
            }
            count += 1
        }

        // filter words occuring only once, a lot of those in the corpus..
        val termstatsClean = termstats.filter({case (term, stats) => stats.cf > 2})

        // Saves number of documents in corpus to file
        val w = new PrintWriter(new File(sizeCache))
        w.write("" + count)
        w.close

        // Save TermStats to file
        val pw = new PrintWriter(new File(termCache))
        termstatsClean.foreach { case(term, stat) => pw.write(term + "," + stat.cf + "," + stat.df + "\n") }
        pw.close
    }

    def calculateTermFreqFromTokens(tokens: Seq[String]) = {
        // IMPORTANT with the ordering!!
        // Make lowercase
        // Remove stop words
        // Group by word
        // Stem every word
        // Group again by word and aggregate tf
        // Done!
        /*
        StopWords.filter(tokens.map(_.toLowerCase())).
            groupBy(identity).mapValues(l => l.length).
            groupBy(t => PorterStemmer.stem(t._1)).
            mapValues(_.foldLeft(0)(_+_._2))
        */
        // Slow with stemming so here's without
        StopWords.filter(tokens.map(_.toLowerCase().replaceAll("[^a-z0-9]", "")))
                 .groupBy(identity).mapValues(l => l.length)

    }

    def tokenize(text: String) = {
        processTokens(text.split("[ .,;:?!\t\n\r\f-/]+").toList)
    }

    def processTokens(tokens: Seq[String]) : Seq[String] = {
        //StopWords.filter(tokens).map(t => PorterStemmer.stem(t))
        StopWords.filter(tokens)
    }

    // Log2 to be used by other parts of the program
    val lnOf2 = scala.math.log(2) // natural log of 2
    def log2(x: Double): Double = scala.math.log(x) / lnOf2

}
