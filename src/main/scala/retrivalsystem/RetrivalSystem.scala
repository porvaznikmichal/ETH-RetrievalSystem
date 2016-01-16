package retrivalsystem

import java.io._
import scala.io.Source
import scala.collection.mutable.ListBuffer
import ch.ethz.dal.tinyir.processing.TipsterCorpusIterator
import ch.ethz.dal.tinyir.lectures.TipsterGroundTruth
import ch.ethz.dal.tinyir.lectures.PrecisionRecall

import scala.collection.immutable.ListMap

// Main object
object RetrivalSystem {

    // Paths to cache files
    val CacheDir  = "cache/"
    val CollectionTermCache = CacheDir + "collection_terms.csv"
    val CollectionSizeCache = CacheDir + "collection_size.csv"

    // Path to output folder
    val OutputDir = "output/"

    def main(args: Array[String]) {

        // Checking arguments
        if(args.size < 3) {
            throw new IllegalArgumentException("Arguments: query-file tipster-zip-path ground-truth")
        }

        // Arguments
        val queryPath = args(0)
        val zipPath   = args(1)
        val truthPath = args(2)

        // Reads preprocessed collection and document freq from file
        println("Loading collection data...")
        val (colSize, colTerms) = preprocessCollection(zipPath)
        println("Collection data loaded!")

        // Read queries from file
        println("Reading queries...")
        val queries = Query.readCSV(queryPath)

        // Define which models to use
        val models: List[Model] = List(LangModel, TermOverlapModel)



        // Create a list of heaps for each query in each model, to keep
        // track of the retrieved documents
        val results: List[List[QueryHeap]] = models.map(
            _ => queries.map(q => new QueryHeap(q, 100))
        )

        println("Done!")
        println("Precalculate queries and collection statistics...")

        // Precalculate query statistics
        val qs = queries.map(q => QueryStats(q))

        // Per calculate all collection statistics in CollectionStats TODO: numbers
        val cs = CollectionStats(colTerms, colSize)

        println("Done!")

        // Iterator of corpus
        println("Load iterator and start loop....")
        val iter = new TipsterCorpusIterator(zipPath)

        var count = 0;
        for(doc <- iter) {

            // Don't process empty document
            if (doc.tokens.length > 0) {

                // Percalculates all document statistics in DocumentStats
                val ds = DocumentStats(doc)

                // For each query
                for(m <- 0 until models.length;
                  q <- 0 until queries.length) {
                  val score = models(m).score(qs(q), ds, cs)
                  results(m)(q).add(DocumentScore(doc.name, score))
                }

            }

            if(count % 10000 == 0) {
                println(f"iter: $count%d")
            }

            count += 1
        }
        println("Done!")

        println("Evaluating models and saving results...")
        val truth = new TipsterGroundTruth(truthPath)
        evaluateAndSaveResults(models, results, truth)
        println("Done!")
    }

    // Preprosses collection and returns corpus size and map with cf and df.
    // If cache file exists it loads it from there instead.
    def preprocessCollection(zipdir: String): (Int, Map[String, TermStats]) = {

        val termCache = new File(CollectionTermCache)
        val sizeCache = new File(CollectionSizeCache)

        // If no cached version exists, we do the whole preprocessing
        if(!termCache.exists || !sizeCache.exists) {
            println("No cache found! Processing collection terms...")
            Util.preprocess(zipdir, CollectionTermCache, CollectionSizeCache)
        }

        val terms = Source.fromFile(CollectionTermCache).getLines.map(
                        line => {
                            line.split(",") match {
                                // .. and match, and finally add key -> value to map
                                case Array(term, cf, df) => term -> TermStats(cf.toInt, df.toInt)
                            }
                        }
                    ).toMap

        val size = Source.fromFile(CollectionSizeCache).mkString.trim.toInt

        (size, terms)
    }

    // Evaluate each model based upon its retrieved documents and the ground truth.
    // Also saves it to the output folder with one ranking and one evalutation file.
    def evaluateAndSaveResults(models: List[Model], results: List[List[QueryHeap]], truth: TipsterGroundTruth) = {

        for(m <- 0 until models.length) {

            var num = 0   // Number of evaluated queries
            var mpr = 0.0 // Precision
            var mre = 0.0 // Recall
            var mf1 = 0.0 // F1
            var map = 0.0 // MAP
            for(q <- 0 until results(m).length) {

                // Only do it for queries we have data to
                if(results(m)(q).query.id <= 90) {

                    // Fecth relevant and retrieved document ids
                    val rel = truth.judgements.get(results(m)(q).query.id.toString).get.toSet

                    // Count number of queries we evaluate (for avg later)
                    num += 1

                    val ret = results(m)(q).results.map(_.id).toSet

                    val pr  = PrecisionRecall.evaluate(ret, rel)

                    mpr += pr.precision
                    mre += pr.recall

                    if(pr.precision + pr.recall > 0) {
                        mf1 += 2 * (pr.precision * pr.recall) / (pr.precision + pr.recall)
                    }
                    map += PrecisionRecall.avgPrecision(ret, rel)
                }
            }

            // Averaging
            mpr /= num
            mre /= num
            mf1 /= num
            map /= num

            // Save evalutaion
            saveEvaluation(mpr, mre, mf1, map, OutputDir + models(m).name + "_evaluation.txt")

            // Saves results to file
            saveResults(results(m), OutputDir + models(m).name + "_ranking.txt")
        }
    }

    // Saves evalutaion to file
    def saveEvaluation(pr: Double, re: Double, f1: Double, map : Double, file: String) = {
        val w = new PrintWriter(new File(file))
        w.write("Precision: " + pr + "\n")
        w.write("Recall: " + re + "\n")
        w.write("F1: " + f1 + "\n")
        w.write("MAP: " + map + "\n")
        w.close
    }

    // Save results to file
    def saveResults(results: List[QueryHeap], file: String) = {
        // Save collection freq to file
        val w = new PrintWriter(new File(file))

        // Loops over each heap in results, and prints retrieved
        // documents for each query
        for(heap <- results) {
            val qid = heap.query.id
            heap.results.zipWithIndex foreach {
                case(d, i) => w.write(qid + " " + (i+1) + " " + d.id + "\n")
            }
        }
        w.close
    }
}
