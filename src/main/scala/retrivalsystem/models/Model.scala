package retrivalsystem

// Abstract class of models, each model should implement a score method
abstract class Model {
    def score(q: QueryStats, d: DocumentStats, c: CollectionStats): Double
    def name = this.getClass.getName.replaceAll("retrivalsystem\\.","").replaceAll("\\$","")
}
