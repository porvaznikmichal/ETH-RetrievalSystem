package retrivalsystem

// Used for storing documents with their score
case class DocumentScore(id: String, score: Double) {
    // def id = id.replaceAll("-", "")
    override def toString(): String = this.id + " : " + this.score;
}
