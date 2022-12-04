import java.util
import scala.collection.immutable.HashSet
import scala.collection.{MapView, mutable}
import scala.math.{pow, sqrt}

object Simil extends App {
import scala.io.Source
  import scala.collection.mutable.ListBuffer
  val PATH_STOPWORDS = "english-stop.txt"
  def main(): Unit = {
    println("Select a file")
    val text: String = Source.fromFile(scala.io.StdIn.readLine()).mkString
    println("Select a file to compare")
    val text2: String = Source.fromFile(scala.io.StdIn.readLine()).mkString
    //show all the results of the functions to show that they do their job
    mostrarFreq(freq(text))
    println("\n\n")
    mostrarFreq(nonstopfreq(text))
    println("\n\n")
    paraulafreqfreq(text)
    println("\n\n")
    mostrarNgrama(ngrama(3,text))
    println("\n\n")
    print("La similitud entre els 2 fitxers es de "+cosinesim(text, text2))
  }
  def mostrarFreq(list: List[(String, Int)]): Unit ={
    val total = list.map(w => w._2).sum
    println("Num de Paraules: " + total + "  Diferents: " + list.length)
          println("Paraules             ocurrencies             frequencia")
          println("---------------------------------------------------------")
          for (x <- 0 to 9) {
            val frequencia = (list(x)._2*100).toDouble/total
            println(list(x)._1 + "      " + list(x)._2 + "      " + f"$frequencia%1.2f")
          }
  }
  def mostrarNgrama(list: List[(String, Int)]): Unit = {
    for(x <- 0 to 9){
      println(list(x)._1 + "      " + list(x)._2)
    }
  }

  def splitWords(llibre: String): Array[String] = {
    //Cleanse the text changing deleting all non characters from the text
    // Splits the text and creates a list with all the words
    llibre.replaceAll("[\\W\\r\\n\\t\\d_]+", " ").toLowerCase().split(" ").filter(_.nonEmpty)
  }
  def freq(llibre: String): List[(String, Int)] ={
    //take a list of words and creates another list that counts all the occurrences of every word in the text
      splitWords(llibre).map(w => (w, 1)).groupBy(_._1).map{ _._2.reduce({ (a,b) => (a._1, a._2+b._2)})}.toList.sortBy(_._2).reverse
  }

  def nonstopfreq(llibre: String) : List[(String, Int)] = {
    //text with all Stop Words we don't want in a text
    val text: String = Source.fromFile(PATH_STOPWORDS).mkString
    //Splits string into a list of strings
    var textSplit : Set[String] = new HashSet[String]
    textSplit = splitWords(text).toSet

    //Splits the text
    //Creates a List of tuples with a String and the number of ocurrences in that text but erasing the stopwords in textSplit
    splitWords(llibre).filter(w => !textSplit.contains(w)).map(w => (w,1)).groupBy(_._1).map {
    _._2.reduce({ (a, b) => (a._1, a._2 + b._2) })
    }.toList.sortBy(_._2).reverse




  }
  def paraulafreqfreq(Llibre: String) {
    // create a list with counting the number of words that appear a specific number of times
    // Sort by the max number of words that appear a specific number of times
    val frequens = freq(Llibre).groupBy(_._2).mapValues(_.size).toList.sortBy(_._2).reverse
    println("Les 10 paraules mes frequens")
    for (x <- 0 to 9) {
      println(frequens(x)._2 + " paraules apareixen " + frequens(x)._1+" vegades")
    }
    // revers the list to show the less frequents number of times a word appear
    val menysfrequens = frequens.reverse
    println("Les 5 paraules menys frequens")
    for (x <- 0 to 4) {
      println(menysfrequens(x)._2 + " paraules apareixen " + menysfrequens(x)._1 + " vegades")
    }
  }


  def ngrama(n: Int, llibre: String): List[(String, Int)] ={
    // splitWords(llibre).sliding(n).toList.map(x => x.mkString(" ")) -> creates a list with n words inside separate by spaces
    // the map creates a list of tuples with the String and the number of times it repeats
    //sorted by ocurrences in descending order
   splitWords(llibre).sliding(n).toList
      .map(x => x.mkString(" ")).map(w => (w, 1)).groupBy(_._1).map{ _._2.reduce({ (a,b) => (a._1, a._2+b._2)})}
      .toList.sortBy(_._2).reverse
  }

  def cosinesim(llibre: String, llibre2: String): Double ={
    //split the 2 texts into a hashmap
    var textSplit: Set[String] = new HashSet[String]
    textSplit = splitWords(llibre).toSet
    var textSplit2: Set[String] = new HashSet[String]
    textSplit2 = splitWords(llibre2).toSet
    // Creates a list of tuples with the Word and the weight
    // the weight is the absolute frequency of the word/ the word with the bigger absolute frequency
    val auxLlibre1 = nonstopfreq(llibre)
    val aux2llibre1 = auxLlibre1.map(w => (w._1, w._2.toDouble/auxLlibre1.head._2)).sortBy(_._2).reverse
    val auxLlibre2 = nonstopfreq(llibre2)
    val aux2llibre2 = auxLlibre2.map(w => (w._1, w._2.toDouble/auxLlibre2.head._2)).sortBy(_._2).reverse
    //Add all the words that not appear to the other file with weigth 0
    val llistallibre1 = (aux2llibre1 ++ aux2llibre2.filter(l2 => !textSplit.contains(l2._1)).map(l2 => (l2._1, 0.0))).sortBy(_._1)
    val llistallibre2 = (aux2llibre2 ++ aux2llibre1.filter(l2 => !textSplit2.contains(l2._1)).map(l2 => (l2._1, 0.0))).sortBy(_._1)

    //send the lists to calculate the cosine similarity
    cosine_similarity(llistallibre1, llistallibre2)

  }

  def cosine_similarity(query: List[(String, Double)], doc: List[(String, Double)]): Double = {
    // Returns the cosine similarity between the two vectors

    var dotProduct = 0.0
    var normA = 0.0
    var normB = 0.0

    for ((p1, p2) <- query.zip(doc)) {
      // multiply the weights of every element of the lists
      dotProduct += p1._2 * p2._2
      // sum every element squared
      normA += math.pow(p1._2, 2)
      normB += math.pow(p2._2, 2)
    }
    // divide the product from the sum of the 2 lists with the product of the square root of normA and normB
    dotProduct / (math.sqrt(normA) * math.sqrt(normB))
  }
main()
};
