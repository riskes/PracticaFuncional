import java.util
import scala.collection.immutable.HashSet
import scala.collection.{MapView, mutable}
import scala.math.{pow, sqrt}

object Simil extends App {
import scala.io.Source
  import scala.collection.mutable.ListBuffer

  def main(): List[(String, Int)] = {
    //ubitext = scala.io.StdIn.readLine();
    val text: String = Source.fromFile("C:\\Users\\arisq\\Downloads\\Practica Funcional + Objectes (1a. part)-20221024\\pg11.txt").mkString
    val text2: String = Source.fromFile("C:\\Users\\arisq\\Downloads\\Practica Funcional + Objectes (1a. part)-20221024\\pg12.txt").mkString

    cosinesim(text, text2)
    freq(text)
  }

  def splitWords(llibre: String): Array[String] = {
    llibre.replaceAll("[\\W\\r\\n\\t\\d_]+", " ").toLowerCase().split(" ").filter(_.nonEmpty)
  }
  def freq(llibre: String): List[(String, Int)] ={
      splitWords(llibre).map(w => (w, 1)).groupBy(_._1).map{ _._2.reduce({ (a,b) => (a._1, a._2+b._2)})}.toList.sortBy(_._2).reverse
  }

  def nonstopfreq(llibre: String) : List[(String, Int)] = {

    val text: String = Source.fromFile("C:\\Users\\arisq\\Downloads\\Practica Funcional + Objectes (1a. part)-20221024\\english-stop.txt").mkString
    var textSplit : Set[String] = new HashSet[String]
    textSplit = splitWords(text).toSet

    //Split de les 11
    splitWords(llibre).filter(w => !textSplit.contains(w)).map(w => (w,1)).groupBy(_._1).map {
    _._2.reduce({ (a, b) => (a._1, a._2 + b._2) })
    }.toList.sortBy(_._2).reverse

      /*println("Num de Paraules: " + ocurrencies.length + "  Diferents: " + prova.length)
      println("Paraules             ocurrencies             frequencia")
      println("---------------------------------------------------------")
      for (x <- 0 to 9) {
        val frequencia = ((prova(x)._2*100).toDouble/ocurrencies.length)
        println(prova(x)._1 + "      " + prova(x)._2 + "      " + f"$frequencia%1.2f")
      }*/


  }
  def paraulafreqfreq(Llibre: String) {
    val frequens = freq(Llibre).groupBy(_._2).mapValues(_.size).toList.sortBy(_._2).reverse
    println("Les 10 paraules mes frequens")
    for (x <- 0 to 9) {
      println(frequens(x)._2 + " paraules apareixen " + frequens(x)._1+" vegades")
    }
    val menysfrequens = frequens.reverse
    println("Les 5 paraules menys frequens")
    for (x <- 0 to 4) {
      println(menysfrequens(x)._2 + " paraules apareixen " + menysfrequens(x)._1 + " vegades")
    }
    frequens
  }


  def ngrama(n: Int, llibre: String): List[(String, Int)] ={
    var ocurrencies: List[(String, Int)] = freq(llibre)

    val llistaparaules: List[String] = splitWords(llibre).sliding(n).toList.map(x => x.mkString(" "))
    for (x <- 0 until llistaparaules.length - 1) {
      ocurrencies = (llistaparaules(x), 1) :: ocurrencies
    }
    ocurrencies.groupBy(_._1).map {
      _._2.reduce({ (a, b) => (a._1, a._2 + b._2) })
    }.toList.sortBy(_._2).reverse
  }
  def cosinesim(llibre: String, llibre2: String): Double ={
    var textSplit: Set[String] = new HashSet[String]
    textSplit = splitWords(llibre).toSet
    var textSplit2: Set[String] = new HashSet[String]
    textSplit2 = splitWords(llibre2).toSet
    //crear pesos de les existents dels 2 fitxers
    val auxLlibre1 = nonstopfreq(llibre)
    val aux2llibre1 = auxLlibre1.map(w => (w._1, w._2.toDouble/auxLlibre1.head._2)).toList.sortBy(_._2).reverse
    val auxLlibre2 = nonstopfreq(llibre2)
    val aux2llibre2 = auxLlibre2.map(w => (w._1, w._2.toDouble/auxLlibre2.head._2)).toList.sortBy(_._2).reverse
    //afegir les paraules que no tenen de l'altre fitxer i ordenar per paraula
    val llistallibre1 = (aux2llibre1 ++ aux2llibre2.filter(l2 => !textSplit.contains(l2._1)).map(l2 => (l2._1, 0.0))).toList.sortBy(_._1)
    val llistallibre2 = (aux2llibre2 ++ aux2llibre1.filter(l2 => !textSplit2.contains(l2._1)).map(l2 => (l2._1, 0.0))).toList.sortBy(_._1)


    //funcio de sim(a,b) , sumem les dues llistes i les dividim per la arrel quadrada del sumatori de cada pes al quadrat dels 2 vectors
    val sim = cosine_similarity(llistallibre1, llistallibre2)
    //val sim =llistallibre1.zip(llistallibre2).map((x => x._1._2*x._2._2)).sum/
    //  (sqrt(llistallibre1.map(w => pow(w._2,2)).sum)*sqrt(llistallibre2.map(w => pow(w._2,2)).sum))


    val aux3 = aux2llibre1.length
    val freq1 = auxLlibre1

    sim
  }

  def cosine_similarity(query: List[(String, Double)], doc: List[(String, Double)]): Double = {
    // Returns the cosine similarity between the two vectors

    var dotProduct = 0.0
    var normA = 0.0
    var normB = 0.0

    for ((p1, p2) <- query.zip(doc)) {
      dotProduct += p1._2 * p2._2
      normA += math.pow(p1._2, 2)
      normB += math.pow(p2._2, 2)
    }

    dotProduct / (math.sqrt(normA) * math.sqrt(normB))
  }
main()
};
