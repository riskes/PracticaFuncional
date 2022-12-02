import java.util
import scala.collection.immutable.HashSet
import scala.collection.{MapView, mutable}

object Simil extends App {
import scala.io.Source
  import scala.collection.mutable.ListBuffer

  def main(): List[(String, Int)] = {
    //ubitext = scala.io.StdIn.readLine();
    val text: String = Source.fromFile("C:\\Users\\arisq\\Downloads\\Practica Funcional + Objectes (1a. part)-20221024\\pg11.txt").mkString
    nonstopfreq(text)
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
main()
};
