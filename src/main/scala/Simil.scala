import scala.collection.MapView

object Simil extends App {
import scala.io.Source
  import scala.collection.mutable.ListBuffer

  def main(): List[(String, Int)] = {
    //ubitext = scala.io.StdIn.readLine();
    val text: String = Source.fromFile("C:\\Users\\arisq\\Downloads\\Practica Funcional + Objectes (1a. part)-20221024\\pg11.txt").mkString
    ngrama(3,text)

  }

  def splitWords(llibre: String): Array[String] = {
    val llistaparaules: String = llibre.replaceAll("[\\W\\r\\n\\t\\d_]", " ")
    val llistaparaules1: String = llistaparaules.replaceAll("( +)", " ")
    return llistaparaules1.toLowerCase().split(" ").filter(_.nonEmpty)
  }
  def freq(llibre: String): List[(String, Int)] ={

    var ocurrencies: List[(String,Int)]= List()
    val llistaparaules: Array[String] = splitWords(llibre)

    for(x <- 0 to llistaparaules.length-1){
      ocurrencies = (llistaparaules(x), 1) :: ocurrencies
    }
    val prova: List[(String, Int)] = ocurrencies.groupBy(_._1).map{ _._2.reduce({ (a,b) => (a._1, a._2+b._2)})}.toList.sortBy(_._2).reverse
    prova
  }

  def nonstopfreq(llibre: String) : List[(String, Int)] = {

    //ubitext = scala.io.StdIn.readLine();
    val text: String = Source.fromFile("C:\\Users\\arisq\\Downloads\\Practica Funcional + Objectes (1a. part)-20221024\\english-stop.txt").mkString
    val textSplit: Array[String] = splitWords(text)
    var ocurrencies: List[(String, Int)] = List()
    val llistaparaules: Array[String] = splitWords(llibre)

    for (x <- 0 to llistaparaules.length - 1) {
      if (textSplit.contains(llistaparaules(x))) {
      } else {
        ocurrencies = (llistaparaules(x), 1) :: ocurrencies
      }
    }
      val prova: List[(String, Int)] = ocurrencies.groupBy(_._1).map {
        _._2.reduce({ (a, b) => (a._1, a._2 + b._2) })
      }.toList.sortBy(_._2).reverse

      println("Num de Paraules: " + ocurrencies.length + "  Diferents: " + prova.length)
      println("Paraules             ocurrencies             frequencia")
      println("---------------------------------------------------------")
      for (x <- 0 to 9) {
        val frequencia = ((prova(x)._2*100).toDouble/ocurrencies.length)
        println(prova(x)._1 + "      " + prova(x)._2 + "      " + f"$frequencia%1.2f")
      }
      prova

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
    print("dintre funcio")
    var ocurrencies: List[(String, Int)] = List()

    val llistaparaules: List[String] = splitWords(llibre).sliding(3).toList.map((x) => x.mkString(" "))
    for (x <- 0 to llistaparaules.length - 1) {
      ocurrencies = (llistaparaules(x), 1) :: ocurrencies
    }
    val prova: List[(String, Int)] = ocurrencies.groupBy(_._1).map {
      _._2.reduce({ (a, b) => (a._1, a._2 + b._2) })
    }.toList.sortBy(_._2).reverse
    print("ey")
    prova
  }
main()
};
