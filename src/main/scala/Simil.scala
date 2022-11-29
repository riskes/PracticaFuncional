import scala.collection.MapView

object Simil extends App {
import scala.io.Source
  import scala.collection.mutable.ListBuffer
  def main(): Unit ={
    //ubitext = scala.io.StdIn.readLine();
    val text: String = Source.fromFile("C:\\Users\\arisq\\Downloads\\Practica Funcional + Objectes (1a. part)-20221024\\pg11.txt").mkString
    freq(text)
  }

  def splitWords(llibre: String): Array[String] = {
    val llistaparaules: String = llibre.replaceAll("[\\W\\r\\n\\t\\d_]", " ")
    val llistaparaules1: String = llistaparaules.replaceAll("( +)", " ")
    return llistaparaules1.toLowerCase().split(" ").filter(_.nonEmpty)
  }
  def freq(llibre: String): List[(String, Int)] ={

    print("dintre funcio")
    var ocurrencies: List[(String,Int)]= List()
    val llistaparaules: Array[String] = splitWords(llibre)

    for(x <- 0 to llistaparaules.length-1){
      ocurrencies = (llistaparaules(x), 1) :: ocurrencies
    }
    val prova = ocurrencies.groupBy(_._1).map{ _._2.reduce({ (a,b) => (a._1, a._2+b._2)})}.toList.sortBy(_._2).reverse
    print("ey")
    return prova
  }

  def nonstopfreq(llibre: String) : List[(String, Int)] ={

    //ubitext = scala.io.StdIn.readLine();
    val text: String = Source.fromFile("C:\\Users\\arisq\\Downloads\\Practica Funcional + Objectes (1a. part)-20221024\\english-stop.txt").mkString
    val textSplit: Array[String] = splitWords(text)
    var ocurrencies: List[(String, Int)] = List()
    val llistaparaules: Array[String] = splitWords(llibre)

    for (x <- 0 to llistaparaules.length - 1) {
      if(textSplit.contains(llistaparaules(x))){
      }else{
    }
    val prova = ocurrencies.groupBy(_._1).map {
      _._2.reduce({ (a, b) => (a._1, a._2 + b._2) })
    }.toList.sortBy(_._2)

    println("Num de Paraules: " + llistaparaules.length + "  Diferents: " + prova.length)
    println("Paraules             ocurrencies             frequencia")
    println("---------------------------------------------------------")
    for(x <- 0 to 9){
      println(prova(x)._1 +"      "+prova(x)._2)
    }
    return prova
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
    return frequens
  }
  def ngrama(n: Int, llibre: String): Unit ={
    print("dintre funcio")
    var ocurrencies: List[(String, Int)] = List()

    val llistaparaules2: List[Array[String]] = splitWords(llibre).sliding(3).toList

    val ngramWithCount = llistaparaules2.groupBy(identity).mapValues(_.size)

    val sumIndex = ngramWithCount.groupBy { case (k, v) => k.take(n - 1) }

    /*val ngramWithProbabilityFaster = ngramWithCount.map { case (k, v) =>
      (k, v.toDouble / sumIndex(k.take(n - 1)))
    }*/
    //val sumIndex = ngramWithCount.groupBy { case (k, v) => k.take(n - 1) }.mapValues(_.values.sum)
    //val ngramWithProbabilityFaster = ngramWithCount.map { case (k, v) => (k, v.toDouble / sumIndex(k.take(n - 1))) }
    //val sumIndex: MapView[Array[String], Int] = llistaparaules2.groupBy { case (k, v) => k.take(n - 1) }.mapValues(_.)
    println("gdas")
    /*val ngramWithProbabilityFaster = llistaparaules2.map { case (k, v) =>
      (k, v.toDouble / sumIndex(k.take(n - 1)))
    }*/
    //val llistaparaules2: Iterator[Array[String]] = llistaparaules1.split(" ").sliding(2).filter(_.nonEmpty)

    println("hola")
  }
main()
}
