object Simil extends App {
import scala.io.Source
  import scala.collection.mutable.ListBuffer
  def main(): Unit ={
    //ubitext = scala.io.StdIn.readLine();
    val text: String = Source.fromFile("C:\\Users\\arisq\\Downloads\\Practica Funcional + Objectes (1a. part)-20221024\\pg11.txt").mkString
    paraulafreqfreq(text)
  }
  def freq(llibre: String): List[(String, Int)] ={

    print("dintre funcio")
    var ocurrencies: List[(String,Int)]= List();
    val llistaparaules: String = llibre.replaceAll("[',\\n\\t\\r.1234567890=¬€~#@|&%$¡¿{}+!ªº*\\():;\"!?_-]"," ");
    val llistaparaules1: String= llistaparaules.replaceAll("(  )"," ");
    val llistaparaules2: Array[String] = llistaparaules1.toLowerCase().split(" ").filter(_.nonEmpty);

    for(x <- 0 to llistaparaules2.length-1){
      ocurrencies = (llistaparaules2(x), 1) :: ocurrencies;
    }
    val prova = ocurrencies.groupBy(_._1).map{ _._2.reduce({ (a,b) => (a._1, a._2+b._2)})}.toList.sortBy(_._2).reverse;

    return prova;
  }

  def nonstopfreq(llibre: String) : List[(String, Int)] ={

    //ubitext = scala.io.StdIn.readLine();
    val text: String = Source.fromFile("C:\\Users\\arisq\\Downloads\\Practica Funcional + Objectes (1a. part)-20221024\\english-stop.txt").mkString.replaceAll("\\n"," ");
    val textSplit: Array[String] = text.toLowerCase().split(" ")
    var ocurrencies: List[(String, Int)] = List();
    var ocurrencies2: List[(String, Int)] = List();
    val llistaparaules: String = llibre.replaceAll("[,\\n\\t\\r.1234567890=¬€~#@|&%$¡¿{}+!'ªº*\\():;\"!?_-]", " ");

    val llistaparaules1: String = llistaparaules.replaceAll("(  )", " ");
    val llistaparaules2: Array[String] = llistaparaules1.toLowerCase().split(" ").filter(_.nonEmpty);

    for (x <- 0 to llistaparaules2.length - 1) {
      if(textSplit.contains(llistaparaules2(x))){
      }else{
        ocurrencies = (llistaparaules2(x), 1) :: ocurrencies;
      }
    }
    val prova = ocurrencies.groupBy(_._1).map {
      _._2.reduce({ (a, b) => (a._1, a._2 + b._2) })
    }.toList.sortBy(_._2).reverse;

    println("Num de Paraules: " + llistaparaules2.length + "  Diferents: " + prova.length)
    println("Paraules             ocurrencies             frequencia")
    println("---------------------------------------------------------")
    for(x <- 0 to 9){
      println(prova(x)._1 +"      "+prova(x)._2);
    }
    return prova;
  }
  def paraulafreqfreq(Llibre: String) {
    val frequens = freq(Llibre).groupBy(_._2).mapValues(_.size).toList.sortBy(_._2).reverse;
    println("Les 10 paraules mes frequens")
    for (x <- 0 to 9) {
      println(frequens(x)._2 + " paraules apareixen " + frequens(x)._1+" vegades");
    }
    val menysfrequens = frequens.reverse;
    println("Les 5 paraules menys frequens")
    for (x <- 0 to 4) {
      println(menysfrequens(x)._2 + " paraules apareixen " + menysfrequens(x)._1 + " vegades");
    }
    return frequens;
  }
main()
}
