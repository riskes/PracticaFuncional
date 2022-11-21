object Simil extends App {
import scala.io.Source
  def main(): Unit ={
    val text: String = Source.fromFile("C:\\Users\\arisq\\Downloads\\Practica Funcional + Objectes (1a. part)-20221024\\pg11.txt").mkString
    freq(text)
  }
  def freq(llibre: String){

    print("dintre funcio")
    var ocurrencies: List[(String,Int)]= List();
    val llistaparaules: String = llibre.replaceAll("[',\\n\\t\\r.1234567890=¬€~#@|&%$¡¿{}+!ªº*\\():;\"!?_-]"," ");
    val llistaparaules1: String= llistaparaules.replaceAll("(  )"," ");
    val llistaparaules2: Array[String] = llistaparaules1.toLowerCase().split(" ").filter(_.nonEmpty);

    for(x <- 0 to llistaparaules2.length-1){
      ocurrencies = (llistaparaules2(x), 1) :: ocurrencies;
    }
    val prova = ocurrencies.groupBy(_._1).map{ _._2.reduce({ (a,b) => (a._1, a._2+b._2)})}.toList.sortBy(_._2);
    return prova;
  }

  def nonstopfreq(llibre: String) {

    print("dintre funcio")
    var ocurrencies: List[(String, Int)] = List();
    val llistaparaules: String = llibre.replaceAll("[',\\n\\t\\r.1234567890=¬€~#@|&%$¡¿{}+!ªº*\\():;\"!?_-]", " ");
    val llistaparaules1: String = llistaparaules.replaceAll("(  )", " ");
    val llistaparaules2: Array[String] = llistaparaules1.toLowerCase().split(" ").filter(_.nonEmpty);

    for (x <- 0 to llistaparaules2.length - 1) {
      ocurrencies = (llistaparaules2(x), 1) :: ocurrencies;
    }
    val prova = ocurrencies.groupBy(_._1).map {
      _._2.reduce({ (a, b) => (a._1, a._2 + b._2) })
    }.toList.sortBy(_._2);
    return prova;
  }
main()
}
