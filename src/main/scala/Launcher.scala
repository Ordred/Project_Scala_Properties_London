import com.github.tototoshi.csv.CSVReader

import java.io.File
import scala.collection.mutable.ListBuffer


object Launcher {
  def main(args: Array[String]): Unit = {
    println("Hello World!")
    val reader = CSVReader.open(new File("./08-PropertiesLondon.csv"));
    val t = reader.toStream;

    val p = t.toList

    var propertyList = new ListBuffer[Property]
    var propertyIterator = 1;

    while (propertyIterator < p.length) {
      propertyList += (new Property(p(propertyIterator)(0).toInt,p(propertyIterator)(1),p(propertyIterator)(2).toFloat,p(propertyIterator)(3),p(propertyIterator)(4).toInt,p(propertyIterator)(5).toInt,p(propertyIterator)(6).toInt,p(propertyIterator)(7).toInt,p(propertyIterator)(8),p(propertyIterator)(9),p(propertyIterator)(10)));
      propertyIterator = propertyIterator+1;
      propertyList.foreach(p => println(p))
    }
    println()
    println()

    propertyList.filter(p => p.price > 5000000);
  }
}

case class Property(id:Int,propertyName:String,price:Float,propertyType:String,number:Int,bedrooms:Int,bathrooms:Int,receptions:Int,
                    location:String,city:String,postal:String)

trait Address {
  val location:String;
  val city:String;
  val postal: String;
}
