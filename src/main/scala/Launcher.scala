import com.github.tototoshi.csv.CSVReader

import java.io.File
import scala.collection.mutable.ListBuffer


object Launcher {
  def main(args: Array[String]): Unit = {
    println("Hello World!")
    val reader = CSVReader.open(new File("./08-PropertiesLondon.csv"));
    val t = reader.toStream;

    val p = t.toList

    var propertyList = new ListBuffer[Property];
    var propertyIterator = 1;

    while (propertyIterator < p.length) {
      propertyList += (new Property(p(propertyIterator)(0).toInt,p(propertyIterator)(1),p(propertyIterator)(2).toFloat,p(propertyIterator)(3),p(propertyIterator)(4).toInt,p(propertyIterator)(5).toInt,p(propertyIterator)(6).toInt,p(propertyIterator)(7).toInt,p(propertyIterator)(8),p(propertyIterator)(9),p(propertyIterator)(10)));
      propertyIterator = propertyIterator+1;
    }


    def filterByType(propertyType:String, list:ListBuffer[Property]) = list.filter(p => p.propertyType.contains(propertyType))

    def filterByPrice(price:Float, list:ListBuffer[Property]) = list.filter(p => p.price < price)

    var filtered = filterByType("House", propertyList);
    filtered = filterByPrice(475000, filtered);

    println(filtered);


  }
}

case class Property(id:Int,propertyName:String,price:Float,propertyType:String,sqFt:Int,bedrooms:Int,bathrooms:Int,receptions:Int,
                    location:String,city:String,postal:String) extends Address with Luxurious {
  override def toString: String = id.toString+"\t"+propertyName+"\t"+price.toString+"\t"+pricePerSQFt.toString+"\t"+propertyType+"\t"+sqFt.toString+"\t"+bedrooms.toString+"\t"+bathrooms.toString+"\t"+
    receptions.toString+"\t"+location+"\t"+city+"\t"+postal+"\t"+isLuxurious.toString+"\n"
}

trait Address {
  val location:String;
  val city:String;
  val postal:String
}

trait Luxurious {
  val price:Float;
  val sqFt:Int;
  val pricePerSQFt = price/sqFt;
  def isLuxurious = pricePerSQFt > 1000;
}



case class PropertyList() extends ListBuffer[PropertyList] with Filters {

  override val list = this;

}

trait Filters {

  val list:PropertyList;

  def filterByType(propertyType:String, list:ListBuffer[Property]) = list.filter(p => p.propertyType.contains(propertyType))

  def filterByPrice(price:Float, list:ListBuffer[Property]) = list.filter(p => p.price < price)


}
