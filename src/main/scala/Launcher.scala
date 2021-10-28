import com.github.tototoshi.csv.CSVReader

import java.io.File
import scala.language.implicitConversions

object Launcher {
  def main(args: Array[String]): Unit = {
    println("Hello World!")
    val reader = CSVReader.open(new File("./08-PropertiesLondon.csv"));
    val t = reader.toStream;

    val p = t.toList

    val propertyList = toPropertyList(p);
    var propertyIterator = 1;

    while (propertyIterator < p.length) {
      propertyList.appended(Property(p(propertyIterator)(0).toInt,p(propertyIterator)(1),p(propertyIterator)(2).toFloat,p(propertyIterator)(3),p(propertyIterator)(4).toInt,p(propertyIterator)(5).toInt,p(propertyIterator)(6).toInt,p(propertyIterator)(7).toInt,p(propertyIterator)(8),p(propertyIterator)(9),p(propertyIterator)(10)));
      propertyIterator = propertyIterator+1;
    }

  def toPropertyList(list: List[List[String]]){
    val newList = List.empty;

    list.map(
      // todo
    )

    return newList;
  }

    def filterByType(propertyType:String, list:List[Property]) = list.filter(p => p.propertyType.contains(propertyType))

    def filterByPrice(price:Float, list:List[Property]) = list.filter(p => p.price < price)

    val filtered = filterByType("House", propertyList);
    val filtered2 = filterByPrice(475000, filtered);

    println(filtered2);

    println(Operations(propertyList).recursiveSearch("Newlands", 0))

    filtered.map(p => p.price * 0.8)

    println(filtered2);

    filtered.map(e => e.propertyName.match{
      case String => "hallo"
      case _ => "bye"
    })

    println()
    println()

    println(propertyList.last);

    println()

    println(Operations(propertyList).addProperty(propertyList.length,"Name",18273.23,"House",999,23,23,23,"London","London","3930").last)

    println()
    println()
    println(Operations(propertyList).groupByType("Flat"))
  }
}

case class Property(id:Int,propertyName:String,price:Double,propertyType:String,sqFt:Int,bedrooms:Int,bathrooms:Int,receptions:Int,
                    location:String,city:String,postal:String) extends Address with Luxurious {
  override def toString: String = id.toString+"\t"+propertyName+"\t"+price.toString+"\t"+pricePerSQFt.toString+"\t"+propertyType+"\t"+sqFt.toString+"\t"+bedrooms.toString+"\t"+bathrooms.toString+"\t"+
    receptions.toString+"\t"+location+"\t"+city+"\t"+postal+"\t"+isLuxurious.toString+"\n"

}

case class Group(propertyType:String,count:Int,total:Double,averagePrice:Double,averagePriceSQFt:Double);

trait Address {
  val location:String;
  val city:String;
  val postal:String
}

trait Luxurious {
  val price:Double;
  val sqFt:Int;
  val pricePerSQFt = price/sqFt;
  def isLuxurious = pricePerSQFt > 1000;
}



case class PropertyList() extends List[PropertyList]{

   val list = this;

}

trait Filters[T](filterCriterion: T, propertyList: List[Property]) extends Enumeration {

  def filterByType(propertyType: T, list:List[Property]) = list.filter(p => p.propertyType.contains(propertyType))
  def filterByPrice(price: T, list:List[Property]) = list.filter(p => p.price < price)
  def filterByIsLuxurious(list: List[Property]) = list.filter(p => p.isLuxurious)


  val propertyType    = filterByType(filterCriterion, propertyList)
  val propertyPrice   = filterByPrice(filterCriterion, propertyList)
  val isLuxourious    = filterByIsLuxurious(propertyList)
}

case class Operations(list:List[Property]) {

  def filterByName(propertyName:String): List[Property] = list.filter(p => p.propertyName.contains(propertyName))

  def filterByType(propertyType:String) = list.filter(p => p.propertyType.contains(propertyType))

  def filterByPrice(price:Float) = list.filter(p => p.price < price)

  def addProperty(id:Int,propertyName:String,price:Double,propertyType:String,sqFt:Int,bedrooms:Int,bathrooms:Int,receptions:Int,
                  location:String,city:String,postal:String):List[Property] = list.appended(new Property(id,propertyName,price,propertyType,sqFt,bedrooms,bathrooms,receptions,location,city,postal))

  def recursiveSearch(searchName:String, iterator:Int): Property = {
    if (list(iterator).propertyName.contains(searchName)) {
      return list(iterator)
    }
    else {
      var iterator2 = iterator+1;
      recursiveSearch(searchName, iterator2);
    }
  }

  def groupByType(propertyType:String):Group = {

    val filteredList = list.filter(p => p.propertyType.contains(propertyType));
    var total:Double = 0;
    var averageSQ:Double = 0;
    filteredList.foreach(p => {
      total += p.price
      averageSQ = averageSQ + p.pricePerSQFt
    })

    return new Group(propertyType,filteredList.length, total,total/filteredList.length,averageSQ/filteredList.length)

  }




}


