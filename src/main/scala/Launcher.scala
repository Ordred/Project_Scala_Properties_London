import com.github.tototoshi.csv.CSVReader

import java.io.File


object Launcher {
  def main(args: Array[String]): Unit = {
    println("Hello World!")
    val reader = CSVReader.open(new File("./08-PropertiesLondon.csv"));
    val t = reader.toStream;

    val p = t.toList
    var property = new Property(p(0)(0),p(0)(1),p(0)(2),p(0)(3),p(0)(4),p(0)(5),p(0)(6),p(0)(7),p(0)(8),p(0)(9),p(0)(10));
    var propertyList = ArrayBuffer.empty[Property]
    var propertyIterator = 1;

    println(property)

    while (propertyIterator < p.length) {
      propertyList.addOne(new Property(p(propertyIterator)(0),p(propertyIterator)(1),p(propertyIterator)(2),p(propertyIterator)(3),p(propertyIterator)(4),p(propertyIterator)(5),p(propertyIterator)(6),p(propertyIterator)(7),p(propertyIterator)(8),p(propertyIterator)(9),p(propertyIterator)(10)))
      propertyIterator = propertyIterator+1;
      propertyList.foreach(p => println(p))
    }
  }



}

case class Property(id:String,propertyName:String,price:String,propertyType:String,number:String,bedrooms:String,bathrooms:String,receptions:String,
                    location:String,city:String,postal:String)

//Option(List:)