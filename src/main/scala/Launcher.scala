import com.github.tototoshi.csv.CSVReader

import java.io.File

object Launcher {
  def main(args: Array[String]): Unit = {

    val propertyList = init();

    //println(Operations(propertyList).filterByPrice(300000))

    //println(Operations(propertyList).filterByType("Flat"))

    //println(Operations(propertyList).groupByType("House"))

    val filters: Filters = Filters(None ,Some("Apartment"),Some(2400000.0), None, None)

    val filters2: Filters = Filters(Some("Road") ,Some("House"),Some(2400000.0), None, None)

    val filters3: Filters = Filters(None ,Some("Duplex"),Some(2400000.0), None, Some("Holland Park"))

    val sortList: List[String] = List("Bedrooms", "Price", "Name")

    val propertyList2 = init();

    //println(Operations.mainFilter(filters, propertyList2).count(p => true), Operations.mainSort(sortList, Operations.mainFilter(filters, propertyList2), true))

    //println(Operations.mainFilter(Filters(Some("The Pryors"),None,None,None, None),propertyList2));

    //println(Services.buyProperty("Jeff Bezos", Operations.mainFilter(Filters(Some("The Pryors"),None,None,None, None),propertyList2).head))

    //Services.showCredit(propertyList, 100000);

    //Services.comparison(propertyList.head, propertyList(3478))

    Services.rentOrBuy(Operations.mainFilter(Filters(Some("The Pryors"),None,None,None, None),propertyList2).head, 11750)

    Services.monthsUntilAmortized(Operations.mainFilter(Filters(Some("The Pryors"),None,None,None, None),propertyList2).head)

    println(Operations.combineFilteredLists(List(Operations.mainFilter(filters, propertyList2), Operations.mainFilter(filters2, propertyList2), Operations.mainFilter(filters3, propertyList2))))
  }


  def init():List[Property] = {

    val reader = CSVReader.open(new File("./08-PropertiesLondon.csv"));
    val t = reader.toStream;

    val p = t.toList

    p.drop(1).map(p => Property(p(0).toInt,p(1).toString,p(2).toDouble,p(3).toString,p(4).toInt,p(5).toInt,p(6).toInt,p(7).toInt,p(8).toString,p(9).toString,p(10).toString,Some("Penis"), None))

  }
}




