
import com.github.tototoshi.csv.CSVReader

import java.io.File
import scala.collection.mutable.ListBuffer
import scala.math.BigDecimal.double2bigDecimal


object Launcher {

  def init():List[Property] = {

    val reader = CSVReader.open(new File("./08-PropertiesLondon.csv"));
    val t = reader.toStream;

    val p = t.toList

    p.drop(1).map(p => Property(p(0).toInt,p(1).toString,p(2).toDouble,p(3).toString,p(4).toInt,p(5).toInt,p(6).toInt,p(7).toInt,p(8).toString,p(9).toString,p(10).toString,Some("Penis"), None))

  }

  def main(args: Array[String]): Unit = {

    val propertyList = init();

    //println(Operations(propertyList).filterByPrice(300000))

    //println(Operations(propertyList).filterByType("Flat"))

    //println(Operations(propertyList).groupByType("House"))

    val filters: Filters = Filters(None ,Some("Apartment"),Some(2400000.0), None, None)

    val sortList: List[String] = List("Bedrooms", "Price", "Name")

    val propertyList2 = init();

    println(mainFilter(filters, propertyList2).count(p => true), mainSort(sortList, mainFilter(filters, propertyList2), true))

    println(mainFilter(Filters(Some("The Pryors"),None,None,None, None),propertyList2));

    println(buyProperty("Jeff Bezos", mainFilter(Filters(Some("The Pryors"),None,None,None, None),propertyList2).head))

    showCredit(propertyList, 100000);

    comparison(propertyList.head, propertyList(3478))



  }

  def comparison(property1:Property,property2:Property): Unit = {
    val priceDifference = property1.price - property2.price;
    val sqFTDifference = property1.sqFt - property2.sqFt;
    val pricePerSqFTDifference = property1.pricePerSQFt - property2.pricePerSQFt;
    val bedroomDifference = property1.bedrooms - property2.bedrooms;
    val bathroomDifference = property1.bathrooms - property2.bathrooms;
    val receptionDifference = property1.receptions - property2.receptions

    def priceDiffKW:String = {
      if (priceDifference > 0) {
        "higher"
      }
      else {
        "lower"
      }
    }

    def sqFTDiffKW:String = {
      if (sqFTDifference > 0) {
        "bigger"
      }
      else {
        "smaller"
      }
    }

    def pricePerSqFTDiffKW:String = {
      if (pricePerSqFTDifference > 0) {
        "higher"
      }
      else {
        "lower"
      }
    }

    def bedroomDiffKW:String = {
      if (bedroomDifference > 0) {
        "more"
      }
      else {
        "less"
      }
    }

    def bathroomDiffKW:String = {
      if (bathroomDifference > 0) {
        "more"
      }
      else {
        "less"
      }
    }

    def receptionDiffKW:String = {
      if (receptionDifference > 0) {
        "more"
      }
      else {
        "less"
      }
    }

    val priceDiffString = property1.propertyName+"s price is "+Math.abs(priceDifference)+" $ "+priceDiffKW+" than "+property2.propertyName
    val sqFTDiffString = property1.propertyName+" has "+Math.abs(sqFTDifference)+" "+sqFTDiffKW+" sqFT  "+property2.propertyName
    val pricePerSqFTDiffString = property1.propertyName+"s price per sqFT is "+Math.floor(Math.abs(pricePerSqFTDifference))+" $ "+pricePerSqFTDiffKW+" than "+property2.propertyName
    val bedroomDiffString = property1.propertyName+" has "+Math.abs(bedroomDifference)+" bedrooms "+bedroomDiffKW+" than "+property2.propertyName
    val bathroomDiffString = property1.propertyName+" has "+Math.abs(bathroomDifference)+" bathrooms "+bathroomDiffKW+" than "+property2.propertyName
    val receptionDiffString = property1.propertyName+" has "+Math.abs(receptionDifference)+" receptions "+receptionDiffKW+" than "+property2.propertyName

    println(priceDiffString)
    println(sqFTDiffString)
    println(pricePerSqFTDiffString)
    println(bedroomDiffString)
    println(bathroomDiffString)
    println(receptionDiffString)
  }

  def showCredit(propertyList:List[Property], income:Double):Unit = {
    propertyList.map(p => print(p+"Interest Rate: "+(p.price/(income*10))+"%"+"\n"+"Term: "+(p.price/(income/3)/12).ceil+" years"+"\n"+"Decision: "+(((p.price/(income/3)/12).ceil) < 10 ).toString+"\n"));
  }

  def mainSort(sortByList: List[String], propertyList: List[Property], firstIteration: Boolean): List[Property] ={
    def sortByName(sortNameList: List[Property]): List[Property] = sortNameList.sortBy(p => p.propertyName)
    def sortByType(sortTypeList: List[Property]): List[Property] = sortTypeList.sortBy(p => p.propertyType)
    def sortByPrice(sortPriceList: List[Property]): List[Property] = sortPriceList.sortBy(p => p.price)
    def sortByBedrooms(sortBedroomsList: List[Property]): List[Property] = sortBedroomsList.sortBy(p => p.bedrooms)

    val tempSortByList: List[String] = if(firstIteration){
      sortByList.reverse;
    }else{
      sortByList;
    }

    val tempList: List[Property] = tempSortByList.head match{
      case "Name" => sortByName(propertyList)
      case "Type" => sortByType(propertyList)
      case "Price" => sortByPrice(propertyList)
      case "Bedrooms" => sortByBedrooms(propertyList)
    }

    if (tempSortByList.length > 1){
      mainSort(tempSortByList.slice(1,tempSortByList.length), tempList, false)
    }else{
      tempList
    }
  }

  def mainFilter(filters: Filters, list: List[Property]): List[Property] ={
    def filterByName(propertyName:String, nameInputList: List[Property]):  Option[List[Property]] = Some(nameInputList.filter(p => p.propertyName.contains(propertyName)))
    def filterByType(propertyType:String, typeInputList: List[Property]): Option[List[Property]] = Some(typeInputList.filter(p => p.propertyType.contains(propertyType)))
    def filterByPrice(price:Double, priceInputList: List[Property]): Option[List[Property]] = Some(priceInputList.filter(p => p.price < price))
    def filerByLuxurious(luxInputList: List[Property]): Option[List[Property]] = Some(luxInputList.filter(p => p.isLuxurious))
    def filterByCity(city:String, cityInputList: List[Property]): Option[List[Property]] = Some(cityInputList.filter(p => p.city.contains(city)))

    val nameFilterList: Option[List[Property]] = {
      if(filters.NameFilter.isDefined){
        filterByName(filters.NameFilter.get, list)
      }else{
        Some(list)
      }
    }

    val typeFilterList: Option[List[Property]] = {
      if(filters.TypeFilter.isDefined){
        filterByType(filters.TypeFilter.get, nameFilterList.get)
      }else{
        Some(nameFilterList.get)
      }
    }

    val priceFilterList: Option[List[Property]] = {
      if(filters.PriceFilter.isDefined){
        filterByPrice(filters.PriceFilter.get, typeFilterList.get)
      }else{
        Some(typeFilterList.get)
      }
    }

    val luxFilterList: Option[List[Property]] = {
      if(filters.NameFilter.isDefined){
        filerByLuxurious(priceFilterList.get)
      }else{
        Some(priceFilterList.get)
      }
    }

    val cityFilterList: Option[List[Property]] = {
      if(filters.CityFilter.isDefined){
        filterByCity(filters.CityFilter.get, luxFilterList.get)
      }else{
        Some(luxFilterList.get)
      }
    }

    cityFilterList.get
  }

  def groupBy(propertyList: List[Property], CriteriaType: String, Criteria: String): Group ={
    def avgPrice(list: List[Property]): Double ={
      list.map(_.price).sum / list.length
    }

    def avgPricePerSQFt(list: List[Property]): Double ={
      list.map(_.pricePerSQFt).sum / list.length
    }

    val tempList: List[Property] = CriteriaType match{
      case "Type" => mainFilter(Filters(None, Some(Criteria), None, None, None), propertyList )
      case "City" => mainFilter(Filters(None, None, None, None,  Some(Criteria)), propertyList )
    }
    Group("Group by: " + Criteria, tempList.count(p => true), tempList.map(_.price).sum, avgPrice(tempList), avgPricePerSQFt(tempList))
  }

  def buyProperty(buyer:String, property:Property):Property = {
    return Property(property.id,property.propertyName,property.price,property.propertyType, property.sqFt, property.bedrooms, property.bathrooms, property.receptions, property.location,property.city,property.postal,Some(buyer), None)
  }
}

case class BoughtProperty(buyer:String,property: Property)

case class BoughtPropertyList(buyer:String,propertyList:List[Property])

case class Property(id:Int,propertyName:String,price:Double,propertyType:String,sqFt:Int,bedrooms:Int,bathrooms:Int,receptions:Int,
                    location:String,city:String,postal:String,owner:Option[String], income:Option[Double]) extends Address with Luxurious with Interest {
  override def toString: String = id.toString+"\t"+propertyName+"\t"+price.toString+"\t"+pricePerSQFt.toString+"\t"+propertyType+"\t"+sqFt.toString+"\t"+bedrooms.toString+"\t"+bathrooms.toString+"\t"+
    receptions.toString+"\t"+location+"\t"+city+"\t"+postal+"\t"+isLuxurious.toString+"\t"+owner+"\t"+interestRate+"\n"

}

case class Group(propertyType:String,count:Int,total:Double,averagePrice:Double,averagePriceSQFt:Double)


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

trait Interest {
  val price:Double;
  val income:Option[Double];
  def interestRate = {if (income != None) price/(income.get*1000) else 0};
}


case class Filters(NameFilter: Option[String],TypeFilter: Option[String],PriceFilter: Option[Double],LuxuriousFilter: Option[Boolean], CityFilter: Option[String])



case class Operations(list:List[Property]) {

  def filterByName(propertyName:String): List[Property] = list.filter(p => p.propertyName.contains(propertyName))

  def filterByType(propertyType:String) = list.filter(p => p.propertyType.contains(propertyType))

  def filterByPrice(price:Float) = list.filter(p => p.price < price)

  def addProperty(id:Int,propertyName:String,price:Double,propertyType:String,sqFt:Int,bedrooms:Int,bathrooms:Int,receptions:Int,
                  location:String,city:String,postal:String):List[Property] = list.appended(new Property(id,propertyName,price,propertyType,sqFt,bedrooms,bathrooms,receptions,location,city,postal,None, None))

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


