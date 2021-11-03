import java.util.concurrent.TimeUnit
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration.{Duration, durationToPair}

object Operations {
  // Takes the List of attributes to sort by, the list of properties and wether it is
  // the first iteration of the recursion or not as parameters
  // Sorted by the reversed sortByList, so that the sorting goes in the order of the list with the
  // priority going from left to right
  // returns the sorted List
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

  // takes a Filter object,that consists of Filter type options, and the list to filter as parameters
  // and applies the filter for all attributes of the Filter object that contain some()
  // returns the filtered List
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

  // takes the property List, the Column, which will be grouped, and the selected group as parameters
  // returns the selected group with some additional information as a Group object
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
    Group("Group by: " + Criteria, tempList.count(p => true), tempList.map(_.price).sum, avgPrice(tempList), avgPricePerSQFt(tempList), tempList)
  }

  // takes a list of property Lists as the parameter and combines them into one property list
  // returns the combined list with the duplicates removed
  def combineFilteredLists(propertyListList: List[List[Property]]): List[Property] ={
    propertyListList.flatten.distinct
  }
}

case class Filters(NameFilter: Option[String],TypeFilter: Option[String],PriceFilter: Option[Double],LuxuriousFilter: Option[Boolean], CityFilter: Option[String])

