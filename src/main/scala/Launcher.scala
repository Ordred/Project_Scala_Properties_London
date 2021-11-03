import com.github.tototoshi.csv.CSVReader

import java.io.File
import java.lang.System.console
import java.util.concurrent.TimeUnit
import scala.concurrent
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.{Duration, DurationInt}
import scala.util.{Failure, Success, Try}

object Launcher {

  def main(args: Array[String]): Unit = {
    val filters: Filters = Filters(None ,Some("Apartment"),Some(2400000.0), None, None)

    val filters2: Filters = Filters(Some("Road") ,Some("House"),Some(2400000.0), None, None)

    val filters3: Filters = Filters(None ,Some("Duplex"),Some(2400000.0), None, Some("Holland Park"))

    val sortList: List[String] = List("Bedrooms", "Price", "Name")

    val propertyList = init();

    println("=============================================================================================")
    println("mainFilter test")
    println("=============================================================================================")

    println(Operations.mainFilter(Filters(Some("The Pryors"),None,None,None, None),propertyList));

    println("=============================================================================================")
    println("mainSort test")
    println("=============================================================================================")

    println(Operations.mainFilter(filters, propertyList).count(p => true), Operations.mainSort(sortList, Operations.mainFilter(filters, propertyList), true))

    println("=============================================================================================")
    println("buyProperty test")
    println("=============================================================================================")

    println(Services.buyProperty("Jeff Bezos", Operations.mainFilter(Filters(Some("The Pryors"),None,None,None, None),propertyList).head))

    println("=============================================================================================")
    println("showCredit test")
    println("=============================================================================================")

    Services.showCredit(propertyList, 100000);

    println("=============================================================================================")
    println("comparison test")
    println("=============================================================================================")

    Services.comparison(propertyList.head, propertyList(3478))

    println("=============================================================================================")
    println("rentOrBuy test")
    println("=============================================================================================")

    Services.rentOrBuy(Operations.mainFilter(Filters(Some("The Pryors"),None,None,None, None),propertyList).head, 11750)

    println("=============================================================================================")
    println("monthsUntilAmortized test")
    println("=============================================================================================")

    Services.monthsUntilAmortized(Operations.mainFilter(Filters(Some("The Pryors"),None,None,None, None),propertyList).head)

    println("=============================================================================================")
    println("combineFilteredLists test")
    println("=============================================================================================")

    // creates future for the function combineFilteredLists
    val future = Future(Operations.combineFilteredLists(List(Operations.mainFilter(filters, propertyList), Operations.mainFilter(filters2, propertyList), Operations.mainFilter(filters3, propertyList))))

    // retreives the result of the future of the function combineFilteredLists
    val resultFuture = Await.result(future, 2.seconds)

    // prints the result of the future of the function combineFilteredLists
    println(resultFuture)
  }

  def init():List[Property] = {

    val reader = try {
      CSVReader.open(new File("./08-PropertiesLondon.csv"))
    }catch {
      case ex:Exception => println("Unable to read file"+ ex.getMessage)
      null
    }

    val t = reader.toStream;

    val p = t.toList

    p.drop(1).map(p => Property(p(0).toInt,p(1).toString,p(2).toDouble,p(3).toString,p(4).toInt,p(5).toInt,p(6).toInt,p(7).toInt,p(8).toString,p(9).toString,p(10).toString,None))

  }

}




