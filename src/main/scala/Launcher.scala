import com.github.tototoshi.csv.CSVReader

import java.io.File


object Launcher {
  def main(args: Array[String]): Unit = {
    println("Hello World!")
    val reader = CSVReader.open(new File("C:/Users/Jonathan/IdeaProjects/Project_Scala_Properties_London/08-PropertiesLondon.csv"));
    var element = reader.readNext();
    while (element != None) {
      println(element);
      element = reader.readNext();
    }
  }

}
