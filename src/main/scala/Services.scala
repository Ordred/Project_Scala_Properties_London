object Services {
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

  def buyProperty(buyer:String, property:Property):Property = {
    Property(property.id,property.propertyName,property.price,property.propertyType, property.sqFt, property.bedrooms, property.bathrooms, property.receptions, property.location,property.city,property.postal,Some(buyer), None)
  }

  def showRent(propertyList: List[Property]): Unit ={
    propertyList.map(p => print(p+"Calculated Rent: "+(calculateRent(p))+"£"+"\n"))
  }

  def calculateRent(property: Property): Double ={
    property.price*.007
  }

  def rentOrBuy(property: Property, income: Double): Unit ={
    val rent: Double = calculateRent(property)
    val monthlyCost: Double = property.price*0.005+1000
    val decision:String = {
        if(rent<monthlyCost) {
          "rented"
        }else{
          "bought"
        }
    }
    if(rent<income){
      if(monthlyCost<income){
        println(property+"With an Income of " + income + " this Property should be " + decision )
      }else{
        println(property+"With an Income of " + income + " this Property should be rented" )
      }
    }else{
      if(monthlyCost<income){
        println(property+"With an Income of " + income + " this Property should be bought" )
      }else{
        println(property+"With an Income of " + income + " this Property should not be acquired" )
      }
    }
  }

  def monthsUntilAmortized(property: Property): Unit ={
    println("It would take " + (property.price/calculateRent(property)).ceil.toInt + " months of renting out the property \"" + property.propertyName + "\" until it is amortized")
  }
}

case class BoughtProperty(buyer:String,property: Property)

case class BoughtPropertyList(buyer:String,propertyList:List[Property])
