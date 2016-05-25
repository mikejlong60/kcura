import scala.annotation.tailrec
import scala.io.Source

object Main extends App {

  val listOfLines = Source.fromFile("./src/main/resources/Sample_Cities.txt").getLines.toList

  val cities = listOfLines.map(line => line.split("\\|")).map(line => CityDemographics(population = line(0), city = line(1), state = line(2), interstates = line(3).split(";").toSet)).toSet
  val chicago = cities.filter(_.city == "Chicago")
  val endResult = degreesFrom(chicago, cities)// :+ Set(chicago)

  val finalResult = endResult.flatten
//  import scala.util.Sorting
//  val pairs = Array(("a", 5, 2), ("c", 3, 1), ("b", 1, 3))
//
//  // sort by 2nd element
//  Sorting.quickSort(finalResult)(Ordering[CityDemographics].on(x => ))
//
//  // sort by the 3rd element, then 1st
//  Sorting.quickSort(pairs)(Ordering[(Int, String)].on(x => (x._3, x._1)))
//  Sorting.quickSort(finalResult)(Ordering[(String)].on(x => (x._3, x._1)))
  println("===============")
  endResult.foreach({
    level => {
      println("length:"+level.size)
      println(level)
    }
  })
  println("++++++++++++++")


  def getDirectConnections(currentCities: Set[CityDemographics], nextLevelCandidates: Set[CityDemographics], degreeOfConnection: Int): Set[CityDemographics] = {
    @tailrec
    def getDirectConnections(currentCities: List[CityDemographics], accum: Set[CityDemographics]): Set[CityDemographics] = {
      def matchesForCity(city: CityDemographics): Set[CityDemographics] =
        nextLevelCandidates.filter(candidateCity => candidateCity.interstates.intersect(city.interstates).nonEmpty)

      currentCities match {
       case city :: Nil => {
         accum ++ matchesForCity(city)
       }
       case city :: cities => {
         getDirectConnections(cities, accum ++ matchesForCity(city))
       }
       case Nil => accum
     }
   }
    getDirectConnections(currentCities.toList, Set.empty[CityDemographics])
  }

  def degreesFrom(startingLevel: Set[CityDemographics], candidates: Set[CityDemographics]): Vector[Set[CityDemographics]] = {
    @tailrec
    def degreesFrom(currentLevel: Set[CityDemographics], candidates: Set[CityDemographics], accum: Vector[Set[CityDemographics]], degreeOfConnection: Int): Vector[Set[CityDemographics]] = {
      val directConnections = getDirectConnections(currentLevel, candidates -- currentLevel, degreeOfConnection)
      if (directConnections.isEmpty)
        accum
      else
        degreesFrom(directConnections, candidates -- currentLevel, accum :+ directConnections, degreeOfConnection + 1)
    }
    degreesFrom(startingLevel, candidates -- startingLevel, Vector.empty[Set[CityDemographics]], 0)
  }
}

case class CityDemographics(degreeOfConnection: Int = -1, city: String, state: String, interstates: Set[String], population: String)// extends Ordered[CityDemographics] {
//  def compare(that: CityDemographics) =  this.degreesOfConnection - that.degreesOfConnection && this.state > that.state
//}
