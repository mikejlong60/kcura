import scala.io.Source

object Main extends App {

  val listOfLines = Source.fromFile("./src/main/resources/Sample_Cities.txt").getLines.toList

  val cities = listOfLines.map(line => line.split("\\|")).map(line => CityDemographics(population = line(0), city = line(1), state = line(2), interstates = line(3).split(";").toSet)).toSet
  val chicago = cities.filter(_.city == "Chicago").head.copy(degreesOfConnection = 0)

  val endResult = degreesFrom(Set(chicago), cities - chicago, Vector.empty[Set[CityDemographics]]) :+ Set(chicago)
  println("===============")
  endResult.foreach({
    level => {
      println("length:"+level.size)
      println(level)
    }
  })
  println("++++++++++++++")

  def getDirectConnections(currentLevel: Set[CityDemographics], nextLevelCandidates: Set[CityDemographics]): Set[CityDemographics] = {
   def getDirectConnections(currentLevel: List[CityDemographics], accum: Set[CityDemographics]): Set[CityDemographics] = {
     currentLevel match {
       case city :: Nil => {
         val matches = nextLevelCandidates.filter(candidateCity => candidateCity.interstates.intersect(city.interstates).nonEmpty)
         accum ++ matches
       }
       case city :: cities => {
         val matches = nextLevelCandidates.filter(candidateCity => candidateCity.interstates.intersect(city.interstates).nonEmpty)
         getDirectConnections(cities, accum ++ matches)
       }
       case Nil => accum
     }
   }
    getDirectConnections(currentLevel.toList, Set.empty[CityDemographics])
  }

  def degreesFrom(currentLevel: Set[CityDemographics], candidates: Set[CityDemographics], accum: Vector[Set[CityDemographics]]): Vector[Set[CityDemographics]] = {
    val directConnections = getDirectConnections(currentLevel, candidates -- currentLevel).toList
    directConnections match {
      case Nil => accum.reverse//Add level -1 to the vector
      case _ => {
        val newLevel = getDirectConnections(currentLevel, candidates -- currentLevel)
        degreesFrom(newLevel, candidates -- currentLevel, accum :+ newLevel)
      }
    }
  }
}

case class CityDemographics(population: String, city: String, state: String, interstates: Set[String], degreesOfConnection: Int = -1)
