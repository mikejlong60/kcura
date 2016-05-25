import java.io.{File, PrintWriter}

import scala.annotation.tailrec
import scala.io.Source

object Main extends App {

  val listOfLines = Source.fromFile("./src/main/resources/Sample_Cities.txt").getLines.toList

  val cities = listOfLines.map(line => line.split("\\|")).map(line => CityDemographics(population = line(0), city = line(1), state = line(2), interstates = line(3).split(";").toSet)).toSet
  val chicago = cities.filter(_.city == "Chicago")
  val endResult = degreesFrom(chicago, cities).flatten

  val writer = new PrintWriter(new File("test.txt" ))

  endResult.foreach { line => writer.write(s"${line.degreeOfConnection} ${line.city},${line.state}\n")}
  writer.close()


  def getDirectConnections(currentCities: Set[CityDemographics], nextLevelCandidates: Set[CityDemographics], degreeOfConnection: Int): Set[CityDemographics] = {
    @tailrec
    def getDirectConnections(currentCities: List[CityDemographics], accum: Set[CityDemographics]): Set[CityDemographics] = {
      def matchesForCity(city: CityDemographics): Set[CityDemographics] =
        nextLevelCandidates.filter(candidateCity => candidateCity.interstates.intersect(city.interstates).nonEmpty)

      currentCities match {
        case city :: Nil => accum ++ matchesForCity(city)
        case city :: cities => getDirectConnections(cities, accum ++ matchesForCity(city))
        case Nil => accum
      }
    }
    getDirectConnections(currentCities.toList, Set.empty[CityDemographics])
  }

  def degreesFrom(startingLevel: Set[CityDemographics], candidates: Set[CityDemographics]): Vector[Set[CityDemographics]] = {
    def addLevels(linkedLevels: Vector[Set[CityDemographics]]) =
      linkedLevels.foldLeft(Vector.empty[Set[CityDemographics]])((allCities, cities) => allCities :+ cities.map(city => city.copy(degreeOfConnection = allCities.length)))

    @tailrec
    def degreesFrom(currentLevel: Set[CityDemographics], candidates: Set[CityDemographics], accum: Vector[Set[CityDemographics]], degreeOfConnection: Int): Vector[Set[CityDemographics]] = {
      val directConnections = getDirectConnections(currentLevel, candidates -- currentLevel, degreeOfConnection)
      if (directConnections.isEmpty) {
        addLevels(accum).+:(candidates -- currentLevel).reverse

      } else
        degreesFrom(directConnections, candidates -- currentLevel, accum :+ directConnections, degreeOfConnection + 1)
    }
    degreesFrom(startingLevel, candidates -- startingLevel, Vector(startingLevel), 0)
  }
}

case class CityDemographics(degreeOfConnection: Int = -1, city: String, state: String, interstates: Set[String], population: String)
