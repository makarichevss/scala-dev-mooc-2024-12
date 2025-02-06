package ru.otus.module1.DataCollection1

import scala.util.Random

class BallsExperiment {

	def isFirstBlackSecondWhite: Boolean = {
		val urn: List[Int] = List(1, 1, 1, 0, 0, 0)
		val drawn: List[Int] = Random.shuffle(urn).take(2)
		drawn.contains(1)
	}
}

object BallsTest {
	def main(args: Array[String]): Unit = {
		val count = 1000000
		val listOfExperiments: List[BallsExperiment] = List.fill(count)(new BallsExperiment)
		val countOfExperiments: List[Boolean] = listOfExperiments.map(_.isFirstBlackSecondWhite)
		val countOfPositiveExperiments: Float = countOfExperiments.count(_ == true)
		println(countOfPositiveExperiments / count)
	}
}