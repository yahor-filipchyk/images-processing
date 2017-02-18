package org.yahor.imageprocessing.processors

import java.awt.image.BufferedImage
import ImageProcessor._

import scala.collection.{mutable, immutable}
import scala.util.Random

object Clustering {

  def classify(image: BufferedImage, classes: Int): BufferedImage = {
    val processedImage = copyImage(image, BufferedImage.TYPE_3BYTE_BGR)
    println("Executing k-medoids...")
    kMedoids(processedImage, classes)
    println("Clustering completed")
    processedImage
  }

  def kMedoids(image: BufferedImage, classesCount: Int): BufferedImage = {
    val labels = labeling(image)
    val labelsMap = labels._2
    val dimensions = labels._3
    println("Initialising medoids...")
    dimensions foreach println
    val centers = initCenters(Nil, classesCount, dimensions).toArray
    centers foreach println
    val initClustering = clusterize(dimensions, centers)
    println("Objects are classified first time")
    var clusters = initClustering._1
    var totalCost = initClustering._2
    var prevCenters: Array[(Int, immutable.Vector[Double])] = Array()
    println("Staring revising configuration...")
    while (prevCenters.toList != centers.toList) {
      prevCenters.foreach(println(_))
      centers.foreach(println(_))
      println("--------------------")
      prevCenters = centers.clone()
      for (i <- 0 until centers.length; center = centers(i);
           point <- clusters(i); if point != center) {
        val nextConfiguration = centers.clone()
        nextConfiguration(i) = point
        val costOfConfiguration = computeTotalCost(nextConfiguration, clusters)
        if (costOfConfiguration < totalCost) {
          totalCost = costOfConfiguration
          centers(i) = point
        }
      }
      val newClustering = clusterize(dimensions, centers)
      clusters = newClustering._1
      totalCost = newClustering._2
    }
    for (c <- 0 until clusters.length;
         (label, dim) <- clusters(c);
         color = rgb(((c * 10 + 23) + 11) * 7 % 255, ((c * 10 + 87) + 51) * 3 % 255, ((c * 10 + 11) + 123) * 5 % 255)) {
      labelsMap(label).foreach(
        p => image.setRGB(p._1, p._2, color))
    }
    image
  }

  def clusterize(labels: mutable.Map[Int, immutable.Vector[Double]], centers: Array[(Int, immutable.Vector[Double])]) = {
    val clusters = new Array[List[(Int, immutable.Vector[Double])]](centers.length).map(_ => List[(Int, immutable.Vector[Double])]())
    var totalCost: Double = 0.0
    for ((label, dimensions) <- labels) {
      val clusterNumber = cluster(dimensions, centers)
      clusters(clusterNumber) = (label -> dimensions) :: clusters(clusterNumber)
      totalCost += distance(dimensions, centers(clusterNumber)._2)
      //      println(dimensions, centers(clusterNumber)._2)
      //      println(totalCost)
    }
    (clusters, totalCost)
  }

  def computeTotalCost(centers: Array[(Int, immutable.Vector[Double])], clusters: Array[List[(Int, immutable.Vector[Double])]]) = {
    var totalCost: Double = 0.0
    for (i <- 0 until clusters.length;
         point <- clusters(i)) {
      totalCost += distance(point._2, centers(i)._2)
    }
    totalCost
  }

  def cost(point1: (Int, Int), point2: (Int, Int)) = math.abs(point1._1 - point2._1) + math.abs(point1._2 - point2._2)

  def distance(point1: immutable.Vector[Double], point2: immutable.Vector[Double]): Double = euclidianDistance(point1, point2)

  def euclidianDistance(point1: immutable.Vector[Double], point2: immutable.Vector[Double]): Double =
    math.sqrt((point1 zip point2).foldRight(0.0)((dims, res) => math.pow(dims._1 - dims._2, 2) + res))

  def manhattanDistance(point1: immutable.Vector[Double], point2: immutable.Vector[Double]): Double =
    math.abs((point1 zip point2).foldRight(0.0)((dims, res) => dims._1 - dims._2 + res))

  def cluster(point: immutable.Vector[Double], centers: Array[(Int, immutable.Vector[Double])]) =
    centers.indexOf(centers reduce ((p1, p2) => {
      val d1 = distance(point, p1._2)
      val d2 = distance(point, p2._2)
      if (d1 < d2) p1 else p2
    }))

  def initCenters(medoids: List[(Int, immutable.Vector[Double])], classesCount: Int,
                  labels: mutable.Map[Int, immutable.Vector[Double]]): List[(Int, immutable.Vector[Double])]= {
    if (medoids.size == classesCount)
      medoids
    else {
      val rand = new Random()
      val index = rand.nextInt(labels.keySet.size)
      val candidate: Int = labels.keySet.toArray.apply(index)
      val point = (candidate, labels(candidate))
      medoids match {
        case List() => initCenters(point :: medoids, classesCount, labels)
        case head::rest =>
          if (!(medoids contains point))
            initCenters(point :: medoids, classesCount, labels)
          else
            initCenters(medoids, classesCount, labels)
      }
    }
  }

  def distance(point1: (Int, Int), point2: (Int, Int)) = math.sqrt(math.pow(point1._1 - point2._1, 2)
    + math.pow(point1._2 - point2._2, 2))

}
