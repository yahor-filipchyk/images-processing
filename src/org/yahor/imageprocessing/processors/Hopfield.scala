package org.yahor.imageprocessing.processors

import java.awt.Color
import java.awt.image.BufferedImage
import java.util.Random
import ImageProcessor.brightness

class Hopfield() {

  private var shapes: Array[Array[Int]] = null
  private var weight: Array[Array[Int]] = null

  def teach(images: java.util.List[BufferedImage]) = {
    println(images, images.size())
    val height = images.get(0).getHeight
    val width = images.get(0).getWidth
    shapes = new Array(images.size())
    for (i <- 0 until shapes.length) {
      shapes(i) = new Array[Int](height * width)
    }

    weight = new Array(height * width)
    for (i <- 0 until weight.length) {
      weight(i) = new Array[Int](height * width)
    }

    for (i <- 0 until images.size();
         image = images.get(i);
         j <- 0 until image.getHeight;
         k <- 0 until image.getWidth) {
      val br = brightness(images.get(i).getRGB(j, k))
      if (br == 5 || br == 255) shapes(i)(k + j * height) = -1
      else shapes(i)(k + j * height) = 1
    }

    initWeights()
  }

  def initWeights() = {
    for (k <- 0 until shapes.length;
         i <- 0 until shapes(k).length;
         j <- 0 until shapes(k).length) {
      if (i != j) weight(i)(j) += shapes(k)(i) * shapes(k)(j)
      else weight(i)(j) = 0
    }
  }

  def detect(image: BufferedImage): BufferedImage = {
    val heigth = image.getHeight
    val width = image.getWidth
    val imageVector = Hopfield.imageToVector(image, heigth, width)

    val newVec = new Array[Int](heigth * width)

    var finished = false
    val random = new Random()

    while (!finished) {
      for (i <- 0 until heigth * width;
           j <- 0 until heigth * width) {
        newVec(i) += weight(i)(j) * imageVector(j)
      }

      val neuron = random.nextInt(heigth * width)
      if (newVec(neuron) > 0)
        imageVector(neuron) = 1
      else
        imageVector(neuron) = -1

      for (i <- 0 until heigth * width) {
        if (newVec(i) > 0)
          newVec(i) = 1
        else
          newVec(i) = -1
      }

      finished = true
      for (i <- 0 until heigth * width) {
        if (imageVector(i) != newVec(i))
          finished = false
        newVec(i) = imageVector(i)
      }
    }

    Hopfield.vectorToImage(imageVector, heigth, width)
  }
}

object Hopfield {

  def imageToVector(image: BufferedImage, height: Int, width: Int): Array[Int] = {
    val vector = new Array[Int](width * height)
    for (i <- 0 until height;
         j <- 0 until width) {
      val br = brightness(image.getRGB(i, j))
//      println(br)
      if (br == 5 || br == 255) vector(j + i * height) = -1
      else vector(j + i * height) = 1
    }
    vector
  }

  def vectorToImage(vector: Array[Int], height: Int, width: Int): BufferedImage = {
    val image = new BufferedImage(width, height, BufferedImage.TYPE_BYTE_BINARY)
    for (i <- 0 until width;
         j <- 0 until height) {
      if (vector(j + i * height) == 1) image.setRGB(i, j, Color.BLACK.getRGB)
      else image.setRGB(i, j, Color.WHITE.getRGB)
    }
    image
  }

  def printVector(vector: Array[Int], height: Int, width: Int) =
    for (i <- 0 until vector.length) {
      print((if (vector(i) == -1) " " else "  ") + vector(i))
      if ((i + 1) % width == 0) println()
    }

  def noise(image: BufferedImage, percentage: Int) = {
    val heigth = image.getHeight
    val width = image.getWidth
    val imageVector = imageToVector(image, heigth, width)
    printVector(imageVector, heigth, width)

    val totalPixels = heigth * width
    val pixels = totalPixels * percentage / 100

    var indexes = List[Int]()
    val random = new Random()
    for (i <- 0 until pixels) {
      var index = random.nextInt(totalPixels)
      while (indexes contains index) {
        index = random.nextInt(totalPixels)
      }
      indexes = index::indexes
    }

    for (index <- indexes) {
      if (imageVector(index) == -1) imageVector(index) = 1
      else imageVector(index) = -1
    }

    vectorToImage(imageVector, heigth, width)
  }
}
