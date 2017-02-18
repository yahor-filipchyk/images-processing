package org.yahor.imageprocessing.processors

import java.awt.{Graphics2D, Color}
import java.awt.image.BufferedImage
import ImageProcessor.gaussianFilter
import ImageProcessor.copyImage

object BlobsDetector {

  def showBlobs(image: BufferedImage, sigmaRatio: Float): BufferedImage = {
    val startingTime = System.currentTimeMillis()
    val blobs = BlobsDetector.blobDoG(image, sigmaRatio = sigmaRatio)
    val processedImage = copyImage(image, BufferedImage.TYPE_3BYTE_BGR)
    val g = processedImage.getGraphics.asInstanceOf[Graphics2D]
    g.setPaint(Color.red)
    g.setColor(Color.red)
    for (blob <- blobs; if blob._3 > 0) {
      val radius = (math.sqrt(2) * blob._3).asInstanceOf[Int]
      g.drawOval(blob._1 - radius, blob._2 - radius, radius * 2, radius * 2)
    }
    g.dispose()
    println((System.currentTimeMillis() - startingTime) / 1000 + " secs")
    processedImage
  }

  def blobDoG(image: BufferedImage, minSigma: Float = 0.25F, maxSigma: Int = 40, sigmaRatio: Float = 1.6F): List[(Int, Int, Float)] = {
    val k: Int = logOfBase(sigmaRatio, maxSigma.asInstanceOf[Double] / minSigma).asInstanceOf[Int] + 1
    val sigmaList = for (i <- 0 to k) yield (minSigma * math.pow(sigmaRatio, i)).asInstanceOf[Float]
    println(sigmaList)
    val gaussianImages = for (sigma <- sigmaList) yield gaussianFilter(image, sigma)
    val dogImages = for (i <- 0 until k) yield subtractImages(gaussianImages(i), gaussianImages(i + 1), sigmaList(i))
    deleteOverlapped(getBlobs(dogImages, sigmaList))
  }

  def getBlobs(dogImages: IndexedSeq[BufferedImage], sigmaList: IndexedSeq[Float]): List[(Int, Int, Float)] = {
    var blobs = List[(Int, Int, Float)]()
    for (i <- 0 until dogImages.length - 1) {
      val dogImage = dogImages(i)
      val lowerNeighbor = if (i > 0) dogImages(i - 1) else null
      val upperNeighbor = dogImages(i + 1)
      for (y <- 3 until dogImage.getHeight - 3;
           x <- 3 until dogImage.getWidth - 3) {
        val pointValue = dogImage.getRGB(x, y)
        if (isMax(pointValue, dogImage, lowerNeighbor, upperNeighbor, x, y)) {
          blobs =  (x, y, sigmaList(i)) :: blobs
        }
      }
    }
    blobs
  }

  def isMax(pointValue: Int, dogImage: BufferedImage, lowerNeighbor: BufferedImage, upperNeighbor: BufferedImage, x: Int, y: Int): Boolean = {
    val expected =  if (lowerNeighbor != null) 26 else 17
    var greaterThanCount = expected
    for (i <- 0 until 3;
         j <- 0 until 3) {
      if (lowerNeighbor != null) {
        val candidateLower = lowerNeighbor.getRGB(x + (j - 1), y + (i - 1))
        if (candidateLower <= pointValue) {
          greaterThanCount -= 1
        }
      }
      val candidateUpper = upperNeighbor.getRGB(x + (j - 1), y + (i - 1))
      if (candidateUpper <= pointValue) {
        greaterThanCount -= 1
      }
    }

    for (i <- 0 until 3; if i != 1;
         j <- 0 until 3; if j != 1) {
      val candidate = dogImage.getRGB(x + (j - 1), y + (i - 1))
      if (candidate <= pointValue) {
        greaterThanCount -= 1
      }
    }

    greaterThanCount == expected
  }

  def subtractImages(image1: BufferedImage, image2: BufferedImage, sigma: Float): BufferedImage = {
    val result = new BufferedImage(image2.getWidth, image2.getHeight, BufferedImage.TYPE_BYTE_GRAY)
    for (y <- 0 until image2.getHeight; x <- 0 until image2.getWidth) {
      result.setRGB(x, y, ((image2.getRGB(x, y) - image1.getRGB(x, y)) * sigma).asInstanceOf[Int])
    }
    result
  }

  def deleteOverlapped(blobs: List[(Int, Int, Float)]): List[(Int, Int, Float)] = {
    val resultingBlobs = blobs.toArray
    for (i <- 0 until blobs.size; blob = blobs(i)) {
      for (j <- 0 until blobs.size; anotherBlob = blobs(j); if blob != anotherBlob) {
        if (overlaps(blob, anotherBlob) > 0.4) {
          if (blob._3 > anotherBlob._3) resultingBlobs(j) = (0, 0, 0.0F)
          else resultingBlobs(i) = (0, 0, 0.0F)
        }
      }
    }
    resultingBlobs.toList
  }

  def overlaps(blob1: (Int, Int, Float), blob2: (Int, Int, Float)): Double = {
    val r1 = blob1._3 * math.sqrt(2)
    val r2 = blob2._3 * math.sqrt(2)

    val xDelta = blob1._1 - blob2._1
    val yDelta = blob1._2 - blob2._2
    val d = xDelta * xDelta + yDelta * yDelta

    if (d > r1 + r2) 0
    else if (d <= math.abs(r1 - r2)) 1
    else d / (r1 + r2)
  }

  def logOfBase(base: Double, num: Double) = math.log(num) / math.log(base)

}
