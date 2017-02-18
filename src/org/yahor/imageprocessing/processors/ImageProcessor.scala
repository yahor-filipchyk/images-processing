package org.yahor.imageprocessing.processors

import java.awt.{Graphics2D, Color}
import java.awt.image.{Kernel, ConvolveOp, ColorModel, BufferedImage}
import java.util

import scala.collection.mutable
import scala.collection.immutable
import scala.util.Random

object ImageProcessor {

  val brightness = brightnessCCIR601 _

  def getHistogram(image: BufferedImage): util.Map[Integer, Integer] = {
    require(image != null, "image == null")
    val histogram = new util.HashMap[Integer, Integer](255)
    for (y <- 0 until image.getHeight;
         x <- 0 until image.getWidth) {
      val pointBrightness = brightness(image.getRGB(x, y))
      histogram.put(pointBrightness,
        if (histogram.containsKey(pointBrightness)) histogram.get(pointBrightness) + 1 else 0)
    }
    histogram
  }

  /**
   * http://stackoverflow.com/questions/596216/formula-to-determine-brightness-of-rgb-color
   * @param rgb
   * @return
   */
  def brightnessITU_R(rgb: Int): Int = {
    val point = new Color(rgb)
    (0.2126 * point.getRed + 0.7152 * point.getGreen + 0.0722 * point.getBlue).asInstanceOf[Int]
  }

  /**
   * gives more weight to the R and B components
   * @param rgb
   * @return
   */
  def brightnessCCIR601(rgb: Int): Int = {
    val point = new Color(rgb)
    (0.299 * point.getRed + 0.587 * point.getGreen + 0.114 * point.getBlue).asInstanceOf[Int]
  }

  def g(br: Int, fmin: Int, fmax: Int, gmin: Int, gmax: Int): Int = {
    if (br <= fmin)
      gmin
    else if (br >= fmax)
      gmax
    else
      br
  }

  def prepareDecrease(image: BufferedImage, gmax: Int): BufferedImage = {
    val gDecrease = g (_: Int, 255 - gmax, 255, 0, gmax)
    prepare(image, gDecrease)
  }

  def prepareIncrease(image: BufferedImage, fmax: Int): BufferedImage = {
    val gIncrease = g (_: Int, 0, fmax, 255 - fmax, 255)
    prepare(image, gIncrease)
  }

  def prepare(image: BufferedImage, gFunc: Int => Int): BufferedImage = {
    val preparedImage = new BufferedImage(image.getWidth, image.getHeight, BufferedImage.TYPE_BYTE_GRAY)
    for (y <- 0 until image.getHeight;
         x <- 0 until image.getWidth) {
      val pointBrightness = brightness(image.getRGB(x, y))
      val newBrightness = gFunc(pointBrightness)
      preparedImage.setRGB(x, y, grayRGB(newBrightness))
    }
    preparedImage
  }

  def rgb(r: Int, g: Int, b: Int) = {
    val R = (r << 16) & 0x00FF0000
    val G = (g << 8) & 0x0000FF00
    val B = b & 0x000000FF
    0xFF000000 | R | G | B
  }

  def grayRGB(gray: Int) = rgb(gray, gray, gray)

  def filter(plainImage: BufferedImage, processedImage: BufferedImage,
             filterFunc: (Int, Int, BufferedImage) => Int): BufferedImage = {
    for (y <- 1 until plainImage.getHeight - 1;
         x <- 1 until plainImage.getWidth - 1) {
      processedImage.setRGB(x, y, grayRGB(filterFunc(x, y, plainImage)))
    }
    processedImage
  }

  def prewittFilter(image: BufferedImage): BufferedImage = {
    val filteredImage = new BufferedImage(image.getWidth, image.getHeight, BufferedImage.TYPE_BYTE_GRAY)
    filter(image, filteredImage, prewittFilter)
  }

  def lowBrightnessFilter(image: BufferedImage): BufferedImage = {
    val filteredImage = new BufferedImage(image.getWidth, image.getHeight, BufferedImage.TYPE_BYTE_GRAY)
    filter(image, filteredImage, lowBrightnessFilter)
  }

  def toBlackWhite(image: BufferedImage, threshold: Int): BufferedImage = {
    val processedImage = new BufferedImage(image.getWidth, image.getHeight, BufferedImage.TYPE_BYTE_BINARY)
    implicit val fmax = threshold
    filter(image, processedImage, toBlackWhite)
  }

  def copyImage(image: BufferedImage, imageType: Int) = {
    val newImage = new BufferedImage(image.getWidth, image.getHeight, imageType)
    for (y <- 0 until image.getHeight; x <- 0 until image.getWidth) newImage.setRGB(x, y, image.getRGB(x, y))
    newImage
  }

  def extendImage(image: BufferedImage, imageType: Int, pixelsCount: Int) = {
    val newImage = new BufferedImage(image.getWidth + pixelsCount * 2, image.getHeight + pixelsCount * 2, imageType)
  }

  def gaussianFilter(image: BufferedImage, sigma: Float): BufferedImage = {
    val filteredImage = new BufferedImage(image.getWidth, image.getHeight, BufferedImage.TYPE_BYTE_GRAY)
    new ConvolveOp(getGaussKernel(sigma)).filter(image, filteredImage)
  }

  def getGaussKernel(sigma: Float): Kernel = {
    val radius = (4 * sigma + 0.5).asInstanceOf[Int]
    val size = radius * 2 + 1
    val kernel = new Array[Float](size * size)
    var counter = 0
    for (y <- -radius to radius;
         x <- -radius to radius) {
      val gaussCell = gaussianFilter(x, y, sigma)
      kernel(counter) = gaussCell
      counter += 1
    }
    new Kernel(size, size, kernel)
  }

//  val k = math.pow(2, 0.5)

  def gaussianFilter(x: Int, y: Int, sigma: Float): Float = {
    val squireSigma = sigma * sigma
    ((1.0 / (2.0 * math.Pi * squireSigma))
      * math.pow(math.E, -1 * (x * x + y * y) / (2.0 * squireSigma))).asInstanceOf[Float]
  }

  def showLabels(image: BufferedImage): BufferedImage = {
    val labels = labeling(image)
//    println("Created")
    val graphics = image.getGraphics.asInstanceOf[Graphics2D]

    graphics.setPaint(Color.red)
    graphics.setColor(Color.red)
    for ((area, points) <- labels._2) {
      graphics.drawString(String.valueOf(area), points.head._1, points.head._2)
    }
    graphics.dispose()
    image
  }

  val h1 = Array(Array(-1, 0, 1),
                 Array(-1, 0, 1),
                 Array(-1, 0, 1))
  val h2 = Array(Array(-1, -1, -1),
                 Array( 0,  0,  0),
                 Array( 1,  1,  1))

  def prewittFilter(x: Int, y: Int, image: BufferedImage): Int = {
    var p: Int = 0
    var q: Int = 0
    for (i <- 0 until 3;
         j <- 0 until 3) {
      val pointBrightness = brightness(image.getRGB(x + (j - 1), y + (i - 1)))
      p += pointBrightness * h1(i)(j)
      q += pointBrightness * h2(i)(j)
    }
    math.max(math.abs(p), math.abs(q))
  }

  val H1 = (1.0 / 9.0,
            Array(Array(1, 1, 1),
                  Array(1, 1, 1),
                  Array(1, 1, 1)))

  val H2 = (0.1,
            Array(Array(1, 1, 1),
                  Array(1, 2, 1),
                  Array(1, 1, 1)))

  val H3 = (1.0 / 16.0,
            Array(Array(1, 2, 1),
                  Array(2, 4, 2),
                  Array(1, 2, 1)))

  def lowBrightnessFilter(x: Int, y: Int, image: BufferedImage): Int = {
    var h1: Double = 0
    var h2: Double = 0
    var h3: Double = 0
    for (i <- 0 until 3;
         j <- 0 until 3) {
      val pointBrightness = brightness(image.getRGB(x + (j - 1), y + (i - 1)))
      h1 += pointBrightness * H1._2(i)(j)
      h2 += pointBrightness * H2._2(i)(j)
      h3 += pointBrightness * H3._2(i)(j)
    }
    h1 *= H1._1
    h2 *= H2._1
    h3 *= H3._1
    math.max(math.max(math.abs(h1), math.abs(h2)), math.abs(h3)).asInstanceOf[Int]
  }

  def blackWhite(brightness: Int, fmax: Int) = g(brightness, fmax, fmax, 0, 255)

  def toBlackWhite(x: Int, y: Int, image: BufferedImage)(implicit fmax: Int) = {
    blackWhite(brightness(image.getRGB(x, y)), fmax)
  }

  def labeling(image: BufferedImage): (Array[Array[Int]], mutable.HashMap[Int, mutable.Set[(Int, Int)]],
    mutable.HashMap[Int, immutable.Vector[Double]]) = {
    val labels = new Array[Array[Int]](image.getHeight)
    for (i <- 0 until labels.length) labels(i) = new Array[Int](image.getWidth)
    val equivalentLabels = new mutable.ArrayBuffer[mutable.Set[Int]]()
    var label = 1
    for (y <- 1 until image.getHeight;
         x <- 1 until image.getWidth) {
      if (brightness(image.getRGB(x, y)) != 0) {
        brightness(image.getRGB(x, y))
        val b = labels(y)(x - 1)
        val c = labels(y - 1)(x)
        if (b == 0 && c == 0) {
          label += 1
          labels(y)(x) = label
        } else if (b != 0 && c == 0) labels(y)(x) = b
        else if (b == 0 && c != 0) labels(y)(x) = c
        else if (b == c) labels(y)(x) = b
        else if (b != c) {
          labels(y)(x) = b
          setEquality(equivalentLabels, b, c)
//          println(b, c)
        }
      }
    }
//    equivalentLabels foreach println
    reviseLabels(labels, equivalentLabels)
//    println("Creating cache")
    val labelsMap = new mutable.HashMap[Int, mutable.Set[(Int, Int)]]()
    for (y <- 0 until labels.length;
         x <- 0 until labels(y).length; labelValue = labels(y)(x); point = (x, y); if labelValue != 0) {
      if (!(labelsMap contains labelValue)) labelsMap += (labelValue -> mutable.Set(point))
      else labelsMap(labelValue) += point
    }
    val dimensions = new mutable.HashMap[Int, immutable.Vector[Double]]()
    for ((lbl, points) <- labelsMap) {
      val dims = getDimentions(labels, lbl)
      if (dims.forall(value => !value.isNaN && !value.isInfinity))
        dimensions += (lbl -> getDimentions(labels, lbl))
    }
    (labels, labelsMap, dimensions)
  }

  def getDimentions(labels: Array[Array[Int]], areaNumber: Int) = immutable.Vector(
//    area(labels, areaNumber),
//    perimeter(labels, areaNumber),
    density(labels, areaNumber),
    elongation(labels, areaNumber)
  )

  def reviseLabels(labels: Array[Array[Int]], equivalentLabels: mutable.ArrayBuffer[mutable.Set[Int]]): Unit = {
    val withMinLabel = for (labelGroup <- equivalentLabels) yield labelGroup.min -> labelGroup
    for (y <- 0 until labels.length;
         x <- 0 until labels(y).length) {
      for (labelGroup <- withMinLabel; if labels(y)(x) != 0 && (labelGroup._2 contains labels(y)(x))) {
        labels(y)(x) = labelGroup._1
      }
    }
  }

  def area(labels: Array[Array[Int]], areaLabel: Int): Int = {
    var pointsCount = 0
    for (row <- labels) {
      for (label <- row; if label == areaLabel) pointsCount += 1
    }
    pointsCount
  }

  def massCenter(labels: Array[Array[Int]], areaLabel: Int): (Int, Int) = {
    var xCenter = 0
    var yCenter = 0
    for (y <- 0 until labels.length;
         x <- 0 until labels(y).length; if labels(y)(x) == areaLabel) {
      xCenter += x
      yCenter += y
    }
    val space = area(labels, areaLabel)
    (xCenter / space, yCenter / space)
  }

  def perimeter(labels: Array[Array[Int]], areaLabel: Int): Int = {
    var per = 0
    for (y <- 0 until labels.length;
         x <- 0 until labels(y).length; if labels(y)(x) == areaLabel) {
      if (isBorderPoint(labels, x, y)) per += 1
    }
    per
  }

  def density(labels: Array[Array[Int]], areaLabel: Int): Double =
    math.pow(perimeter(labels, areaLabel), 2) / area(labels, areaLabel)

  def centralMoment(labels: Array[Array[Int]], i: Int, j: Int, areaLabel: Int): Double = {
    var m = 0.0
    val massCent = massCenter(labels, areaLabel)
    for (y <- 0 until labels.length;
         x <- 0 until labels(y).length; if labels(y)(x) == areaLabel) {
      m += Math.pow(x - massCent._1, i) * Math.pow(y - massCent._2, j)
    }
    m
  }

  def elongation(labels: Array[Array[Int]], areaLabel: Int): Double = {
    val moment20 = centralMoment(labels, 2, 0, areaLabel)
    val moment02 = centralMoment(labels, 0, 2, areaLabel)
    val moment11 = centralMoment(labels, 1, 1, areaLabel)
    val left = moment20 +  moment02
    val right = Math.pow(Math.pow(moment20 - moment02, 2) + 4 * Math.pow(moment11, 2), 0.5)
    (left + right) / (left - right)
  }

  private def isBorderPoint(labels: Array[Array[Int]], x: Int, y: Int): Boolean = {
    var borderPoint = false
    val label = labels(y)(x)
    for (i <- 0 until 3;
         j <- 0 until 3) {
      if (labels(y + (i - 1))(x + (j - 1)) != label) borderPoint = true
    }
    borderPoint
  }

  private def setEquality(equivalentLabels: mutable.ArrayBuffer[mutable.Set[Int]], b: Int, c: Int) = {
    var wasAdded = false
    var toRemove = -1
    for (label <- equivalentLabels) {
      if (label contains b) {
        label += c
        wasAdded = true
        for (i <- 0 until equivalentLabels.length; l = equivalentLabels(i); if (l contains c) && !(l contains b)) {
          label ++= l
          toRemove = i
        }
      }
    }
    if (toRemove != -1) equivalentLabels.remove(toRemove)
    toRemove = -1
    for (label <- equivalentLabels) {
      if (label contains c) {
        label += b
        wasAdded = true
        for (i <- 0 until equivalentLabels.length; l = equivalentLabels(i); if (l contains b) && !(l contains c)) {
          label ++= l
          toRemove = i
        }
      }
    }
    if (toRemove != -1) equivalentLabels.remove(toRemove)
    //    for (label <- equivalentLabels) {
    //      if (label contains c) {
    //        label += b
    //        wasAdded = true
    //      }
    //    }
    if (!wasAdded) equivalentLabels += mutable.Set(b, c)
  }
}