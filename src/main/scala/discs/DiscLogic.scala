package discs

import scala.annotation.tailrec

object DiscLogic {
  // look at the 100 points - which has the largest radius? (check the other points and the existing discs)
  // make that point into a disc and add it to list of Discs
  // now, remove that point from the list of points and do it again
  // do this until we have made 50 discs
  def workOutAnswer(points: List[Point]): List[Disc] = {
    points.take(50).foldRight[List[Disc]](Nil){ (_, acc) =>
      getPointWithLargestRadius(points, acc) match {
        case Some(disc) => disc :: acc
        case None => acc
      }
    }
    // zero value can be anything. it can be a tuple. name it by using case after the { and then naming your acc as appropriate
    // acc could be case class

//    @tailrec
//    def loop(points: List[Point], discs: List[Disc]): List[Disc] = {
//      if (discs.length >= 50) discs else {
//        getPointWithLargestRadius(points, discs) match {
//          case Some(disc) => loop(points.filterNot(_ == disc.centre), disc :: discs)
//          case None => discs
//        }
//      }
//    }
//    loop(points, Nil)
  }

  // check the point's distance to other points and distance to Discs
  // make it the acc if it's greater than the current acc
  def getPointWithLargestRadius(points: List[Point], discs: List[Disc]): Option[Disc] = {
    points.foldRight[Option[Disc]](None){ (head, acc) =>
      acc match {
        case Some(disc) =>
          val radiusOfHeadPoint: Double = maxRadius(head, points, discs)
          val radiusOfAccPoint: Double = maxRadius(disc.centre, points, discs)
          if (radiusOfHeadPoint > radiusOfAccPoint) Some(Disc(head, radiusOfHeadPoint))
          else acc
        case None => Some(Disc(head, maxRadius(head, points, discs)))
      }
    }
  }
  def maxRadius(point: Point, points: List[Point], discs: List[Disc]): Double = {
    val nearestPoint = distanceToNearestPoint(points.filterNot(_ == point), point)
    val nearestDiscPerimeter = distanceToDiscPerimeter(discs, point)
    List(nearestPoint,nearestDiscPerimeter).flatten.min
  }

  // for a given point, find the nearest point. output the distance.
  def distanceToNearestPoint(points: List[Point], point: Point): Option[Double] = {
    points.map { p =>
      math.sqrt(math.pow(point.x - p.x, 2) + math.pow(point.y - p.y, 2))
    } match {
      case Nil => None
      case list => Some(list.min)
    }
  }

  // for a given point, find the nearest disc perimeter. output the distance.
  def distanceToDiscPerimeter(discs: List[Disc], point: Point): Option[Double] = {
    discs.map { disc =>
      math.sqrt(math.pow(point.x - disc.centre.x, 2) + math.pow(point.y - disc.centre.y, 2)) - disc.radius
    } match {
      case Nil => None
      case list => Some(list.min)
    }
  }

  // https://www.google.com/search?q=area+of+circle&oq=area+of+circle
  def areaOfDisc(disc: Disc): Double = {
    math.Pi * math.pow(disc.radius, 2)
  }

  // for a list of points, create the radius for each point and output a list of discs
  //  def getOptimalDiscs(points: List[Point]): List[Disc] = {
  //    points.foldRight[List[Disc]](Nil) { (head, acc) =>
  //      val radius = List(distanceToNearestPoint(points.filterNot(_ == head), head), distanceToDiscPerimeter(acc, head)).flatten.min
  //      Disc(head, radius) :: acc
  //    }
  //  }
  // and recursive option below:
  //    @tailrec
  //    def loop(remainingPoints: List[Point], remainingDiscs: List[Disc]): List[Disc] = remainingPoints match {
  //      case head :: tail =>
  //        // findNearestPoint will never be None in this case, so we can use min safely
  //        val radius = List(distanceToNearestPoint(points.filterNot(_ == head), head), distanceToDiscPerimeter(remainingDiscs, head)).flatten.min
  //        loop(tail, Disc(head, radius) :: remainingDiscs)
  //      case Nil => remainingDiscs
  //    }
  //    loop(points, Nil)
}
