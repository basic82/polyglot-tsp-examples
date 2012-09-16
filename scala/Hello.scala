package com.google.gwt.sample.jribble.client;

import com.google.gwt.core.client.EntryPoint;
import com.google.gwt.user.client.ui.Label;
import com.google.gwt.user.client.ui.RootPanel;


abstract class CityGraph [T: Numeric] {
  val numCities: Int
  def length(from: Int, to: Int): T
}

/* A contrived and highly regular problem involving N cities numbered
   0..N-1. Length of road linking consecutively numbered cities is
   1. All other edges in the graph have length equal to the shortest
   sequential path between the vertices. Shortest tours visit the
   cities in ascending or descending numerical order, modulo
   N. Optimum is N.
 */
class RingedCities (
    override val numCities: Int
  ) extends CityGraph[Double] {
  def length(from: Int, to: Int): Double = {
    val lo = from min to
    val hi = from max to
    (hi - lo) min (lo+numCities - hi)
  }
}

/* Definition of gr17 problem. Optimum is 2085. Source:
   http://www2.iwr.uni-heidelberg.de/groups/comopt/software/TSPLIB95/tsp/gr17.tsp.gz 
 */
object Gr17 extends CityGraph[Int] {
  override val numCities = 17
  def length(from: Int, to: Int): Int =
    weights((from max to) - 1)(from min to)
  val weights =
    // Rows in the lower triangle of the distance matrix.
    Vector(Vector(633),
	   Vector(257, 390),
	   Vector(91, 661, 228),
	   Vector(412, 227, 169, 383),
	   Vector(150, 488, 112, 120, 267),
	   Vector(80, 572, 196, 77, 351, 63),
	   Vector(134, 530, 154, 105, 309, 34, 29),
	   Vector(259, 555, 372, 175, 338, 264, 232, 249),
	   Vector(505, 289, 262, 476, 196, 360, 444, 402, 495),
	   Vector(353, 282, 110, 324, 61, 208, 292, 250, 352, 154),
	   Vector(324, 638, 437, 240, 421, 329, 297, 314, 95, 578, 435),
	   Vector(70, 567, 191, 27, 346, 83, 47, 68, 189, 439, 287, 254),
	   Vector(211, 466, 74, 182, 243, 105, 150, 108, 326, 336, 184, 391, 145),
	   Vector(268, 420, 53, 239, 199, 123, 207, 165, 383, 240, 140, 448, 202, 57),
	   Vector(246, 745, 472, 237, 528, 364, 332, 349, 202, 685, 542, 157, 289, 426, 483),
	   Vector(121, 518, 142, 84, 297, 35, 29, 36, 236, 390, 238, 301, 55, 96, 153, 336))
}

/* Class of Hamiltonian cycles through a given graph. Instance is
   initialized to a given sequence containing a permutation of the
   city ID numbers 0..N-1.
 */
abstract class Tour [T: Numeric] (
    cities: CityGraph[T], 
    val path: Seq[Int]	// Keep it to facilitate printing later.
  ) {
  require(cities.numCities == path.length)
  val distance = {
    val numeric = implicitly [Numeric[T]]
    path.foldLeft(numeric.zero, path.last) {
      case ((total, src), dest) => 
	(numeric.plus(total, cities.length(src, dest)),
	 dest)
    } _1
  }
}

class RandomTour [T: Numeric] (
    cities: CityGraph[T], rng: util.Random
  ) extends Tour[T](cities, 
		    rng.shuffle(
		      (0 until cities.numCities) toIndexedSeq))

class Hello extends EntryPoint {
  def onModuleLoad() {
    val result = 0
/*
      (for (i <- 0 until 1000) 
	 yield (new RandomTour[Double](new RingedCities(10), 
				       new util.Random(i))).distance
       ).min
*/
    val gr17result =
      (for (i <- 0 until 1) 
	 yield (new RandomTour[Int](Gr17, 
				    new util.Random(i))).distance
       ).min
    RootPanel.get().add(new Label("Result: " + result));
    RootPanel.get().add(new Label("Result: " + gr17result));
  }
}
