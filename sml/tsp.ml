fun randomTour rand n = 
    (* Create array [0..n-1] and shuffle in place by Fisher-Yates'
       method. See http://en.wikipedia.org/wiki/Knuth_shuffle *)
    let
      val tour = Array.tabulate (n, fn ix => ix)
      fun swap (ix, ix') =
 	  let 
	    val elt = Array.sub (tour, ix)
	  in 
	    Array.update (tour, ix, Array.sub (tour, ix'));
	    Array.update (tour, ix', elt)
	  end
      fun randSwap ix = (ix, (Random.randRange (ix, n-1)) rand)
      val swapSpecs = List.tabulate (n-1, fn ix => randSwap ix)
    in 
      List.app swap swapSpecs;
      tour
    end

(* This implementation is a bit fishy because it modifies the array
   while traversing it.

fun randomTour rand n = 
    let 
	val shuffled = Array.tabulate (n, fn x => x)
	fun swapSuccessor (ix, elt) = 
	    let 
		val ix' = (Random.randRange (ix, n-1)) rand
		val elt' = Array.sub (shuffled, ix')
	    in
		Array.update (shuffled, ix', elt);
		elt'
	    end
    in 
	Array.modifyi swapSuccessor shuffled;
	shuffled
    end
*)

local 
  val gr17Edges =
      (* Data for gr17 problem. Optimum 2085. Each vector is a row in
         the lower triangle of the distance matrix. Source:
         http://www2.iwr.uni-heidelberg.de/groups/comopt/software/TSPLIB95/tsp/gr17.tsp.gz *)
      #[#[633],
	#[257, 390],
	#[91, 661, 228],
	#[412, 227, 169, 383],
	#[150, 488, 112, 120, 267],
	#[80, 572, 196, 77, 351, 63],
	#[134, 530, 154, 105, 309, 34, 29],
	#[259, 555, 372, 175, 338, 264, 232, 249],
	#[505, 289, 262, 476, 196, 360, 444, 402, 495],
	#[353, 282, 110, 324, 61, 208, 292, 250, 352, 154],
	#[324, 638, 437, 240, 421, 329, 297, 314, 95, 578, 435],
	#[70, 567, 191, 27, 346, 83, 47, 68, 189, 439, 287, 254],
	#[211, 466, 74, 182, 243, 105, 150, 108, 326, 336, 184, 391, 145],
	#[268, 420, 53, 239, 199, 123, 207, 165, 383, 240, 140, 448, 202, 57],
	#[246, 745, 472, 237, 528, 364, 332, 349, 202, 685, 542, 157, 289, 426, 483],
	#[121, 518, 142, 84, 297, 35, 29, 36, 236, 390, 238, 301, 55, 96, 153, 336]]
in
  fun gr17Lookup (from, to) =
      let
	val (col, row) = if from<to then (from, to) else (to, from)
      in 
	Vector.sub (Vector.sub (gr17Edges, row-1), col)
      end
end

fun tourLength lookup tour =
    let
      val (_, total) = 
	  Array.foldl 
	      (fn (curr, (prev, total)) => 
		  (curr, total + lookup (prev, curr)))
	      (Array.sub (tour, (Array.length tour) - 1), 0)
	      tour
    in  
      total
    end

fun randomSearch lookup cities trials seeds =
    let
      val rng = Random.rand seeds
    in
      List.tabulate 
	  (trials, fn _ => tourLength lookup (randomTour rng cities))
    end
