fun arraySwap arr (ix1, ix2) = 
    let 
      val elt1 = Array.sub (arr, ix1)
    in 
      Array.update (arr, ix1, Array.sub (arr, ix2));
      Array.update (arr, ix2, elt1)
    end

fun randomTour rng cities = 
    (* Create array [0..n-1] and shuffle in place by Fisher-Yates'
       method. See http://en.wikipedia.org/wiki/Knuth_shuffle *)
    let
      val swaps = cities-1
      val swapSpecs = 
	  List.tabulate 
	      (swaps, fn ix => 
			 (ix, (Random.randRange (ix, swaps)) rng))
      and tour = Array.tabulate (cities, fn ix => ix)
    in 
      List.app (arraySwap tour) swapSpecs;
      tour
    end

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

fun tourDistance lookup tour =
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
	  (trials, fn _ => 
		      tourDistance lookup (randomTour rng cities))
    end

fun invertPair rng tour =
    let 
      val length = Array.length tour
      val invertPos = (Random.randRange (0, length-1)) rng
      and tour' = Array.array (length, 0)
    in
      Array.copy {src=tour, dst=tour', di=0};
      arraySwap tour' (invertPos, (invertPos+1) mod length);
      tour'
    end

local
  fun generate mutate lookup tour =
      let 
	val tour' = mutate tour
      in
	(tour', tourDistance lookup tour')
      end
in
  fun bestMutant mutate lookup trials tour =
      let 
	val allMutants = 
	    List.tabulate (trials, fn _ => 
				      generate mutate lookup tour)
      in
	List.foldl 
	    (fn (curr as (_, distance), best as (_, bestDist)) =>
		if distance<bestDist then curr else best)
	    (hd allMutants)
	    (tl allMutants)
      end
end

fun test () =
    let
      val rng = Random.rand (1,2)
      val tour = randomTour rng 17
      val (best, distance) = 
	  bestMutant (invertPair rng) gr17Lookup 4 tour
      val showTour = Array.app (fn x => (print (Int.toString x); print " "))
    in
      showTour tour;
      print ":"; 
      print (Int.toString (tourDistance gr17Lookup tour));
      print "\n";
      showTour best; (* answer was verified *)
      print ":"; 
      print (Int.toString distance)
    end

fun hillclimb lookup cities neighbours (* numFailures *) seeds =
    let
      val rng = Random.rand seeds
      val start = randomTour rng cities
      and lookAround = bestMutant (invertPair rng) lookup neighbours
      fun climb (here as (_, currDist)) (there as (tour, dist)) =
	  if dist >= currDist then here
	  else climb there (lookAround tour)
    in
      climb (start, tourDistance lookup start) (lookAround start)
    end
