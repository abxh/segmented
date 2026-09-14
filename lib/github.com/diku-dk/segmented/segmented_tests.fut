-- | ignore

import "segmented"

-- ==
-- entry: test_segmented_scan
-- input { [true,false,false,true,false,false,true,false,false,false]
--         [1i64,2i64,3i64,4i64,5i64,6i64,7i64,8i64,9i64,10i64] }
-- output { [1i64,3i64,6i64,4i64,9i64,15i64,7i64,15i64,24i64,34i64] }
-- input { [true] [1i64] }
-- output { [1i64] }
-- input { empty([0]bool) empty([0]i64) }
-- output { empty([0]i64) }

entry test_segmented_scan (flags: []bool) (as: []i64) =
  segmented_scan (+) 0 flags as

-- ==
-- entry: test_segmented_reduce
-- input { [true,false,false,true,false,false,true,false,false,false]
--         [1i64,2i64,3i64,4i64,5i64,6i64,7i64,8i64,9i64,10i64] }
-- output { [6i64,15i64,34i64] }
-- input { [true] [1i64] }
-- output { [1i64] }

entry test_segmented_reduce (flags: []bool) (as: []i64) =
  segmented_reduce (+) 0 flags as

-- ==
-- entry: test_replicated_iota
-- input { [2i64,3i64,1i64] } output { [0i64,0i64,1i64,1i64,1i64,2i64] }
-- input { [3i64] } output { [0i64,0i64,0i64] }
-- input { [2i64,0i64,1i64] } output { [0i64,0i64,2i64] }
-- input { empty([0]i64) } output { empty([0]i64) }
-- input { [0i64] } output { empty([0]i64) }
-- input { [0i64,0i64] } output { empty([0]i64) }

entry test_replicated_iota (repl: []i64) : []i64 =
  replicated_iota repl

-- ==
-- entry: test_repl_segm_iota
-- input { [2i64,3i64,1i64] } output { [0i64,0i64,1i64,1i64,1i64,2i64] [0i64,1i64,0i64,1i64,2i64,0i64] }
-- input { [3i64] } output { [0i64,0i64,0i64] [0i64,1i64,2i64] }
-- input { [2i64,0i64,1i64] } output { [0i64,0i64,2i64] [0i64,1i64,0i64] }
-- input { empty([0]i64) } output { empty([0]i64) empty([0]i64) }
-- input { [0i64] } output { empty([0]i64) empty([0]i64) }
-- input { [0i64,0i64] } output { empty([0]i64) empty([0]i64) }
entry test_repl_segm_iota (repl: []i64) : ([]i64, []i64) =
  repl_segm_iota repl

-- ==
-- entry: test_segmented_iota
-- input { [false,false,false,true,false,false,false] }
-- output { [0i64,1i64,2i64,0i64,1i64,2i64,3i64] }
-- input { [false] } output { [0i64] }
-- input { [true] } output { [0i64] }
-- input { empty([0]bool) } output { empty([0]i64) }

entry test_segmented_iota (flags: []bool) : []i64 =
  segmented_iota flags

-- ==
-- entry: test_expand
-- input { [2i64,3i64,1i64] }
-- output { [0i64,2i64,0i64,3i64,6i64,0i64] }
-- input { [0i64,0i64,3i64,1i64] }
-- output { [0i64,3i64,6i64,0i64] }

entry test_expand (arr: []i64) : []i64 =
  expand (\x -> x) (\x i -> x * i) arr

-- ==
-- entry: test_expand_reduce
-- input { [2i64,0i64,3i64,1i64] }
-- output { [2i64,9i64,0i64] }

entry test_expand_reduce (arr: []i64) : []i64 =
  expand_reduce (\x -> x) (\x i -> x * i) (+) 0 arr

-- ==
-- entry: test_expand_outer_reduce
-- input { [2i64,0i64,3i64,1i64] }
-- output { [2i64,0i64,9i64,0i64] }

entry test_expand_outer_reduce (arr: []i64) : []i64 =
  expand_outer_reduce (\x -> x) (\x i -> x * i) (+) 0 arr

-- ==
-- entry: test_expand_filter
-- input { [2i64,3i64,1i64] }
-- output { [2i64,3i64,6i64] }
-- input { [0i64,0i64,3i64,1i64] }
-- output { [3i64,6i64] }

entry test_expand_filter (arr: []i64) : []i64 =
  expand_filter (\x -> x) (\x i -> x * i) (\x i -> x * i != 0) arr

-- ==
-- entry: test_expand_filter_all
-- input { [64i64] }
-- output { [0i64,1i64,2i64,3i64,4i64,5i64,6i64,7i64,8i64,9i64,10i64,11i64,12i64,
--           13i64,14i64,15i64,16i64,17i64,18i64,19i64,20i64,21i64,22i64,23i64,24i64,
--           25i64,26i64,27i64,28i64,29i64,30i64,31i64,32i64,33i64,34i64,35i64,36i64,
--           37i64,38i64,39i64,40i64,41i64,42i64,43i64,44i64,45i64,46i64,47i64,48i64,
--           49i64,50i64,51i64,52i64,53i64,54i64,55i64,56i64,57i64,58i64,59i64,60i64,61i64,62i64,63i64] }

entry test_expand_filter_all (arr: []i64) : []i64 =
  expand_filter (\x -> x) (\_ i -> i) (\_ _ -> true) arr


-- ==
-- entry: test_expand_filter_single
-- input { [65i64] }
-- output { [64i64] }
entry test_expand_filter_single (arr: []i64) : []i64 =
  expand_filter (\x -> x) (\_ i -> i) (\_ i -> i == 64) arr
