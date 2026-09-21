import "lib/github.com/diku-dk/segmented/segmented"

entry random_shape a b n m =
  iota n
  |> map (\x -> (a * (1 + x) + b) %% m)

entry replicate_shape (n: i64) (a: i64) =
  replicate n a

-- ==
-- entry: bench_repl_segm_iota bench_replicated_iota
-- script input { random_shape 32131i64 1321337i64 10000000 50 }
-- script input { random_shape 32131i64 1321337i64 5000 100000 }
-- script input { replicate_shape 1000 200000 }
-- script input { replicate_shape 100000000 1 }
-- script input { replicate_shape 200000000 0 }
entry bench_repl_segm_iota = repl_segm_iota
entry bench_replicated_iota = replicated_iota

-- ==
-- entry: bench_filter_ref bench_filter
-- script input { gen 1000000i64 0i64  8i64 }
-- script input { gen 1000000i64 0i64 16i64 }
-- script input { gen 1000000i64 0i64 32i64 }
-- script input { gen 1000000i64 0i64 64i64 }
-- script input { gen 1000000i64 0i64 128i64 }
-- script input { gen 1000000i64 0i64 256i64 }
-- script input { gen 1000000i64 0i64 512i64 }
-- script input { gen 1000000i64 0i64 1024i64 }

def expand_filter_ref 'a 'b
                  (sz: a -> i64)
                  (get: a -> i64 -> b)
                  (pred: a -> i64 -> bool)
                  (arr: []a) : []b =
  let szs = map sz arr
  let (idxs, iotas) = repl_segm_iota szs
  in zip idxs iotas
     |> filter (\(i, j) -> pred arr[i] j)
     |> map (\(i, j) -> get arr[i] j)

local
def hash (x: i32) : i32 =
  let x = u32.i32 x
  let x = ((x >> 16) ^ x) * 0x45d9f3b
  let x = ((x >> 16) ^ x) * 0x45d9f3b
  let x = ((x >> 16) ^ x)
  in i32.u32 x

entry gen (n: i64) (lo: i64) (hi: i64) : []i64 =
  let xs = iota n
  in map (\i ->
            let h = hash (i32.i64 i)
            let r = u32.i32 h
            in lo + i64.u32 r % (hi - lo + 1))
         xs

def get (x: i64) (i: i64) : i64 =
  x + i

def pred (x: i64) (i: i64) : bool =
  (x * 31 + i * 17) % 100 < 50

entry bench_filter (xs: []i64) : []i64 =
  expand_filter id get pred xs

entry bench_filter_ref (xs: []i64) : []i64 =
  expand_filter_ref id get pred xs
