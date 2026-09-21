-- | Irregular segmented operations, like scans and reductions.

-- | Segmented scan. Given a binary associative operator ``op`` with
-- neutral element ``ne``, computes the inclusive prefix scan of the
-- segments of ``as`` specified by the ``flags`` array, where `true`
-- starts a segment and `false` continues a segment.
def segmented_scan [n] 't
                   (op: t -> t -> t)
                   (ne: t)
                   (flags: [n]bool)
                   (as: [n]t) : *[n]t =
  (unzip (scan (\(x_flag, x) (y_flag, y) ->
                  ( x_flag || y_flag
                  , if y_flag then y else x `op` y
                  ))
               (false, ne)
               (zip flags as))).1

-- | Segmented reduction. Given a binary associative operator ``op``
-- with neutral element ``ne``, computes the reduction of the segments
-- of ``as`` specified by the ``flags`` array, where `true` starts a
-- segment and `false` continues a segment.  One value is returned per
-- segment.
def segmented_reduce [n] 't
                     (op: t -> t -> t)
                     (ne: t)
                     (flags: [n]bool)
                     (as: [n]t) =
  segmented_scan op ne flags as
  |> zip (rotate 1 flags)
  |> filter (.0)
  |> map (.1)

-- | Replicated iota. Given a repetition array, the function returns
-- an array with each index (starting from 0) repeated according to
-- the repetition array. As an example, replicated_iota [2,3,1]
-- returns the array [0,0,1,1,1,2].
def replicated_iota [n] (reps: [n]i64) : []i64 =
  let offsets =
    map2 (-) (scan (+) 0 reps) reps
    |> map2 (\r o -> if r == 0 then -1 else o) reps
  let size = reduce_comm (+) 0 reps
  let tmp = scatter (replicate size 0) offsets (iota n)
  in scan i64.max i64.lowest tmp

-- | Segmented iota. Given a flags array, the function returns an
-- array of index sequences, each of which is reset according to the
-- flags array. As an examples, segmented_iota
-- [false,false,false,true,false,false,false] returns the array
-- [0,1,2,0,1,2,3].
def segmented_iota [n] (flags: [n]bool) : *[n]i64 =
  let iotas = segmented_scan (+) 0 flags (replicate n 1)
  in map (\x -> x - 1) iotas

-- | Replicated and segmented iota generated together in a slighly more
-- efficient way. Each segment in the segmented iota corresponds to a segment in
-- the replicated iota. As an example repl_segm_iota [2,3,1] returns the arrays
-- [0,0,1,1,1,2] and [0,1,0,1,2,0].
def repl_segm_iota [n] (reps: [n]i64) : (*[]i64, *[]i64) =
  let offsets =
    map2 (-) (scan (+) 0 reps) reps
    |> map2 (\r o -> if r == 0 then -1 else o) reps
  let size = reduce_comm (+) 0 reps
  let tmp = scatter (replicate size 0) offsets (iota n)
  let repl = scan i64.max i64.lowest tmp
  let segm = map2 (\i r -> i - offsets[r]) (iota size) repl
  in (repl, segm)

-- | Generic expansion function. The function expands a source array
-- into a target array given (1) a function that determines, for each
-- source element, how many target elements it expands to and (2) a
-- function that computes a particular target element based on a
-- source element and the target element number associated with the
-- source. As an example, the expression expand (\x->x) (*) [2,3,1]
-- returns the array [0,2,0,3,6,0].
def expand 'a 'b (sz: a -> i64) (get: a -> i64 -> b) (arr: []a) : *[]b =
  let szs = map sz arr
  let (idxs, iotas) = repl_segm_iota szs
  in map2 (\i j -> get arr[i] j) idxs iotas

-- | Expansion function equivalent to performing a segmented reduction
-- to the result of a general expansion with a flags vector expressing
-- the beginning of the expanded segments. The function makes use of
-- the intermediate flags vector generated as part of the expansion
-- and the `expand_reduce` function is therefore more efficient than
-- if a segmented reduction (with an appropriate flags vector) is
-- explicitly followed by a call to expand.
def expand_reduce 'a 'b
                  (sz: a -> i64)
                  (get: a -> i64 -> b)
                  (op: b -> b -> b)
                  (ne: b)
                  (arr: []a) : *[]b =
  let szs = map sz arr
  let idxs = replicated_iota szs
  let flags = map2 (!=) idxs (rotate (-1) idxs)
  let iotas = segmented_iota flags
  let vs = map2 (\i j -> get arr[i] j) idxs iotas
  in segmented_reduce op ne flags vs

-- | Expansion followed by an ''outer segmented reduce'' that ensures
-- that each element in the result array corresponds to expanding and
-- reducing the corresponding element in the source array.
def expand_outer_reduce 'a 'b [n]
                        (sz: a -> i64)
                        (get: a -> i64 -> b)
                        (op: b -> b -> b)
                        (ne: b)
                        (arr: [n]a) : *[n]b =
  let sz' x =
    let s = sz x
    in if s == 0 then 1 else s
  let get' x i = if sz x == 0 then ne else get x i
  in expand_reduce sz' get' op ne arr :> [n]b

local
-- | Helper function to find the position of the k'th set bit in an u8
#[inline]
def select_u8 (b: u8) (k: i32) : i32 =
  let f b = b & (b - 1)
  let s0 = b
  let s1 = f s0
  let s2 = f s1
  let s3 = f s2
  let s4 = f s3
  let s5 = f s4
  let s6 = f s5
  let s7 = f s6
  let w =
    match k
    case 0 -> s0
    case 1 -> s1
    case 2 -> s2
    case 3 -> s3
    case 4 -> s4
    case 5 -> s5
    case 6 -> s6
    case 7 -> s7
    case _ -> 0u8
  in u8.ctz w

local
-- | Helper function to find the position of the k'th set bit in an u16
#[inline]
def select_u16 (b: u16) (k: i32) : i32 =
  let lower = u8.u16 b
  let upper = b >> 8 |> u8.u16
  let low_count = u8.popc lower
  in if k < low_count
     then select_u8 lower k
     else select_u8 upper (k - low_count) + 8

local
-- | Helper function to find the position of the k'th set bit in an u32
#[inline]
def select_u32 (b: u32) (k: i32) : i32 =
  let lower = u16.u32 b
  let upper = b >> 16 |> u16.u32
  let low_count = u16.popc lower
  in if k < low_count
     then select_u16 lower k
     else select_u16 upper (k - low_count) + 16

local
-- | Helper function to find the position of the k'th set bit in an u64
#[inline]
def select_u64 (b: u64) (k: i32) : i32 =
  let lower = u32.u64 b
  let upper = b >> 32 |> u32.u64
  let low_count = u32.popc lower
  in if k < low_count
     then select_u32 lower k
     else select_u32 upper (k - low_count) + 32

-- | Expansion function with an additional predicate function ``pred`` that
-- takes the segment source element and segment index to pre-filter them before
-- obtaining the corresponding target element with ``get``.
--
-- This can be used to replace use cases where the source element is transformed
-- to the form #some value | #none with ``get``, by pre-filtering the #none cases
-- with ``pred`` and just outputting the value with ``get`` instead, thereby avoid
-- wasting memory on values that would otherwise be discarded. Note this implementation
-- performs a fixed sequentialization of 64 iterations per segment for ``pred``.
def expand_filter 'a 'b
                  (sz: a -> i64)
                  (get: a -> i64 -> b)
                  (pred: a -> i64 -> bool)
                  (arr: []a) : *[]b =
  let num_bits = i64.i32 u64.num_bits
  let f (xi, o, n) =
    let mask =
      loop mask = 0
      for i < num_bits do
        let b = if i < n then pred arr[xi] (o + i) else false
        in u64.set_bit (i32.i64 i) mask (i32.bool b)
    in (xi, o, mask)
  let get' (xi, o, mask) j =
    let i = select_u64 mask (i32.i64 j)
    in get arr[xi] (o + i64.i32 i)
  let szs = map sz arr
  let arr_szs =
    zip (indices arr) szs
    |> expand (\(_, s) -> (s + num_bits - 1) / num_bits)
              (\(xi, s) i ->
                 let o = num_bits * i
                 let n = i64.min num_bits (s - o)
                 in (xi, o, n))
  let arr_szs' = map f arr_szs
  in expand (\(_, _, mask) -> i64.i32 <| u64.popc mask) get' arr_szs'

-- | Expansion function with an additional predicate function ``pred`` that takes
-- the target / output element to filter. This calls ``get`` twice for every target
-- element produced that fulfill the predicate, and once on the target elements that don't.
def expand_filter_output 'a 'b
                         (sz: a -> i64)
                         (get: a -> i64 -> b)
                         (pred: b -> bool)
                         (arr: []a) : *[]b =
  let pred' x i = pred (get x i)
  in expand_filter sz get pred' arr
