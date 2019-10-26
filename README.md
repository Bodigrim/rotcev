# vector-rotcev

A wrapper for an arbitrary `Vector` with O(1) `reverse`. Instead of creating a copy, it just flips a flag, which inverts indexing. This is basically the same idea, which is employed for O(1) `slice` of `Vector`.
