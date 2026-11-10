# Scaling station-based quality control to large networks

## Diagnosis

The original spatial checks repeatedly fitted every station against every
other station. With 2,000 stations, one spatial pass considered almost four
million directed station pairs. Precipitation repeated neighbour selection at
every accumulation scale, while both precipitation and temperature also ran
an R loop for every target timestamp. PSOCK workers received large all-pairs
distance and elevation matrices. More cores therefore multiplied memory and
serialization costs without removing the quadratic work.

## Robust fix applied in `precipQC` and `tempQC`

1. Validate and align station metadata once. Store distance and elevation
   similarity as compact target-specific candidate vectors instead of dense
   station-by-station matrices.
2. Apply the exact distance filter first. For a located target, retain at most
   `max.candidates` nearest candidates, including the elevation penalty. For a
   target without coordinates, use a deterministic 64-timestamp correlation
   screen before retaining the candidate bound.
3. Use at most `selection.samples` evenly spaced timestamps for initial
   correlation/regression screening. Scale `min.overlap` to that sample. Final
   QC scores still use every timestamp.
4. Select precipitation neighbours once at the native resolution and reuse
   the same graph at every requested accumulation scale. Return full estimates
   and scores only for the native scale; coarser scales return flags.
5. Calculate precipitation row medians, weighted medians, and local MAD values
   in a registered C routine. This removes the target-by-time R loop.
6. Calculate temperature correlations, regression coefficients, RMSE values,
   predictions, and weighted combinations with vectorized sufficient
   statistics. This removes station-pair `lm.fit` calls and timestamp loops.
7. Keep station chunks deterministic across serial, fork, and PSOCK backends.
   The existing `ncores`, `parallel.type`, and `cluster.nodes` controls remain
   the only parallel interface.

The scalable defaults are `max.candidates=200L` and
`selection.samples=2048L`. Setting either argument to `Inf` requests the
corresponding exhaustive behaviour. Small networks and short records therefore
retain their previous exact path automatically.

## Pattern to reuse in another station function

Use this sequence when adapting another spatial test:

1. Separate candidate discovery, candidate selection, and final scoring.
2. Create one immutable spatial context before starting workers or temporal
   scale loops.
3. Filter by cheap authoritative information first: coordinates, distance,
   elevation, instrument type, or catchment membership.
4. Bound the remaining candidate pool and make approximate screening
   deterministic. Always provide an explicit exhaustive opt-in.
5. Sample only the expensive model-selection stage. Never sample the final
   observation flags unless that changes the documented scientific method.
6. Replace inner R loops with matrix sufficient statistics or a small
   registered compiled kernel. Keep the outer station chunk as the parallel
   unit so task scheduling remains coarse.
7. Avoid returning or exporting intermediate objects that the caller does not
   use. In particular, do not create dense all-pairs matrices when only a few
   neighbours per target are needed.
8. Record performance controls in the result settings and progress output,
   preserve station order and object classes, and stop worker clusters on exit.
9. Validate exact small-data behaviour, serial/PSOCK/fork equivalence, missing
   coordinates, exhaustive mode, invalid bounds, and a 2,000-station smoke
   case. Build and check the exact package archive after source-tree tests.

## Expected complexity

Let `N` be stations, `C` the candidate bound, `K` selected neighbours, `S`
selection timestamps, and `T` all timestamps. Initial exact selection is
bounded by approximately `N * C * S`, and final scoring by `N * K * T`.
This replaces repeated `N^2 * T` work. Geographic screening is approximately
`N^2` inexpensive distance arithmetic but does not allocate an `N` by `N`
matrix. For networks with complete coordinates, no all-network correlation
matrix is calculated.

Run `inst/benchmarks/benchmark_QC_spatial_2000.R` after installation to measure
the local serial or parallel backend with a reproducible 2,000-station case.
