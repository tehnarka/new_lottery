(**
  Lazy SWRR variants

  This module describes two functionally equivalent, lazily-updated variants of
  Smooth Weighted Round Robin (SWRR). The goal in both cases is to avoid
  updating the credit state of every delegate at every block,
  while preserving the deterministic and fair behavior of classic SWRR.

  Both versions assume a fixed stake per delegate during a cycle and require
  a global "now" counter (representing the current block index within the cycle).

  Before updating delegate stakes between cycles, you must first convert all 
  implicit credits into explicit ones—by computing each delegate’s current 
  credit—so that the new stakes do not break the correctness of credit-based 
  selection in both lazy variants.
  
  -------------------------------------------------------------------------------
  Variant A — Algebraic Lazy SWRR (using `times_chosen`)
  -------------------------------------------------------------------------------

  In this formulation, each delegate tracks only two values:

    - [stake]         : their current stake (weight)
    - [times_chosen]  : how many times they have been selected so far in the cycle

  The effective credit for a delegate at round [now] is computed as:

       [credit = stake × now − total_stake × times_chosen]

  This formula reflects the net credit that would have accumulated in classic SWRR:
  stake is added each round, and total_stake is subtracted whenever the delegate wins.

  Only the winning delegate is updated (by incrementing [times_chosen]).
  All others remain unchanged between rounds.

  This is the most minimal and purely lazy implementation:
    - No explicit credit tracking
    - No need for last-updated timestamps
    - No map-wide mutations
    - Fully deterministic and fair

  -------------------------------------------------------------------------------
  Variant B — Timestamp-Based Lazy SWRR (using `last_updated`)
  -------------------------------------------------------------------------------

  This variant is closer to the classical SWRR style. Each delegate stores:

    - [stake]          : their current stake
    - [credit]         : their last known explicit credit
    - [last_updated]   : the round at which [credit] was last updated

  The effective credit at round [now] is calculated as:

      [credit + stake × (now − last_updated)]

  At each block:
    - We compute the effective credit for all delegates to select the winner.
    - The winner's credit is recomputed and decreased by total_stake.
    - The winner’s [credit] and [last_updated] are updated accordingly.

  This implementation still avoids full-map traversal for updates, but it involves
  more state per delegate and slightly more mutation logic.

  -------------------------------------------------------------------------------
  Summary Comparison

    - Both versions yield identical selection sequences to classical SWRR.
    - Variant A is cleaner, more compact, and easier to reason about.
    - Variant B is closer to the textbook model and allows partial precomputation.

*)
