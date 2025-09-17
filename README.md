# new_lottery — Leader Selection in OCaml

> **Status:** research prototype · OCaml · Tezos-style levels/rounds · multiple algorithms (Alias, LowVAlloc, MinVAlloc, SWRR, SSSA)

This repository contains clean, minimal OCaml implementations of several stake‑proportional leader/slot allocation methods with a shared test harness:

- **Alias method** (`alias.ml`) — Vose’s alias sampling over a fixed committee.
- **MinVAlloc** (`minvalloc.ml`) — minimal variance apportionment (deterministic quota rounding).
- **LowVAlloc** (`lowvalloc.ml`) — low‑variance variant of quota rounding.
- **SSSA** (`sssa.ml`) — Sorted Stride‑Based Slot Allocation to spread a precomputed array across the cycle.
- **SWRR (map/stream)** (`swrr_map.ml`, `swrr.ml`) — Smooth Weighted Round Robin (deterministic, credit‑based).
- **Lazy SWRR with memory** (`swrr_lazy.ml`) — single‑update per block; supports stake updates between cycles via `finalize_cycle`/`update_state`.

An interactive runner (**`test.ml`**) lets you compare methods across multiple cycles, print per‑cycle stats, and (optionally) post‑process with **SSSA**.

---

## Quick start

```bash
opam switch create . ocaml-base-compiler.5.2.1 # or 5.1.x
opam install dune fmt
dune build
dune exec new_lottery.test
```

If the project references a library `new_lottery.common`, make sure it is present in your workspace (as a local library). The `dune` file for the executable expects:

```lisp
(executable
 (name test)
 (public_name new_lottery.test)
 (modules test alias swrr swrr_map swrr_lazy lowvalloc minvalloc sssa )
 (libraries new_lottery.common))
```

Run the interactive harness and follow the prompts:

```
1 - Alias method
2 - LowVAlloc + SSSA
3 - MinVAlloc + SSSA
4 - SWRR via stream (with memory)         # data for several cycles
5 - SSSA after MinVAlloc
6 - SWRR using map (fixed nb of cycles)
7 - Lazy SWRR (map-based, memory-aware)
8 - Test Lazy SWRR (diagnostics)
> <enter method id>
How many cycles? > <enter integer>
```

---

## Algorithms at a glance

### Alias (Vose)
- **File:** `alias.ml`
- **Purpose:** O(1) expected-time random sampling proportional to stake.
- **Use:** `let output = Alias.run input blocks_per_cycle` (see `test.ml` for shape)
- **Trade-offs:** excellent throughput; randomness‑heavy; per-slot sampling variance.

### MinVAlloc & LowVAlloc
- **Files:** `minvalloc.ml`, `lowvalloc.ml`
- **Purpose:** deterministic apportionment: each delegate i gets approximately `R * s_i` slots with minimal/low variance rounding.
- **Use:** `let raw = Minvalloc.run input` (or `Lowvalloc.run`), then optionally `Sssa.run raw` to redistribute evenly over the cycle.
- **Properties observed in tests:** absolute error per delegate typically ∈ [−1, +1], often tighter for LowVAlloc.

### SSSA (Sorted Stride-Based Slot Allocation)
- **File:** `sssa.ml`
- **Input:** array `output : string array` with multiplicities per delegate (e.g., from MinVAlloc/LowVAlloc).
- **Effect:** spreads each delegate’s slots uniformly over indices `[0, R−1]` to reduce clustering while preserving counts.
- **Use:** `let output = Sssa.run raw`

### SWRR (Smooth Weighted Round Robin)
- **Files:** `swrr_map.ml`, `swrr.ml`
- **Model:** classic credit‑based scheduler; deterministic order over a cycle.
- **Map variant:** state stored in a map; updated per step.
- **Stream variant:** produces rights on the fly without precomputing the full array.

### Lazy SWRR with memory
- **File:** `swrr_lazy.ml`
- **Types:**

  ```ocaml
  type delegate_state = { stake : int; credit : int; last_updated : int }
  type state = { delegates : delegate_state DelegateMap.t; step : int }
  ```

- **Core API:**
  - `iteration    : (string * int) list -> state -> string * state`
  - `finalize_cycle : (string * int) list -> state -> state`  (*materialize implicit credit before stake change*)
  - `update_state : (string * int) list -> state -> state`    (*apply new stakes; preserve memory*)
- **Idea:** only the *winner’s* credit is updated per block; others are lazily updated when touched, tracked by `last_updated`. Between cycles, call `finalize_cycle` then `update_state` to keep continuity even if the delegate set changes (new = 0 credit; old keep carry‑over).

---

## Usage patterns

- **Deterministic precompute:** `MinVAlloc`/`LowVAlloc` → `SSSA.run` → array of delegates for `R` blocks.
- **On-the-fly deterministic:** `SWRR` or `Lazy SWRR` → call `iteration` per block, store next `state` only.
- **Randomized sampling:** `Alias.run` per block; optionally post-balance with larger windows.
  
---

## Invariants and “why”

- **Cycle snapshot:** stakes are frozen at cycle start to provide a fixed, manipulation‑resistant basis for rights allocation.
- **Normalization:** work with shares `s_i = S_i / Σ S_i`; expected slots `E_i = R * s_i`; scale‑invariant checks.
- **Memory across cycles (SWRR):** carry over unmet credit; new delegates start with credit `0`; removed delegates are dropped.

---
## Testing & diagnostics

The interactive harness (`test.ml`) prints:
- per‑cycle distribution checks: expected vs actual counts,
- change statistics between states (useful with lazy SWRR),
- optional round/level matrices when SSSA is applied.

Run and explore different methods & cycle counts.

---

## References

- M. D. Vose, *A linear algorithm for generating random numbers with a given distribution*, IEEE TSE, 1991. (alias method)
- L. Grimmett, *Apportionment Methods* (minimal/low variance rounding; classic quota methods).
- Tenderbake & Tezos documentation (levels/rounds model; cycle snapshots; attestations).

> The code is written in a **functional, allocation‑aware style** (no unnecessary loops; persistent structures where reasonable).

---


