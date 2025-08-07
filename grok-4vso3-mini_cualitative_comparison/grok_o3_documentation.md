# Grok-4 vs o3-mini – Problem-Level Comparison Toolkit

This document explains what the R script does and how to interpret each output. The goal is to surface **“interesting” problems** when comparing **Grok-4** and **o3-mini** on your math benchmark, beyond the usual significance tests.

---

## Expected Input

A CSV (default: `results_math_bench.csv`) with columns:

- `problem_id` — unique id (≈105)
- `domain` — one of: Cálculo, Geometría, Lógica Matemática, Álgebra, Combinatoria, Probabilidad, Teoría de números
- `language` — `ES` or `EN`
- `prompt` — `ZS` (zero-shot) or `COT`
- `model` — model name
- `run` — run index (1–3)
- `correct` — 0/1

The script normalizes labels (`language`, `prompt` to upper-case; model names collapsed to “Grok-4” and “o3-mini” by regex) and filters to only these two models for the qualitative comparison.

---

## Analysis Pipeline

### 1) Aggregate per configuration cell
A *cell* is a `(problem_id × model × language × prompt)` combination.  
For each cell, we compute across runs:
- `acc` = mean of `correct` (e.g., 2/3 = 0.667)
- `run_sd` = SD across runs (higher = more run-to-run instability)
- `n_runs` = #runs observed (usually 3)

### 2) Roll up per problem & model
For each `(problem_id × model)` we summarize across cells:
- `acc_overall` = **mean of the cell accuracies** (treats each cell equally)  
  *Note:* If you prefer weighting by runs, switch to a weighted mean.
- `best_acc`, `worst_acc` = max/min across cells
- `acc_ES`, `acc_EN` = mean accuracy within ES/EN
- `acc_COT`, `acc_ZS` = mean accuracy within prompt types
- `run_sd_mean` = average of the run SDs across cells

### 3) Compare Grok-4 vs o3-mini per problem
Create side-by-side columns for both models and compute:
- `solved_any_*` — whether a model achieved any success (acc_overall > 0)
- **Strength flags** (configurable thresholds):
  - “strong”: `acc_overall ≥ 0.67`
  - “weak”: `acc_overall ≤ 0.33`
- **Uniqueness / failure** flags:
  - `grok_only_any` — only Grok solved
  - `o3_only_any` — only o3 solved
  - `both_fail` — neither solved in any config
- **Contrast flags**:
  - `grok_strong_o3_weak`, `o3_strong_grok_weak`
- **Sensitivity gaps** (within-model) and **between-model differences**:
  - `lang_gap_*` = |ES − EN|, `prompt_gap_*` = |COT − ZS|
  - `lang_gap_diff`, `prompt_gap_diff` = how different Grok’s gap is vs o3’s
- **Stability difference**:
  - `stability_diff` = |`run_sd_mean_grok` − `run_sd_mean_o3`|
- **Relative advantage**:
  - `rel_adv` = `acc_overall_grok − acc_overall_o3` (positive → Grok better)

### 4) Flag & rank “interesting” problems
- `flags` — human-readable tags (e.g., “Only Grok solved | Large difference in prompt sensitivity”) based on thresholds.
- `n_flags` — number of flags per problem.
- `interesting` — ranked by `n_flags` (desc) then `|rel_adv|` (desc).

---

## Returned Objects (when calling `out <- find_interesting_problems(df)`)

- **`out$per_cell`** — one row per `(problem_id, model, language, prompt)` with `acc`, `run_sd`, `n_runs`.  
  *Use it for* fine-grained inspection and custom plots.

- **`out$per_problem_model`** — one row per `(problem_id, model)` with rolled-up metrics.  
  *Use it for* model-level summaries per problem.

- **`out$interesting`** — one row per `problem_id`, with side-by-side metrics and flags:  
  Includes `acc_overall__*`, `acc_ES__*`, `acc_EN__*`, `acc_COT__*`, `acc_ZS__*`, `run_sd_mean__*`, `rel_adv`, `flags`, `n_flags`, and booleans (`grok_only_any`, `o3_only_any`, `both_fail`, `grok_strong_o3_weak`, `o3_strong_grok_weak`).  
  *Use it for* ranking, quick filtering (e.g., “both_fail”), and paper tables.

- **`out$interesting_long`** — long format: one row per `(problem_id, flag)`.  
  *Use it for* counting/plotting specific flag types.

---

## Saved Files (in `outputs_grok_o3/`)

1. **`appendix_interesting_problems.csv`**  
   A clean, appendix-ready table (top 50 by default) with:
   - `problem_id`, `domain`
   - `n_flags`, `flags`
   - `acc_grok`, `acc_o3` (overall accuracies)
   - `rel_adv` (Grok − o3)

   **Interpretation:** A curated list of “stories”: unique solves, joint failures, big sensitivity gaps, or large margins.

2. **`scatter_grok_vs_o3.png`** — *Per-problem accuracy scatter*  
   - **Axes:** x = Grok overall accuracy, y = o3 overall accuracy (0–1)  
   - Diagonal = parity; dashed lines at 0.67 mark the “strong” threshold  
   - **Shapes** encode categories; **size** scales with `n_flags`  
   - **Labels** show top-N most notable problems  
   **Read:** Head-to-head performance map.  
   - Below diagonal → Grok advantage; above → o3 advantage; near origin → both weak.

3. **`domain_advantage.png`** — *Domain-level advantage distribution*  
   - y = `rel_adv = acc_grok − acc_o3`  
   - Boxplots per domain; zero line for parity; hollow diamond marks **mean**  
   **Read:** Where each model tends to outperform and how variable the gaps are.

4. **`sensitivity_diffs.png`** — *Language & prompt sensitivity (faceted)*  
   - Facets: **Language gap (|ES − EN|)** and **Prompt gap (|COT − ZS|)**  
   - Columns: Grok-4 vs o3-mini  
   - Boxplots by domain  
   **Read:** Which model is more/less sensitive to language/prompt.

5. **`drilldown_problem_<ID>.png`** — *Per-problem detail with 95% CIs*  
   - Bars per `(language / prompt)` cell, side-by-side for Grok & o3  
   - Height = accuracy over runs; error bars = **95% binomial CIs**  
   - Dashed line at 0.67 (“strong”)  
   **Read:** Which configs worked for which model and how stable the result looks.

---

## How to Use in Your Paper

- **Main text:** Use the scatter to anchor the qualitative comparison and call out labeled case studies (“only Grok solved”, “both fail”, “o3 strong while Grok weak”).  
- **Per-domain narrative:** Use the domain advantage plot to say “Grok tends to be stronger in X; o3 in Y,” with medians/means.  
- **Robustness:** Use sensitivity plots to argue which model is more stable across ES/EN and ZS/COT.  
- **Appendix:** Include the CSV table and a few drilldown figures for representative items.

---

## Key Definitions & Thresholds

- **Solved (any config):** `acc_overall > 0` for that problem.  
- **Strong:** `acc_overall ≥ 0.67` (≥ 2/3 runs correct on average across cells).  
- **Weak:** `acc_overall ≤ 0.33`.  
- **Sensitivity gaps:** within-model absolute differences; **…_diff** compares the gap sizes across models.  
- **Stability:** `run_sd` is SD over 3 runs in a cell; `run_sd_mean` averages it across cells for a problem.

> **Averaging note:** `acc_overall` averages **cells equally**, not runs. This avoids overweighting configs with more runs. If you prefer run-weighted accuracies, use a weighted mean with `n_runs`.

---

## Customization Knobs

- **Model names:** edit regex mapping and `GROK_NAME`/`O3_NAME`.  
- **Thresholds:** tweak `solved_consistent`, `solved_weak`, `gap_big`, `stab_big`, `advantage_big`.  
- **Labeling:** increase `label_top_n` on the scatter to annotate more points.  
- **Weighted overall accuracy:** switch the roll-up to `weighted.mean(acc, n_runs)` if desired.

---

## Quality Checks & Edge Cases

- **Missing cells** (e.g., no EN/COT): NA-robust means; missing cells simply don’t contribute.  
- **Few runs:** With only 3 runs, CI bars are wide—interpret stability cautiously.  
- **Imbalanced configs:** If your evaluation omits some cells by design, mention it; `acc_overall` reflects only observed cells.

---

## Suggested 2–3 Sentences for the Paper

> “Beyond significance tests, we examined problem-level patterns between Grok-4 and o3-mini. We flagged items where only one model ever solved the task, where both consistently failed, and where language or prompt type induced large within-model swings. Figure X maps per-problem accuracies (diagonal = parity), Figure Y shows domain-level advantage distributions, and Figure Z visualizes language/prompt sensitivity per model; an appendix lists the top flagged items with short descriptors.”
