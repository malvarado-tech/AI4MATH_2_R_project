# ============================
# Grok-4 vs o3-mini: Interesting Problems Finder + Plots
# ============================

# ---- Config (edit these) ----
INPUT_CSV  <- "ai4math_runs.csv"   # path to your dataset
GROK_NAME  <- "Grok-4"
O3_NAME    <- "o3-mini"

THRESHOLDS <- list(
  solved_consistent = 2/3,   # >= 0.67 = "strong"
  solved_weak       = 1/3,   # <= 0.33 = "weak"
  gap_big           = 0.50,  # large lang/prompt sensitivity diff (between models)
  stab_big          = 0.40,  # large difference in run-to-run SD
  advantage_big     = 0.50   # large overall advantage (Grok - o3 or vice versa)
)

OUTDIR <- "outputs_grok_o3"
dir.create(OUTDIR, showWarnings = FALSE, recursive = TRUE)

# ---- Packages ----
pkgs <- c("dplyr","tidyr","stringr","purrr","readr","ggplot2","ggrepel","scales","rlang")
for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
lapply(pkgs, library, character.only = TRUE)

# ---- Load & normalize data ----
df <- readr::read_csv(INPUT_CSV, show_col_types = FALSE) %>%
  dplyr::mutate(
    language = toupper(language),            # "ES"/"EN"
    prompt   = toupper(prompt),              # "ZS"/"COT"
    run      = as.integer(run),
    correct  = as.integer(correct)
  )

required_cols <- c("problem_id","domain","language","prompt","model","run","correct")
stopifnot(all(required_cols %in% names(df)))

# ============================
# Core function: find_interesting_problems
# ============================
find_interesting_problems <- function(df,
                                      grok_name = "Grok-4",
                                      o3_name   = "o3-mini",
                                      thresholds = THRESHOLDS) {
  safe_mean <- function(x) if (length(x) == 0 || all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
  safe_sd   <- function(x) if (length(x) == 0 || all(is.na(x))) NA_real_ else sd(x, na.rm = TRUE)
  
  dat <- df %>%
    mutate(
      model = case_when(
        str_detect(tolower(model), "grok") ~ grok_name,
        str_detect(tolower(model), "o[- ]?3") ~ o3_name,
        TRUE ~ model
      ),
      language = toupper(language),
      prompt   = toupper(prompt)
    ) %>%
    filter(model %in% c(grok_name, o3_name))
  
  # cell = (problem, model, lang, prompt)
  per_cell <- dat %>%
    group_by(problem_id, domain, model, language, prompt) %>%
    summarise(
      acc    = safe_mean(correct),
      run_sd = safe_sd(correct),
      n_runs = dplyr::n(),
      .groups = "drop"
    )
  
  # roll up per (problem, model)
  per_problem_model <- per_cell %>%
    group_by(problem_id, domain, model) %>%
    summarise(
      acc_overall  = safe_mean(acc),
      best_acc     = suppressWarnings(max(acc, na.rm = TRUE)),
      worst_acc    = suppressWarnings(min(acc, na.rm = TRUE)),
      n_cells      = dplyr::n(),
      acc_ES       = safe_mean(acc[language == "ES"]),
      acc_EN       = safe_mean(acc[language == "EN"]),
      acc_COT      = safe_mean(acc[prompt   == "COT"]),
      acc_ZS       = safe_mean(acc[prompt   == "ZS"]),
      run_sd_mean  = safe_mean(run_sd),
      .groups = "drop"
    )
  
  wide <- per_problem_model %>%
    pivot_wider(
      names_from  = model,
      values_from = c(acc_overall, best_acc, worst_acc, acc_ES, acc_EN, acc_COT, acc_ZS, run_sd_mean),
      names_sep   = "__"
    )
  
  g <- function(col) rlang::exec(`$`, wide, paste0(col, "__", grok_name))
  o <- function(col) rlang::exec(`$`, wide, paste0(col, "__", o3_name))
  
  wide <- wide %>%
    mutate(
      solved_any_grok = tidyr::replace_na(g("acc_overall") > 0, FALSE),
      solved_any_o3   = tidyr::replace_na(o("acc_overall") > 0, FALSE),
      
      grok_strong = tidyr::replace_na(g("acc_overall") >= thresholds$solved_consistent, FALSE),
      o3_strong   = tidyr::replace_na(o("acc_overall") >= thresholds$solved_consistent, FALSE),
      grok_weak   = tidyr::replace_na(g("acc_overall") <= thresholds$solved_weak, FALSE),
      o3_weak     = tidyr::replace_na(o("acc_overall") <= thresholds$solved_weak, FALSE),
      
      grok_only_any = solved_any_grok & !solved_any_o3,
      o3_only_any   = solved_any_o3   & !solved_any_grok,
      both_fail     = !solved_any_grok & !solved_any_o3,
      
      grok_strong_o3_weak = grok_strong & o3_weak,
      o3_strong_grok_weak = o3_strong   & grok_weak,
      
      lang_gap_grok = abs(g("acc_ES") - g("acc_EN")),
      lang_gap_o3   = abs(o("acc_ES") - o("acc_EN")),
      lang_gap_diff = abs(lang_gap_grok - lang_gap_o3),
      
      prompt_gap_grok = abs(g("acc_COT") - g("acc_ZS")),
      prompt_gap_o3   = abs(o("acc_COT") - o("acc_ZS")),
      prompt_gap_diff = abs(prompt_gap_grok - prompt_gap_o3),
      
      stability_diff = abs(g("run_sd_mean") - o("run_sd_mean")),
      
      rel_adv = g("acc_overall") - o("acc_overall")
    ) %>%
    mutate(
      flags = purrr::pmap_chr(
        list(grok_only_any, o3_only_any, both_fail,
             grok_strong_o3_weak, o3_strong_grok_weak,
             lang_gap_diff, prompt_gap_diff, stability_diff, rel_adv),
        ~ {
          labs <- c()
          if (..1) labs <- c(labs, "Only Grok solved (any config)")
          if (..2) labs <- c(labs, "Only o3-mini solved (any config)")
          if (..3) labs <- c(labs, "Both failed (all configs)")
          if (..4) labs <- c(labs, "Grok strong, o3-mini weak")
          if (..5) labs <- c(labs, "o3-mini strong, Grok weak")
          if (!is.na(..6) && ..6 >= thresholds$gap_big) labs <- c(labs, "Large difference in language sensitivity")
          if (!is.na(..7) && ..7 >= thresholds$gap_big) labs <- c(labs, "Large difference in prompt sensitivity")
          if (!is.na(..8) && ..8 >= thresholds$stab_big) labs <- c(labs, "Very different run stability")
          if (!is.na(..9) && abs(..9) >= thresholds$advantage_big) {
            labs <- c(labs, ifelse(..9 > 0, "Large overall advantage: Grok", "Large overall advantage: o3-mini"))
          }
          if (length(labs) == 0) "" else paste(labs, collapse = " | ")
        }
      ),
      n_flags = if_else(flags == "", 0L, stringr::str_count(flags, "\\|") + 1L)
    )
  
  interesting_long <- wide %>%
    mutate(flag_vec = stringr::str_split(flags, "\\s*\\|\\s*")) %>%
    tidyr::unnest(flag_vec) %>%
    mutate(flag_vec = trimws(flag_vec)) %>%
    filter(flag_vec != "") %>%
    rename(flag = flag_vec)
  
  list(
    per_cell = per_cell,
    per_problem_model = per_problem_model,
    interesting = wide %>% arrange(desc(n_flags), desc(abs(rel_adv))),
    interesting_long = interesting_long
  )
}

# ============================
# Plot helpers
# ============================
plot_grok_o3_scatter <- function(out, grok_name = GROK_NAME, o3_name = O3_NAME, label_top_n = 15) {
  tbl <- out$interesting %>%
    mutate(
      acc_grok = .[[paste0("acc_overall__", grok_name)]],
      acc_o3   = .[[paste0("acc_overall__", o3_name)]],
      cat = dplyr::case_when(
        grok_only_any ~ "Only Grok",
        o3_only_any   ~ "Only o3-mini",
        both_fail     ~ "Both fail",
        grok_strong_o3_weak ~ "Grok strong, o3 weak",
        o3_strong_grok_weak ~ "o3 strong, Grok weak",
        TRUE ~ "Other"
      )
    )
  
  label_ids <- tbl %>%
    mutate(rank_score = n_flags + scales::rescale(abs(rel_adv), to = c(0, 1))) %>%
    arrange(desc(rank_score)) %>%
    slice_head(n = label_top_n) %>%
    pull(problem_id)
  
  ggplot(tbl, aes(x = acc_grok, y = acc_o3)) +
    geom_abline(slope = 1, intercept = 0, linetype = 2) +
    geom_point(aes(shape = cat, size = pmin(1 + n_flags, 5)), alpha = 0.85) +
    geom_hline(yintercept = 0.67, linetype = 3) +
    geom_vline(xintercept = 0.67, linetype = 3) +
    ggrepel::geom_label_repel(
      data = subset(tbl, problem_id %in% label_ids),
      aes(label = problem_id),
      box.padding = 0.4, max.overlaps = Inf
    ) +
    scale_x_continuous("Grok-4 accuracy (overall)", limits = c(0, 1), labels = scales::percent_format(accuracy = 1)) +
    scale_y_continuous("o3-mini accuracy (overall)", limits = c(0, 1), labels = scales::percent_format(accuracy = 1)) +
    scale_shape_discrete(name = "Category") +
    scale_size_continuous(name = "# flags", range = c(2, 6), guide = "none") +
    coord_equal() +
    theme_minimal(base_size = 12)
}

plot_domain_advantage <- function(out, grok_name = GROK_NAME, o3_name = O3_NAME) {
  tbl <- out$interesting %>%
    transmute(
      domain, problem_id,
      rel_adv = .[[paste0("acc_overall__", grok_name)]] - .[[paste0("acc_overall__", o3_name)]]
    )
  ggplot(tbl, aes(x = domain, y = rel_adv)) +
    geom_hline(yintercept = 0, linetype = 2) +
    geom_boxplot(outlier.alpha = 0.4) +
    stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = NA) +
    labs(x = "Domain", y = "Advantage (Grok - o3)") +
    theme_minimal(base_size = 12) +
    coord_flip()
}

plot_sensitivity_diffs <- function(out) {
  tbl <- out$interesting %>%
    select(problem_id, domain,
           lang_gap_grok, lang_gap_o3,
           prompt_gap_grok, prompt_gap_o3) %>%
    pivot_longer(cols = -c(problem_id, domain),
                 names_to = c("type", "model"),
                 names_pattern = "(lang_gap|prompt_gap)_(grok|o3)",
                 values_to = "gap") %>%
    mutate(
      type  = ifelse(type == "lang_gap", "Language gap (|ES-EN|)", "Prompt gap (|COT-ZS|)"),
      model = ifelse(model == "grok", "Grok-4", "o3-mini")
    )
  
  ggplot(tbl, aes(x = domain, y = gap)) +
    geom_boxplot(outlier.alpha = 0.35) +
    facet_grid(type ~ model, scales = "free_y") +
    labs(x = "Domain", y = "Gap in accuracy") +
    theme_minimal(base_size = 12) +
    coord_flip()
}

# Per-problem drilldown (by language/prompt) with binomial CIs over runs
plot_problem_drilldown <- function(df, problem_id, grok_name = GROK_NAME, o3_name = O3_NAME) {
  dat <- df %>%
    mutate(
      model = case_when(
        str_detect(tolower(model), "grok") ~ grok_name,
        str_detect(tolower(model), "o[- ]?3") ~ o3_name,
        TRUE ~ model
      ),
      language = toupper(language),
      prompt   = toupper(prompt)
    ) %>%
    filter(model %in% c(grok_name, o3_name), problem_id == !!problem_id)
  
  # summary per cell (model x language x prompt)
  cell <- dat %>%
    group_by(model, language, prompt) %>%
    summarise(
      n = dplyr::n(),
      k = sum(correct, na.rm = TRUE),
      acc = ifelse(n > 0, k / n, NA_real_),
      ci_lo = tryCatch(stats::prop.test(k, n)$conf.int[1], error = function(e) NA_real_),
      ci_hi = tryCatch(stats::prop.test(k, n)$conf.int[2], error = function(e) NA_real_),
      .groups = "drop"
    ) %>%
    mutate(cfg = paste(language, prompt, sep = " / "))
  
  ggplot(cell, aes(x = cfg, y = acc, fill = model)) +
    geom_hline(yintercept = 2/3, linetype = 3) +
    geom_col(position = position_dodge(width = 0.7), width = 0.6) +
    geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi),
                  position = position_dodge(width = 0.7), width = 0.2) +
    scale_y_continuous("Accuracy (runs)", limits = c(0, 1), labels = scales::percent_format(accuracy = 1)) +
    labs(x = "Language / Prompt", title = paste("Problem", problem_id, "— per-config accuracy with 95% CIs")) +
    theme_minimal(base_size = 12)
}

# Simple appendix table
top_interesting_table <- function(out, n = 25, grok_name = GROK_NAME, o3_name = O3_NAME) {
  out$interesting %>%
    filter(n_flags > 0) %>%
    transmute(
      problem_id, domain, n_flags, flags,
      acc_grok = .[[paste0("acc_overall__", grok_name)]],
      acc_o3   = .[[paste0("acc_overall__", o3_name)]],
      rel_adv
    ) %>%
    arrange(desc(n_flags), desc(abs(rel_adv))) %>%
    slice_head(n = n)
}

# ============================
# Run analysis
# ============================
out <- find_interesting_problems(df, GROK_NAME, O3_NAME, THRESHOLDS)

# Preview top "interesting" problems
head(out$interesting %>% filter(n_flags > 0) %>%
       select(problem_id, domain, n_flags, flags,
              starts_with(paste0("acc_overall__", GROK_NAME)),
              starts_with(paste0("acc_overall__", O3_NAME))) , 20)

# Save appendix table
appendix_tbl <- top_interesting_table(out, n = 50)
readr::write_csv(appendix_tbl, file.path(OUTDIR, "appendix_interesting_problems.csv"))

# ============================
# Make & save plots
# ============================
p_scatter <- plot_grok_o3_scatter(out); ggsave(file.path(OUTDIR, "scatter_grok_vs_o3.png"), p_scatter, width = 8, height = 6, dpi = 300)
p_adv     <- plot_domain_advantage(out); ggsave(file.path(OUTDIR, "domain_advantage.png"), p_adv, width = 7, height = 5, dpi = 300)
p_sens    <- plot_sensitivity_diffs(out); ggsave(file.path(OUTDIR, "sensitivity_diffs.png"), p_sens, width = 10, height = 6, dpi = 300)

# Example drilldown for the single most-flagged problem (change as you like)
if (nrow(out$interesting) > 0) {
  best_pid <- out$interesting %>% arrange(desc(n_flags), desc(abs(rel_adv))) %>% slice(1) %>% pull(problem_id)
  p_drill  <- plot_problem_drilldown(df, best_pid)
  ggsave(file.path(OUTDIR, paste0("drilldown_problem_", best_pid, ".png")), p_drill, width = 8, height = 5, dpi = 300)
}

message("Done. Outputs saved to: ", normalizePath(OUTDIR))
