library(tidyverse)
library(DescTools)
library(lme4)
library(emmeans)
library(broom.mixed)
library(patchwork)
library(scales)


df <- read_csv("ai4math_runs.csv") %>%                # ❶ load
  mutate(across(c(problem_id, domain, language,       # ❷ factors
                  prompt, model, run),
                ~ factor(.x)))

##############Overall accuracy±95% CI (per model×language×prompt)#############

acc <- df %>%
  group_by(model, language, prompt) %>%
  summarise(n = n(),
            hits = sum(correct),
            .groups = "drop") %>%
  mutate(prop = hits / n,
         ci = map2(hits, n,
                   ~ DescTools::BinomCI(.x, .y, method = "wilson")[,2:3])) %>%
  unnest_wider(ci, names_sep = "_")

# Quick table preview
print(acc, n = 12)

#Save for paper
write_csv(acc, "accuracy.csv")


# Bar plot with error bars
library(ggplot2)
library(scales)

ggplot(acc, aes(x = model, y = prop, fill = language)) +
  geom_col(position = position_dodge(width = .9)) +
  geom_errorbar(aes(ymin = ci_lwr.ci, ymax = ci_upr.ci),
                position = position_dodge(width = .9),
                width = .2) +
  facet_wrap(~prompt) +
  coord_flip() +                                  # <‑‑ solves overlap
  scale_y_continuous(labels = percent_format()) +
  labs(x = NULL,
       y = "Accuracy (± 95 % CI)",
       fill = "Language") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top")                  # keeps legend clear



# Bar plot with error bars V2 (Labeled)
ggplot(acc, aes(x = model, y = prop, fill = language)) +
  geom_col(position = position_dodge(width = .9)) +
  geom_errorbar(aes(ymin = ci_lwr.ci, ymax = ci_upr.ci),
                position = position_dodge(width = .9),
                width = .2) +
  # value labels at the bar start, with padding
  geom_text(
    aes(y = 0, label = scales::percent(prop, accuracy = 0.1)),
    position = position_dodge(width = .9),
    hjust = -0.4,   # move a bit to the right from 0 (padding on the left)
    size = 3
  ) +
  facet_wrap(~prompt) +
  coord_flip() +
  scale_y_continuous(
    labels = percent_format(),
    expand = expansion(mult = c(0.05, 0.02))  # small space at left/right edges
  ) +
  labs(
    x = NULL,
    y = "Accuracy (± 95 % CI)",
    fill = "Language"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top")



#########[Grok-4 REFERENCE] Main significance test: mixed‑effects logistic regression ###############

######### Main significance test: mixed‑effects logistic regression ###############

library(lme4)
library(emmeans)
library(broom.mixed)
library(forcats)
library(ggplot2)
library(scales)
library(stringr)
library(readr)
library(MASS)

## 0.  Re‑level *model* so Grok‑4 is the reference (without altering `df`)
df_grok <- df %>%
  mutate(model = fct_relevel(model, "Grok-4"))

## 1.  Fit the mixed model (robust optimiser)
fit <- glmer(
  correct ~ model * language * prompt +
    (1 | problem_id) + (1 | problem_id:run),
  family  = binomial,
  data    = df_grok,
  control = glmerControl(optimizer = "bobyqa",
                         optCtrl   = list(maxfun = 2e5))
)


#Getting the best model: Variable Evaluation for the model

library(lme4)
fit <- glmer(correct ~ model * language * prompt +
               (1 | problem_id) + (1 | problem_id:run),
             family = binomial, data = df)

fit2 <- glmer(correct ~ model * language + prompt +
                (1 | problem_id) + (1 | problem_id:run),
              family = binomial, data = df)

fit3 <- glmer(correct ~ model + language * prompt +
                (1 | problem_id) + (1 | problem_id:run),
              family = binomial, data = df)

fit4 <- glmer(correct ~ model + language + prompt +
                (1 | problem_id) + (1 | problem_id:run),
              family = binomial, data = df)

fit5 <- glmer(correct ~ model + language +
                (1 | problem_id) + (1 | problem_id:run),
              family = binomial, data = df)

anova (fit,fit2,fit3,fit4,fit5)



#compute BIC, ΔBIC, and BIC weights
# Collect models
mods <- list(
  `model * language * prompt` = fit,
  `model * language + prompt` = fit2,
  `model + language * prompt` = fit3,
  `model + language + prompt` = fit4,
  `model + language` = fit5
)

# BIC table
bic_tab <- data.frame(
  model = names(mods),
  BIC   = sapply(mods, BIC),
  df    = sapply(mods, function(m) attr(logLik(m), "df"))
)
bic_tab <- bic_tab[order(bic_tab$BIC), ]
bic_tab$deltaBIC <- bic_tab$BIC - min(bic_tab$BIC)

# BIC weights (Raftery/Schwarz analog to Akaike weights)
bic_tab$wBIC <- {
  relL <- exp(-0.5 * bic_tab$deltaBIC)
  relL / sum(relL)
}
bic_tab

write.csv (bic_tab, file = "mixed_model_BIC.csv")

#ΔBIC Plot

library(ggplot2)

ggplot(bic_tab, aes(x = reorder(model, deltaBIC), y = deltaBIC)) +
  geom_col(fill = "#00bfc4") +
  geom_text(aes(label = round(deltaBIC, 2)),
            hjust = -0.1, size = 3.5) +
  coord_flip() +
  labs(x = NULL, y = expression(Delta*BIC),
       title = "Model comparison by ΔBIC (lower is better)") +
  theme_minimal() +
  expand_limits(y = max(bic_tab$deltaBIC) * 1.1)   # add 10% headroom






# Pairwise model contrasts within each language‑prompt slice
library(emmeans)

emm  <- emmeans(fit5, ~ model | language )
ctrs <- pairs(emm, adjust = "holm") %>%
  summary(infer = TRUE)            # odds ratios & CI


# Save for paper
write_csv(ctrs, "pairwise_contrasts.csv")

# Forest plot against a reference (change "Grok‑4" if needed)
ref <- "Grok‑4"

forest <- broom.mixed::tidy(fit5, conf.int = TRUE, effects="fixed") %>%
  filter(str_detect(term, "^model")) %>%
  mutate(model = str_remove(term, "^model")) %>%
  ggplot(aes(y = reorder(model, estimate),
             x = exp(estimate))) +
  geom_vline(xintercept = 1, lty = 2) +
  geom_errorbarh(aes(xmin = exp(conf.low),
                     xmax = exp(conf.high)), height = .2) +
  geom_point(size = 2) +
  scale_x_log10(labels = scales::number_format(accuracy=0.1)) +
  labs(x = paste("Odds ratio vs", ref),
       y = NULL, title = "Overall model comparison") +
  theme_minimal(base_size = 11)

#colored plot
forest <- broom.mixed::tidy(fit5, conf.int = TRUE, effects="fixed") %>%
  filter(str_detect(term, "^model")) %>%
  mutate(model = str_remove(term, "^model")) %>%
  ggplot(aes(y = reorder(model, estimate),
             x = exp(estimate))) +
  geom_vline(xintercept = 1, lty = 2) +
  geom_errorbarh(
    aes(xmin = exp(conf.low), xmax = exp(conf.high)),
    height = .2,
    color = "#0072B2"   # error bars
  ) +
  geom_point(
    size = 2,
    color = "#0072B2"   # points
  ) +
  scale_x_log10(labels = scales::number_format(accuracy = 0.1)) +
  labs(x = paste("Odds ratio vs", ref),
       y = NULL, title = "Overall model comparison") +
  theme_minimal(base_size = 11)







print(forest)





## 2.  Marginal means per model  → odds ratio vs Grok‑4
emm_model  <- emmeans(fit, ~ model)        # log‑odds scale
contr_grok <- contrast(emm_model,
                       method = "trt.vs.ctrl",   # <- change here
                       ref    = "Grok-4")

or_df <- summary(contr_grok, infer = c(TRUE, TRUE)) %>%
  mutate(
    model      = str_remove(contrast, " - Grok-4"),
    odds_ratio = exp(estimate),
    lower      = exp(asymp.LCL),
    upper      = exp(asymp.UCL)
  ) %>%
  select(model, odds_ratio, lower, upper, p.value)


write_csv(or_df, "pairwise_contrasts_grok4.csv")    # ready for paper




## 3.  Forest plot of odds ratios vs Grok‑4
ref <- "Grok‑4"

forest <- or_df %>%
  ggplot(aes(y = reorder(model, odds_ratio),
             x = odds_ratio)) +
  geom_vline(xintercept = 1, lty = 2) +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = .2) +
  geom_point(size = 2) +
  scale_x_log10(labels = scales::number_format(accuracy = 0.1)) +
  labs(x = "Odds ratio vs Grok 4",
       y = NULL,
       title = "Overall model comparison\n(marginal over language & prompt)") +
  theme_minimal(base_size = 11)

#colored plot
forest <- or_df %>%
  ggplot(aes(y = reorder(model, odds_ratio),
             x = odds_ratio)) +
  geom_vline(xintercept = 1, lty = 2) +
  geom_errorbarh(
    aes(xmin = lower, xmax = upper),
    height = .2,
    color = "#0072B2"   # error bars
  ) +
  geom_point(
    size = 2,
    color = "#0072B2"   # points
  ) +
  scale_x_log10(labels = scales::number_format(accuracy = 0.1)) +
  labs(
    x = "Odds ratio vs Grok 4",
    y = NULL,
    title = "Overall model comparison\n(marginal over language & prompt)"
  ) +
  theme_minimal(base_size = 11)





print(forest)





##########################Domain‑level diagnostics##############################################

############################################
## STEP 4 — Mixed model + domain analysis ##
############################################

library(lme4)
library(car)         # for Anova(type = 3)
library(emmeans)     # for domain‑specific contrasts
library(dplyr)
library(ggplot2)
library(scales)      # percent_format()
library(viridis)     # for viridis palette (if not installed: install.packages("viridis"))

#df <- read_csv("ai4math_runs_english.csv") %>%                # ❶ load
  mutate(across(c(problem_id, domain, language,       # ❷ factors
                  prompt, model, run),
                ~ factor(.x)))

## 0.  Make sure 'domain' is a factor
  # --- Packages ---
  library(lme4); library(dplyr); library(ggplot2); library(scales); library(emmeans)
  
  # --- Factors (keep language as a column if present; not used in formulas) ---
  df_dom <- df %>%
    mutate(across(c(problem_id, domain, model, run), factor))
  
  # --- Two nested models for domain analysis (no Language) ---
  fit_add <- glmer(
    correct ~ model + domain +
      (1 | problem_id) + (1 | problem_id:run),
    family = binomial, data = df_dom,
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
  )
  
  fit_int <- glmer(
    correct ~ model * domain +
      (1 | problem_id) + (1 | problem_id:run),
    family = binomial, data = df_dom,
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
  )
  
  # --- BIC table, ΔBIC, and weights ---
  bic_tab <- tibble::tibble(
    model = c("additive: model + domain",
              "interaction: model × domain"),
    BIC   = c(BIC(fit_add), BIC(fit_int))
  ) %>%
    arrange(BIC) %>%
    mutate(
      deltaBIC = BIC - min(BIC),
      wBIC = { rl <- exp(-0.5 * deltaBIC); rl / sum(rl) }
    )
  
  print(bic_tab)
  
  # --- Auto-sentence for Results ---
  best <- bic_tab[1, ]; nextbest <- bic_tab[2, ]
  cat(sprintf(
    "Model selection (BIC). We compared two nested mixed-effects logistic regression models varying the treatment of domain. The model with the lowest BIC was %s (BIC = %.1f), which outperformed the alternative with ΔBIC = %.1f and a BIC weight of %.2f. According to conventional guidelines (ΔBIC > 6), this provides %s evidence in favor of the %s specification. All models included random intercepts for problem_id and problem_id:run.",
    best$model, best$BIC, nextbest$deltaBIC, best$wBIC,
    ifelse(nextbest$deltaBIC > 10, "very strong",
           ifelse(nextbest$deltaBIC > 6, "strong", "positive")),
    ifelse(grepl("^additive", best$model), "additive", "interaction")
  ))
  
  # --- ΔBIC Plot ---
  ggplot(bic_tab, aes(x = reorder(model, deltaBIC), y = deltaBIC)) +
    geom_col() +
    coord_flip() +
    labs(x = NULL, y = expression(Delta*BIC),
         title = "Domain analysis: model comparison by ΔBIC (lower is better)") +
    theme_minimal()
  
  # --- ΔBIC Plot Color adjusted ---
  ggplot(bic_tab, aes(x = reorder(model, deltaBIC), y = deltaBIC)) +
    geom_col(fill = "#00bfc4") +
    geom_text(aes(label = round(deltaBIC, 2)),
              hjust = -0.1, size = 3.5) +
    coord_flip(clip = "off") +   # ensures labels outside bars are visible
    labs(x = NULL, y = expression(Delta*BIC),
         title = "Domain analysis: model comparison by ΔBIC (lower is better)") +
    theme_minimal() +
    theme(plot.margin = margin(5.5, 30, 5.5, 5.5))   # add padding on right
  


## 2.  Order‑independent (type‑III) Wald χ² tests
anova_dom <- car::Anova(fit_dom2, type = 3)
print(anova_dom)

# Example console output:
#                 Chisq Df Pr(>Chisq)
# model          110.28  7  <2e-16 ***
# domain          93.65  6  <2e-16 ***
# model:domain    54.10 42   0.040 *

## 3.  Domain heat‑map (unchanged, but now using df_dom)
dom_plot <- df_dom %>%
  group_by(model, domain) %>%
  summarise(acc = mean(correct), .groups = "drop") %>%
  ggplot(aes(domain, model, fill = acc)) +
  geom_tile() +
  geom_text(aes(label = percent(acc, accuracy = 1)),
            colour = "white", size = 5) +
  scale_fill_viridis_c(name = "Accuracy",
                       limits = c(0, 1),
                       labels = percent_format(accuracy = 1)) +
  labs(x = NULL, y = NULL,
       title = "Accuracy per Domain and Model") +
  theme_minimal(base_size = 14)

print(dom_plot)



#3_1 Domain difficulty (marginal means from the GLMM)

library(emmeans); library(dplyr); library(ggplot2); library(scales)

# Selected model without Language:
# fit_add: correct ~ model + domain + (1|problem_id) + (1|problem_id:run)

dom_mm <- emmeans(fit_add, ~ domain, type = "response") %>% as.data.frame()
# dom_mm has: domain, prob (=P(correct)), SE, asymp.LCL, asymp.UCL

ggplot(dom_mm, aes(x = reorder(domain, prob), y = prob)) +
  geom_col() +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = .15) +
  coord_flip() +
  scale_y_continuous(labels = percent) +
  labs(x = NULL, y = "Estimated accuracy (marginal mean)",
       title = "Baseline domain difficulty (pooled across models)")


#Better plot
library(dplyr)
library(forcats)
library(scales)
library(ggplot2)

pad <- 0.02  # ~2 percentage points of padding

dom_plot <- dom_mm %>%
  mutate(
    domain = fct_reorder(domain, prob),
    label   = percent(prob, accuracy = 1),
    inside  = prob >= 0.15,
    label_x = ifelse(inside, pmax(prob - pad, 0), prob + pad),
    hjust_x = ifelse(inside, 1, 0),
    col_x   = ifelse(inside, "white", "black")
  )

ggplot(dom_plot, aes(x = domain, y = prob)) +
  geom_col(width = 0.7, fill = "#00bfc4") +
  # 25% transparency on error bars (i.e., 75% opacity)
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                width = 0.2, alpha = 0.50) +
  coord_flip() +
  geom_text(aes(y = label_x, label = label, hjust = hjust_x, colour = col_x),
            size = 4.2, fontface = "bold", show.legend = FALSE) +
  scale_colour_identity() +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x = NULL, y = "Estimated accuracy (marginal mean)",
    title = "Baseline domain difficulty (pooled across models)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  )

#summary
library(emmeans); library(dplyr); library(scales)

dom_mm <- emmeans(fit_add, ~ domain, type = "response") %>% as.data.frame() %>%
  arrange(prob)  # low = harder

write.csv (dom_mm, file= "Domain_marginal_means.csv")  # table for the supplement paper

# One-liner for Results:
hardest <- dom_mm$domain[1]; hardest_p <- percent(dom_mm$prob[1], 1)
easiest <- dom_mm$domain[nrow(dom_mm)]; easiest_p <- percent(dom_mm$prob[nrow(dom_mm)], 1)
sprintf("Baseline domain difficulty (marginal means) varies substantially: %s is hardest (p̂ = %s) and %s is easiest (p̂ = %s).",
        hardest, hardest_p, easiest, easiest_p)





#3_2 Model robustness across domains (dispersion)

acc_by_cell <- df_dom %>%
  group_by(model, domain) %>%
  summarise(acc = mean(correct), .groups="drop")

robust_tbl <- acc_by_cell %>%
  group_by(model) %>%
  summarise(
    mean_acc = mean(acc),
    sd_acc   = sd(acc),
    iqr_acc  = IQR(acc),
    min_acc  = min(acc),
    max_acc  = max(acc),
    .groups="drop"
  ) %>% arrange(desc(mean_acc))

robust_tbl

write.csv(robust_tbl, "Model_robustness_across_domains.csv")





## 4.  OPTIONAL — domain‑specific pairwise contrasts (Holm‑adjusted)
#     Here we compare every pair of models *within* each domain on the response scale.
emm_dom <- emmeans(fit_dom, ~ model | domain, type = "response")

# All pairwise comparisons inside each domain, Holm correction
dom_contrasts <- pairs(emm_dom, adjust = "holm") %>%
  summary(infer = TRUE)

# Preview first few rows
print(head(dom_contrasts), n = 10)

# If you only want comparisons vs a reference (e.g., Grok‑4) inside each domain:
dom_contrasts <- contrast(emm_dom, method = "trt.vs.ctrl", ref = "Grok-4",
                          adjust = "holm") %>% summary(infer = TRUE)

# You can write these to CSV for the supplement:
write_csv(dom_contrasts, "domain_specific_contrasts_grok4.csv")


##############################################
## Domain‑specific contrasts plot vs Grok‑4 (OR) ##
##############################################

library(emmeans)
library(dplyr)
library(ggplot2)
library(scales)
library(stringr)
library(viridis)

#BEST MODEL#
fit_add

## 1.  Get emmeans on the *link* scale (default) → contrasts vs Grok‑4
emm_dom <- emmeans(fit_add, ~ model | domain)  # no type = "response"

dom_contrasts <- contrast(emm_dom,
                          method = "trt.vs.ctrl",
                          ref    = "Grok-4",
                          adjust = "holm") %>%
  summary(infer = TRUE)         # gives estimate + CI

## 2.  Prepare data for plotting
plot_df <- dom_contrasts %>%
  mutate(
    model      = str_remove(contrast, " - Grok-4"),
    odds_ratio = exp(estimate),
    lower      = exp(asymp.LCL),
    upper      = exp(asymp.UCL),
    sig        = ifelse(p.value < 0.05, "sig", "ns")
  )

## 3.  Faceted forest‑plot
ggplot(plot_df,
       aes(x = odds_ratio, y = reorder(model, odds_ratio), colour = sig)) +
  geom_vline(xintercept = 1, linetype = 2) +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = .2) +
  geom_point(size = 2) +
  scale_x_log10(labels = number_format(accuracy = 0.1)) +
  scale_colour_manual(values = c(sig = "#0072B2", ns = "grey60"),
                      name   = "Holm adj.\n p < 0.05",
                      labels = c("No", "Yes")) +
  facet_wrap(~ domain, ncol = 2, scales = "free_y") +
  labs(x = "Odds ratio vs Grok 4 (log scale)",
       y = NULL,
       title = "Domain specific model performance") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top",
        strip.text = element_text(face = "bold"))
































#######################[Entropy] Run‑to‑run stability analysis###############################################

library(entropy)   # install.packages("entropy")

stab <- df %>%
  group_by(problem_id, model, language, prompt) %>%
  summarise(ent = entropy::entropy(tabulate(correct + 1,
                                            nbins = 2), unit="log2"),
            .groups = "drop")

# quick strip chart
ggplot(stab, aes(x = model, y = ent,
                 colour = language)) +
  geom_jitter(width = .2, height = 0, alpha = .4) +
  stat_summary(fun = median, geom = "point",
               shape = 95, size = 6, colour = "black") +
  facet_wrap(~prompt) +
  labs(y = "Shannon entropy (0=deterministic)",
       x = NULL) +
  theme_minimal(base_size = 11)

# Beta mixed model (glmmTMB supports zero/one inflation if needed)
library(glmmTMB)

# Rescale entropy to 0-1
stab <- stab %>%
  mutate(ent_scaled = ent / log2(3))

# Apply zero/one adjustment: shrink values at the boundaries
eps <- 1e-4
stab <- stab %>%
  mutate(ent_scaled = ifelse(ent_scaled == 0, eps,
                             ifelse(ent_scaled == 1, 1 - eps, ent_scaled)))

fit_stab <- glmmTMB(ent_scaled ~ model * language * prompt +
                      (1 | problem_id),
                    family = beta_family(link = "logit"), data = stab)
summary(fit_stab)



###Cleaner code to summarize and visual
library(dplyr)
library(ggplot2)
library(entropy)
library(scales)

## 1.  Compute entropy & a simple ‘flip’ indicator
stab <- df %>%
  group_by(problem_id, model, language, prompt) %>%
  summarise(
    ent  = entropy(tabulate(correct + 1, nbins = 2), unit = "log2"),
    flip = as.integer(ent > 0),    # 1 if model disagreed with itself
    .groups = "drop"
  )

## 2.  Quick table: % of problems with any flip
flip_summary <- stab %>%
  group_by(model, language, prompt) %>%
  summarise(p_flip = mean(flip), .groups = "drop")

print(flip_summary %>% arrange(desc(p_flip)), n = 20)

write_csv(flip_summary, "flip_summary_entropy.csv")    # ready for paper

## 3.  Plot *percentage of flips* instead of raw entropy
ggplot(flip_summary, aes(model, p_flip, fill = language)) +
  geom_col(position = position_dodge(width = .8)) +
  facet_wrap(~ prompt) +
  scale_y_continuous(labels = percent_format()) +
  labs(y = "% problems with inconsistent prediction (3 runs)",
       x = NULL,
       title = "Run to run instability across models") +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





###########REPEATING MATTER?####################

#Run‑to‑Run Reliability (Intra‑Class Correlation, ICC)
install.packages(c("irr", "purrr", "patchwork"))

library(tidyverse); library(irr)

icc_tab <- df %>%
   pivot_wider(id_cols = c(problem_id, model, language, prompt),
               names_from  = run,
               values_from = correct,
               names_prefix = "run_") %>%
   group_split(model, language, prompt) %>%
   map_dfr(~{
     mat <- .x %>% select(starts_with("run_")) %>% drop_na() %>% as.matrix()
     if (nrow(mat) < 2) return(NULL)
     icc_val <- irr::icc(mat, model = "twoway", type = "consistency",
                         unit = "single")$value
     tibble(model = .x$model[1],
            language = .x$language[1],
            prompt = .x$prompt[1],
            ICC = icc_val)
   })
 

print(icc_tab, n = 20)      # table for the paper

#reader‑friendly table
library(dplyr)
library(knitr)
library(kableExtra)

# convert ICC to qualitative label
icc_tab_disp <- icc_tab %>%
  mutate(Quality = cut(ICC,
                       breaks = c(-Inf, .60, .75, .90, Inf),
                       labels = c("Poor", "Moderate", "Good", "Excellent"))) %>%
  arrange(desc(ICC))                             # best at top

write_csv(icc_tab_disp,
          file = "TableS2_ICC_full.csv")


library(ggplot2)
plot_df <- icc_tab_disp %>%
  mutate(slice = reorder(paste(model, language, prompt, sep = " / "), ICC))

ggplot(plot_df,
       aes(x = ICC, y = slice, colour = Quality)) +
  geom_segment(aes(xend = 0.6, yend = slice),   # use the new column
               linetype = 3, colour = "grey80") +
  geom_point(size = 3) +
  scale_colour_manual(values = c(Poor = "#d73027",
                                 Moderate = "#fc8d59",
                                 Good = "#4575b4",
                                 Excellent = "#1a9850")) +
  labs(x = "Intra class correlation",
       y = NULL,
       title = "Reliability (ICC) for every model / language / prompt slice") +
  theme_minimal(base_size = 10) +
  theme(legend.position = "top")








#Rank‑Instability Probability


## majority‑vote "ground truth" ranking
truth_rank <- df %>%
  group_by(model, language, prompt) %>%
  summarise(acc = mean(correct), .groups="drop") %>%
  group_by(language, prompt) %>%
  arrange(desc(acc)) %>%
  summarise(order_truth = list(model), .groups="drop")

## ranking from each single run
run_rank <- df %>%
  group_by(run, model, language, prompt) %>%
  summarise(acc = mean(correct), .groups="drop") %>%
  group_by(run, language, prompt) %>%
  arrange(desc(acc)) %>%
  summarise(order_run = list(model), .groups="drop")

## compare
rank_instab <- run_rank %>%
  left_join(truth_rank, by = c("language", "prompt")) %>%
  mutate(disagree = map2_int(order_run, order_truth,
                             ~ sum(.x != .y)))   # count mismatches

p_instab <- mean(rank_instab$disagree > 0)
p_instab   # e.g. 0.12 means 12 % of slices mis‑order at least one pair

p_instab <- mean(rank_instab$disagree > 0)
n_slices <- nrow(rank_instab)
cat(sprintf("Rank instability: %.1f%% of %d slices\n",
            p_instab * 100, n_slices))









#Sign‑Flip Rate in Pairwise Significance
library(lme4); library(broom.mixed); library(purrr)

## helper: model & significance table for a data frame
get_sig <- function(d) {
  fit5 <- glmer(correct ~ model + language +
                  (1 | problem_id) + (1 | problem_id:run),
                family = binomial, data = d)
  
  tidy(fit, conf.int = TRUE) %>%
    filter(str_detect(term, "^model")) %>%
    mutate(sig = conf.low * conf.high > 0) %>%
    select(term, sig)
}

sig_multi  <- get_sig(df)               # 3‑run reference
sig_single <- map_dfr(1:3, ~ get_sig(df %>% filter(run == .x)) %>%
                        mutate(run = .x))

flip_rate <- sig_single %>%
  left_join(sig_multi, by = "term", suffix = c("_single", "_multi")) %>%
  mutate(flip = sig_single != sig_multi) %>%
  group_by(term) %>%              # one line per contrast
  summarise(flip = any(flip)) %>%
  summarise(rate = mean(flip)) %>%
  pull(rate)


flip_rate   # e.g. 0.08  →  8 % of contrasts flip significance




#Precision-vs-Compute Curve#
df_num <- df %>% mutate(run = as.integer(as.character(run)))


precision_df <- map_dfr(1:3, function(R) {
  d <- df_num %>% filter(run <= R)
  se <- d %>%
    group_by(model, language, prompt) %>%
    summarise(acc = mean(correct), .groups = "drop") %>%
    summarise(se = sd(acc) / sqrt(n())) %>%     # SE per slice
    pull(se) %>% mean()                         # average across slices
  tibble(runs = R, se = se)
})


ggplot(precision_df, aes(runs, se)) +
  geom_line() + geom_point(size = 3) +
  labs(x = "Number of runs averaged",
       y = "Average standard error of accuracy",
       title = "Stability improvement from additional runs") +
  theme_minimal(base_size = 11)


#Precision-vs-Compute Curve v.2#
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(ggplot2)

# 0) Ensure run is integer
df_num <- df %>% mutate(run = as.integer(as.character(run)))

# 1) Get per-slice accuracies per run + slice size
by_run_slice <- df_num %>%
  group_by(model, language, prompt, run) %>%
  summarise(acc = mean(correct), n_items = n(), .groups = "drop")

# 2) For each slice, estimate its across-run SD using ALL runs available
#    Keep only slices with >= 2 runs (otherwise SD is undefined)
by_slice <- by_run_slice %>%
  group_by(model, language, prompt) %>%
  filter(n_distinct(run) >= 2) %>%
  summarise(sd_all = sd(acc),
            weight = sum(n_items),    # weight slices by total evaluated items
            .groups = "drop")

# 3) Predict SE for averaging R runs: SE_R = sd_all / sqrt(R)
runs_seq <- seq(1, max(df_num$run, na.rm = TRUE))

precision_pred <- expand_grid(model = unique(by_slice$model),
                              runs  = runs_seq) %>%
  left_join(by_slice, by = "model") %>%
  group_by(model, runs) %>%
  summarise(se_pred = weighted.mean(sd_all / sqrt(runs), weight, na.rm = TRUE),
            .groups = "drop")

# 4) Plot the prediction (monotone decreasing in R)
ggplot(precision_pred, aes(runs, se_pred, color = model)) +
  geom_line() +
  geom_point(size = 2.5) +
  scale_x_continuous(breaks = runs_seq) +
  labs(x = "Number of runs averaged (R)",
       y = "Predicted avg per-slice SE across runs",
       title = "Stability improvement from additional runs (predicted, per model)") +
  theme_minimal(base_size = 11)


empirical <- map_dfr(runs_seq[runs_seq >= 2], function(R) {
  d <- by_run_slice %>% filter(run <= R)
  
  # only slices that have >= R runs for that model
  keep <- d %>%
    count(model, language, prompt, name = "n_runs") %>%
    filter(n_runs >= R) %>%
    select(model, language, prompt)
  
  d2 <- d %>% semi_join(keep, by = c("model","language","prompt"))
  
  d2 %>%
    group_by(model, language, prompt) %>%
    summarise(se_slice = sd(acc) / sqrt(n()),
              weight   = sum(n_items),
              .groups = "drop") %>%
    group_by(model) %>%
    summarise(se_emp = weighted.mean(se_slice, weight, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(runs = R)
})

ggplot() +
  geom_line(data = precision_pred,
            aes(runs, se_pred, color = model), size = 0.9) +
  geom_point(data = precision_pred,
             aes(runs, se_pred, color = model), size = 2.5) +
  geom_line(data = empirical,
            aes(runs, se_emp, color = model), linetype = 2) +
  geom_point(data = empirical,
             aes(runs, se_emp, color = model), shape = 1, size = 2.3) +
  scale_x_continuous(breaks = runs_seq) +
  labs(x = "Number of runs averaged (R)",
       y = "Average per-slice SE across runs",
       title = "Predicted vs empirical stability (per model)",
       subtitle = "Solid = predicted (sd_all/√R), dashed = empirical") +
  theme_minimal(base_size = 11)



#Stability v.3#
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(ggplot2)

# Ensure run is integer
df_num <- df %>% mutate(run = as.integer(as.character(run)))

# Accuracy per slice per run + slice size
by_run_slice <- df_num %>%
  group_by(model, language, prompt, run) %>%
  summarise(acc = mean(correct), n_items = n(), .groups = "drop")

runs_seq <- sort(unique(df_num$run))

# --- Predicted per-model SE under IID: se = sd_all / sqrt(R) ---
by_slice <- by_run_slice %>%
  group_by(model, language, prompt) %>%
  filter(n_distinct(run) >= 2) %>%               # need >=2 runs to estimate sd
  summarise(sd_all = sd(acc), weight = sum(n_items), .groups = "drop")

pred <- expand_grid(model = unique(by_slice$model), runs = runs_seq) %>%
  left_join(by_slice, by = "model") %>%
  group_by(model, runs) %>%
  summarise(se = weighted.mean(sd_all / sqrt(runs), weight, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(kind = "Predicted")

# --- Empirical per-model SE using exactly the first R runs (apples-to-apples) ---
emp <- map_dfr(runs_seq[runs_seq >= 2], function(R) {
  d <- by_run_slice %>% filter(run <= R)
  
  keep <- d %>%
    count(model, language, prompt, name = "n_runs") %>%
    filter(n_runs >= R) %>%
    select(model, language, prompt)
  
  d %>% semi_join(keep, by = c("model","language","prompt")) %>%
    group_by(model, language, prompt) %>%
    summarise(se_slice = sd(acc) / sqrt(n()),
              weight   = sum(n_items), .groups = "drop") %>%
    group_by(model) %>%
    summarise(se = weighted.mean(se_slice, weight, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(runs = R, kind = "Empirical")
})

# --- Model-averaged lines (so you keep the original "overall" story) ---
avg_lines <- bind_rows(pred, emp) %>%
  group_by(kind, runs) %>%
  summarise(se = mean(se, na.rm = TRUE), .groups = "drop")

# Plot
ggplot() +
  # per-model predicted (solid) and empirical (dashed)
  geom_line(data = pred, aes(runs, se, color = model), size = 0.8) +
  geom_point(data = pred, aes(runs, se, color = model), size = 2) +
  geom_line(data = emp,  aes(runs, se, color = model), linetype = 2, size = 0.8) +
  geom_point(data = emp, aes(runs, se, color = model), shape = 1, size = 2) +
  
  # thick black model-averaged lines
  geom_line(data = filter(avg_lines, kind=="Predicted"),
            aes(runs, se), color = "black", size = 1.4) +
  geom_point(data = filter(avg_lines, kind=="Predicted"),
             aes(runs, se), color = "black", size = 2.6) +
  geom_line(data = filter(avg_lines, kind=="Empirical"),
            aes(runs, se), color = "black", linetype = 2, size = 1.2) +
  geom_point(data = filter(avg_lines, kind=="Empirical"),
             aes(runs, se), color = "black", shape = 1, size = 2.4) +
  
  scale_x_continuous(breaks = runs_seq) +
  labs(
    title    = "Stability improvement from additional runs (per model + average)",
    subtitle = "Solid = predicted IID (sd_all/√R).  Dashed = empirical across runs.\nThick black line = model-averaged.",
    x = "Number of runs averaged (R)",
    y = "Average per-slice SE across runs"
  ) +
  theme_minimal(base_size = 11)


# Stability v.4 — apples-to-apples across R
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(ggplot2)

# Ensure types
df_num <- df %>%
  mutate(
    run = as.integer(as.character(run)),
    correct = as.numeric(correct)
  )

# Accuracy per slice per run + slice size
by_run_slice <- df_num %>%
  group_by(model, language, prompt, run) %>%
  summarise(acc = mean(correct), n_items = n(), .groups = "drop")

# Run labels and how many distinct runs we have
run_ids <- sort(unique(by_run_slice$run))
Rmax    <- length(run_ids)
Rs      <- 1:Rmax

# ---- keep only slices that have ALL runs (constant composition across R) ----
complete_slices <- by_run_slice %>%
  count(model, language, prompt, name = "n_runs") %>%
  filter(n_runs == Rmax) %>%
  select(model, language, prompt)

by_run_slice_complete <- by_run_slice %>%
  semi_join(complete_slices, by = c("model","language","prompt"))

# ---- Predicted per-model SE under IID: se = sd_all / sqrt(R) ----
# Use all runs to estimate each slice's across-run SD; weight by mean items/run
by_slice <- by_run_slice_complete %>%
  group_by(model, language, prompt) %>%
  summarise(
    sd_all = sd(acc),
    weight = mean(n_items),
    .groups = "drop"
  )

pred <- expand_grid(model = unique(by_slice$model), R = Rs) %>%
  left_join(by_slice, by = "model") %>%
  group_by(model, R) %>%
  summarise(
    se = weighted.mean(sd_all / sqrt(R), weight, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(kind = "Predicted")

# ---- Empirical per-model SE using exactly the first R runs (same slices) ----
emp <- map_dfr(2:Rmax, function(i) {
  use_runs <- run_ids[1:i]
  
  by_run_slice_complete %>%
    filter(run %in% use_runs) %>%
    group_by(model, language, prompt) %>%
    summarise(
      se_slice = sd(acc) / sqrt(i),    # SE of the mean across i runs
      weight   = mean(n_items),
      .groups  = "drop"
    ) %>%
    group_by(model) %>%
    summarise(se = weighted.mean(se_slice, weight, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(R = i, kind = "Empirical")
})

# ---- Model-averaged lines ----
avg_lines <- bind_rows(pred, emp) %>%
  group_by(kind, R) %>%
  summarise(se = mean(se, na.rm = TRUE), .groups = "drop")

# ---- Plot ----
ggplot() +
  # per-model predicted (solid) and empirical (dashed)
  geom_line(data = pred, aes(R, se, color = model), size = 0.8) +
  geom_point(data = pred, aes(R, se, color = model), size = 2) +
  geom_line(data = emp,  aes(R, se, color = model), linetype = 2, size = 0.8) +
  geom_point(data = emp, aes(R, se, color = model), shape = 1, size = 2) +
  
  # thick black model-averaged lines
  geom_line(data = filter(avg_lines, kind=="Predicted"),
            aes(R, se), color = "black", size = 1.4) +
  geom_point(data = filter(avg_lines, kind=="Predicted"),
             aes(R, se), color = "black", size = 2.6) +
  geom_line(data = filter(avg_lines, kind=="Empirical"),
            aes(R, se), color = "black", linetype = 2, size = 1.2) +
  geom_point(data = filter(avg_lines, kind=="Empirical"),
             aes(R, se), color = "black", shape = 1, size = 2.4) +
  
  scale_x_continuous(breaks = Rs) +
  labs(
    title    = "Stability improvement from additional runs (per model + average)",
    subtitle = "Solid = predicted IID (sd_all/√R).  Dashed = empirical on the same slices (first R runs).\nThick black = model-averaged.",
    x = "Number of runs averaged (R)",
    y = "Average per-slice SE across runs"
  ) +
  theme_minimal(base_size = 11)


# Stability v.5 — apples-to-apples + all-pairs empirical
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(ggplot2)

# Ensure types
df_num <- df %>%
  mutate(
    run = as.integer(as.character(run)),
    correct = as.numeric(correct)
  )

# Accuracy per slice per run + slice size
by_run_slice <- df_num %>%
  group_by(model, language, prompt, run) %>%
  summarise(acc = mean(correct), n_items = n(), .groups = "drop")

run_ids <- sort(unique(by_run_slice$run))
Rmax    <- length(run_ids)
Rs      <- 1:Rmax

# ---- fixed slice set: only slices that have ALL runs ----
complete_slices <- by_run_slice %>%
  count(model, language, prompt, name = "n_runs") %>%
  filter(n_runs == Rmax) %>%
  select(model, language, prompt)

by_run_slice_complete <- by_run_slice %>%
  semi_join(complete_slices, by = c("model","language","prompt"))

# ---- Predicted SE under IID: se = sd_all / sqrt(R) (weights = mean items/run) ----
by_slice <- by_run_slice_complete %>%
  group_by(model, language, prompt) %>%
  summarise(
    sd_all = sd(acc),
    weight = mean(n_items),
    .groups = "drop"
  )

pred <- expand_grid(model = unique(by_slice$model), R = Rs) %>%
  left_join(by_slice, by = "model") %>%
  group_by(model, R) %>%
  summarise(
    se = weighted.mean(sd_all / sqrt(R), weight, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(kind = "Predicted")

# ---- Empirical SE using all pairs for R=2, and all 3 runs for R=3 (same slices) ----
emp_slices <- by_run_slice_complete %>%
  group_by(model, language, prompt) %>%
  summarise(
    # R = 2: average SE over all 3 pairs: {1,2}, {1,3}, {2,3}
    se2 = {
      xs <- acc[order(run)]
      if (length(xs) >= 2) {
        pr <- combn(xs, 2)
        mean(apply(pr, 2, function(v) sd(v) / sqrt(2)), na.rm = TRUE)
      } else NA_real_
    },
    # R = 3: SE from all three runs
    se3 = if (n() >= 3) sd(acc) / sqrt(3) else NA_real_,
    weight = mean(n_items),
    .groups = "drop"
  )

emp <- bind_rows(
  emp_slices %>%
    group_by(model) %>%
    summarise(se = weighted.mean(se2, weight, na.rm = TRUE), .groups = "drop") %>%
    mutate(R = 2, kind = "Empirical"),
  emp_slices %>%
    group_by(model) %>%
    summarise(se = weighted.mean(se3, weight, na.rm = TRUE), .groups = "drop") %>%
    mutate(R = 3, kind = "Empirical")
)

# ---- Model-averaged lines ----
avg_lines <- bind_rows(pred, emp) %>%
  group_by(kind, R) %>%
  summarise(se = mean(se, na.rm = TRUE), .groups = "drop")

# ---- Plot ----
ggplot() +
  geom_line(data = pred, aes(R, se, color = model), size = 0.8) +
  geom_point(data = pred, aes(R, se, color = model), size = 2) +
  geom_line(data = emp,  aes(R, se, color = model), linetype = 2, size = 0.8) +
  geom_point(data = emp, aes(R, se, color = model), shape = 1, size = 2) +
  geom_line(data = filter(avg_lines, kind=="Predicted"),
            aes(R, se), color = "black", size = 1.4) +
  geom_point(data = filter(avg_lines, kind=="Predicted"),
             aes(R, se), color = "black", size = 2.6) +
  geom_line(data = filter(avg_lines, kind=="Empirical"),
            aes(R, se), color = "black", linetype = 2, size = 1.2) +
  geom_point(data = filter(avg_lines, kind=="Empirical"),
             aes(R, se), color = "black", shape = 1, size = 2.4) +
  scale_x_continuous(breaks = Rs) +
  labs(
    title    = "Stability improvement from additional runs (per model + average)",
    subtitle = "Solid = predicted IID (sd_all/√R).  Dashed = empirical using all run pairs for R=2 and all runs for R=3.\nThick black = model-averaged.",
    x = "Number of runs averaged (R)",
    y = "Average per-slice SE across runs"
  ) +
  theme_minimal(base_size = 11)













#Visual Rank Instability#
library(tidyverse)

## ------------------------------------------------------------------
## 1.  Build single-run ranks (run1-run3) – identical to earlier
## ------------------------------------------------------------------
rank_run_all <- map_dfr(1:3, function(r) {
  df %>%                                    # <- your master data frame
    filter(run == r) %>% 
    group_by(model, language, prompt) %>% 
    summarise(acc = mean(correct), .groups = "drop") %>% 
    group_by(language, prompt) %>% 
    mutate(rank = rank(-acc, ties.method = "min"),
           run  = paste0("run", r))
})

## ------------------------------------------------------------------
## 2.  Merge with 3-run majority truth
## ------------------------------------------------------------------
rank_long_all <- rank_truth %>%  # built previously
  pivot_longer(cols  = rank_truth,
               names_to  = "run",
               values_to = "rank") %>% 
  mutate(run = "3-run majority") %>% 
  bind_rows(rank_run_all %>% rename(rank = rank))

## ------------------------------------------------------------------
## 3.  Enforce column order & plot
## ------------------------------------------------------------------
plot_df <- rank_long_all %>% 
  mutate(run = factor(run,
                      levels = c("3-run majority", "run1", "run2", "run3")))

ggplot(plot_df,
       aes(x      = run,
           y      = rank,
           group  = model,
           colour = model)) +
  geom_line(size = 1, alpha = .4) +
  geom_point(size = 2) +
  scale_y_reverse(breaks = 1:max(plot_df$rank)) +
  facet_grid(language ~ prompt, switch = "y") +
  labs(x = NULL,
       y = "Leaderboard rank (1 = best)",
       colour = "Model",
       title  = "Rank stability: single runs vs 3-run ground-truth") +
  theme_minimal(base_size = 10) +
  theme(
    panel.spacing = unit(1, "lines"),
    # add a little left margin so long model names in legend don't clip
    plot.margin  = margin(5.5, 30, 5.5, 20)
  )
