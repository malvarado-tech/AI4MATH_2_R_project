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




#########[GPT-4o REFERENCE] Main significance test: mixed‑effects logistic regression#################
library(lme4)
fit <- glmer(correct ~ model * language * prompt +
               (1 | problem_id) + (1 | problem_id:run),
             family = binomial, data = df)

# Pairwise model contrasts within each language‑prompt slice
library(emmeans)

emm  <- emmeans(fit, ~ model | language * prompt)
ctrs <- pairs(emm, adjust = "holm") %>%
  summary(infer = TRUE)            # odds ratios & CI


# Save for paper
write_csv(ctrs, "pairwise_contrasts.csv")


# Forest plot against a reference (change "GPT‑4o" if needed)
ref <- "Grok‑4"
forest <- broom.mixed::tidy(fit, conf.int = TRUE, effects="fixed") %>%
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
print(forest)






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

print(forest)




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



##########################Domain‑level diagnosticn##############################################

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

## 0.  Make sure 'domain' is a factor
df_dom <- df %>% mutate(domain = as.factor(domain))

## 1.  Fit mixed‑effects logistic model with MODEL × DOMAIN interaction
fit_dom <- glmer(
  correct ~ model * domain +                 # fixed effects
    (1 | problem_id) + (1 | problem_id:run), # random effects
  family  = binomial,
  data    = df_dom,
  control = glmerControl(optimizer = "bobyqa",
                         optCtrl   = list(maxfun = 2e5))
)

## 2.  Order‑independent (type‑III) Wald χ² tests
anova_dom <- car::Anova(fit_dom, type = 3)
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
            colour = "white", size = 3) +
  scale_fill_viridis_c(name = "Accuracy",
                       limits = c(0, 1),
                       labels = percent_format(accuracy = 1)) +
  labs(x = NULL, y = NULL,
       title = "Accuracy per Domain and Model") +
  theme_minimal(base_size = 10)

print(dom_plot)

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
 
 ## 1.  Get emmeans on the *link* scale (default) → contrasts vs Grok‑4
 emm_dom <- emmeans(fit_dom, ~ model | domain)  # no type = "response"
 
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
   theme_minimal(base_size = 11) +
   theme(legend.position = "top",
         strip.text = element_text(face = "bold"))
 










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
  fit <- glmer(correct ~ model + (1 | problem_id),
               family = binomial, data = d,
               control = glmerControl(optimizer = "bobyqa",
                                      optCtrl   = list(maxfun = 2e5)))
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
