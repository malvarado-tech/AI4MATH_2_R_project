library(tidyverse)
library(DescTools)
library(lme4)
library(emmeans)
library(broom.mixed)
library(patchwork)
library(scales)


df <- read_csv("C:\\Users\\migue\\Downloads\\ai4math_runs.csv") %>%                # ❶ load
  mutate(across(c(problem_id, domain, language,       # ❷ factors
                  prompt, model, run),
                ~ factor(.x)))

#########[GPT-4o REFERENCE] Main significance test: mixed‑effects logistic regression#################
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

# Pairwise model contrasts within each language‑prompt slice
library(emmeans)

emm  <- emmeans(fit5, ~ model | language )
ctrs <- pairs(emm, adjust = "holm") %>%
  summary(infer = TRUE)            # odds ratios & CI


# Save for paper
write_csv(ctrs, "pairwise_contrasts.csv")


# Forest plot against a reference (change "GPT‑4o" if needed)
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
print(forest)




################################
#######################################


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
fitgr4 <- glmer(
  correct ~ model * language * prompt +
    (1 | problem_id) + (1 | problem_id:run),
  family  = binomial,
  data    = df_grok,
  control = glmerControl(optimizer = "bobyqa",
                         optCtrl   = list(maxfun = 2e5))
)

fitgr4_2 <- glmer(
  correct ~ model * language + prompt +
    (1 | problem_id) + (1 | problem_id:run),
  family  = binomial,
  data    = df_grok,
  control = glmerControl(optimizer = "bobyqa",
                         optCtrl   = list(maxfun = 2e5))
)

fitgr4_3 <- glmer(
  correct ~ model + language * prompt +
    (1 | problem_id) + (1 | problem_id:run),
  family  = binomial,
  data    = df_grok,
  control = glmerControl(optimizer = "bobyqa",
                         optCtrl   = list(maxfun = 2e5))
)

fitgr4_4 <- glmer(
  correct ~ model + language + prompt +
    (1 | problem_id) + (1 | problem_id:run),
  family  = binomial,
  data    = df_grok,
  control = glmerControl(optimizer = "bobyqa",
                         optCtrl   = list(maxfun = 2e5))
)

fitgr4_5 <- glmer(
  correct ~ model+
    (1 | problem_id) + (1 | problem_id:run),
  family  = binomial,
  data    = df_grok,
  control = glmerControl(optimizer = "bobyqa",
                         optCtrl   = list(maxfun = 2e5))
)

anova(fitgr4,fitgr4_2,fitgr4_3,fitgr4_4,fitgr4_5)


## 2.  Marginal means per model  → odds ratio vs Grok‑4
emm_model  <- emmeans(fitgr4_5, ~ model)        # log‑odds scale
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




###########################################
######################
##################
############




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

fit_dom2 <- glmer(
  correct ~ model + domain +                 # fixed effects
    (1 | problem_id) + (1 | problem_id:run), # random effects
  family  = binomial,
  data    = df_dom,
  control = glmerControl(optimizer = "bobyqa",
                         optCtrl   = list(maxfun = 2e5))
)

anova(fit_dom,fit_dom2)

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
emm_dom <- emmeans(fit_dom2, ~ model | domain, type = "response")

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
emm_dom <- emmeans(fit_dom2, ~ model | domain)  # no type = "response"

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








