#==============================================================================
# 03-fig03-ingroup-sharing-coef-plot.R
# Purpose: to replicate Figure 3 of the paper, in which we plot the marginal
#          effects from a logistic regression predicting in-group sharing.
# Article: "Most users do not engage with political elites on Twitter; Those who
#          do, show overwhelming preferences for ideological congruity"
# Journal: Science Advances
# Year:    2022
# Authors: Magdalena Wojcieszak, Andreu Casas, Xudong Yu, Jonathan Nagler, and
#          Joshua Tucker
#==============================================================================


# PACKAGES
#===============================================================================
library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)
# - our own utils
source("./code/00-functions.R")


# DATA
#===============================================================================
# - macro dataset with all RTs and QTs, and message/user/actor-level features for
#   modeling ingroup sharing. Shares invovling Moderate users/actors have been
#   excluded from the dataset.
#   N = 20,731,455 | 151,066 unique users
db <- read.csv("./data/ingroup-sharing-model-data.csv")

# MAIN
#===============================================================================

# - fit a linear model predicting ingroup sharing as a function of:
#     * actor type
#     * user ideocat
#     * actor ideo extremity
#     * user outgroup elite followed
model_formula <- formula(
  "ingroup ~ type + user_liberal + actor_ideoextrem + outgroup_followed")
set.seed(1234) # - for reproducibility 
coef_db <- NULL
folds <- 1000
for (fold in 1:folds) {
  if (fold %% 10 == 0) {
    print(paste0("[", fold, "/", folds, "]"))
  }
  s <- db %>% sample_n(10000) %>%
    mutate(user_liberal = ifelse(user_ideocat == "Liberal", 1, 0))
  model_out <- glm(model_formula, data = s, 
                   family = "binomial")
  std_out <- get_marfx_logistic(model = model_out, 
                                model_dataset = s,
                                cat_variables = c("type"))
  std_out$fold <- fold
  coef_db <- rbind(coef_db, std_out)
}


# - calculate avg coefficient and 95% CIs
avg_coef <- coef_db %>%
  rename(pe_std = pe, lwr_std = lwr, upr_std = upr) %>%
  group_by(v) %>%
  summarise(pe = mean(pe_std),
            lwr = quantile(lwr_std, probs = 0.025),
            upr = quantile(upr_std, probs = 0.975))

# PLOT / FIGURE
#===============================================================================
# - better names for the variables
avg_coef <- avg_coef %>%
  mutate(v = recode(v,
                    `actor_ideoextrem` = "Ideological extremity of the actor",
                    `type:Media` = "Media tweet [Ref: Pundit]",
                    `type:Politician` = "Politician tweet [Ref: Pundit]",
                    `user_liberal` = "Liberal user [Ref: Conservative]",
                    `outgroup_followed` = "Number out-group elite followed")) %>%
  filter(v != "(Intercept)")

# - transform likelihood to percentage points effects
avg_coef02 <- avg_coef %>%
  mutate(pe = ifelse(pe < 1, -round(1 - pe, 2) * 100, round(pe - 1, 2) * 100),
         lwr = ifelse(lwr < 1, -round(1 - lwr, 2) * 100, round(lwr - 1, 2) * 100),
         upr = ifelse(upr < 1, -round(1 - upr, 2) * 100, round(upr - 1, 2) * 100)) %>%
  arrange(pe) %>%
  mutate(v = factor(v, levels = unique(v)))

# OUTPUT
#===============================================================================
pdf("./figures/figure03.pdf", width = 6, height = 4)
ggplot(avg_coef02 %>%
         arrange(pe) %>%
         mutate(v = factor(v, levels = v)),
       aes(x = pe, y = v)) +
  geom_point(size = 2) +
  geom_text(aes(x = pe, y = as.numeric(factor(v)) + 0.2,
                label = ifelse(pe > 0,
                               paste0("+", round(pe, 2), "%"),
                               paste0(round(pe, 2), "%")))) +
  geom_segment(aes(x = lwr, xend = upr, y = v, yend = v)) +
  geom_vline(xintercept = 1) +
  scale_x_continuous("\nMarginal effect on the likelihood of sharing in-group content",
                     breaks = seq(-50, 100, 25)) +
  scale_y_discrete("") +
  theme(panel.background = element_blank(),
        axis.line.x = element_line(),
        axis.ticks = element_blank(),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 10))
dev.off()
