#==============================================================================
# 04-fig04-sentiment-models.R
# Purpose: to replicate Figure 4 of the paper, in which we plot the marginal
#          effects from multinomial regressions predicting the negative sentiment
#          of quote tweets
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
library(MASS)
library(broom)
library(ggplot2)


# DATA
#===============================================================================
# - dataset with all quote tweets of elite actors.N = 2,973,167
db <- read.csv("../ingroup_filtering_DATA/quote-tweet-db.csv",
               colClasses = "character")

# DATA WRANGLING
#===============================================================================
# - remove observations involving moderate actors or users, as we can't clearly
#   determine the out-group. N = 1,966,023
db_nomod <- db %>%
  filter(actor_ideocat != "Moderate", user_ideocat != "Moderate")

# - create an ingroup/outgroup variable describing the type of quote tweet
db_nomod <- db_nomod %>%
  mutate(congruence = ifelse(actor_ideocat == user_ideocat, 
                             "ingroup", "outgroup"))

# - transform from character to numeric the num var that go into the model
db_nomod <- db_nomod %>%
  mutate(mentioned_actor_followers = as.numeric(mentioned_actor_followers),
         user_friends = as.numeric(user_friends),
         user_followers = as.numeric(user_followers))

# - remove tweets for which we don't have sentiment prediction (we didn't 
#   generate a predictions with tweets with fewer than 5 tokens after 
#   pre-processing -- the CNN architecture doesn't allow for it, as we have 
#   a layer with a 5-token window). N = 1,519,872
db_nomod02 <- db_nomod %>%
  filter(cnn != "")

# - the same but for the topic prediction, remove tweets for which we don't have
#   a topic prediction for the text of the original tweet (same think, the CNN
#   architecture requires tweets to be at least 5 characters after pre-processing)
#   N = 1,469,708
db_nomod03 <- db_nomod02 %>%
  filter(topic != "")

# - create a categorical variable indicating time of the day
db_nomod03$h <- as.numeric(sapply(as.character(db_nomod03$hour), function(x)
  strsplit(x, split = ":")[[1]][1]))

db_nomod03 <- db_nomod03 %>%
  mutate(Time_of_day = ifelse(
    h >= 0 & h < 4, "t1",
    ifelse(h >= 4 & h < 8, "t2",
           ifelse(h >= 8 & h < 12, "t3",
                  ifelse(h >= 12 & h < 16, "t4",
                         ifelse(h >= 16 & h < 20, "t5",
                                "t6"))))),
    ccn = factor(cnn, levels = c("negative", "neutral", "positive")))

# - add information about the ideo extremity of the actors in a {0,1} range
mean_actor_ideo <- mean(as.numeric(db_nomod03$actor_ideo), na.rm = TRUE)

db_nomod03 <- db_nomod03 %>%
  mutate(actor_ideodiff = mean_actor_ideo - as.numeric(actor_ideo),
         actor_ideoextrem = abs(actor_ideodiff))

range01 <- function(x){(x-min(x))/(max(x)-min(x))}
db_nomod03$actor_ideoextrem_std <- range01(db_nomod03$actor_ideoextrem)


# MAIN
#===============================================================================
# - fit all the models, quoting diff types of actors, and to quotes of only Lib
#   actors and then only quotes of Con actors

# ALL MESSAGES
#-------------------------------------------------------------------------------
# - Liberal actors
all_lib <- polr(factor(cnn,levels = c("negative", "neutral", "positive")) ~
                  congruence + log1p(mentioned_actor_followers) + 
                  log1p(user_friends) + log1p(user_followers) + Time_of_day +
                  topic + actor_ideoextrem_std + outgroup_followed,
                data = db_nomod03 %>%
                  filter(actor_ideocat == "Liberal"), Hess = TRUE)

all_lib_tab <- tidy(all_lib)

# - Conservative actors
all_con <- polr(factor(cnn,levels = c("negative", "neutral", "positive")) ~
                  congruence + log1p(mentioned_actor_followers) + 
                  log1p(user_friends) + log1p(user_followers) + Time_of_day +
                  topic + actor_ideoextrem_std + outgroup_followed,
                data = db_nomod03 %>%
                  filter(actor_ideocat == "Conservative"), Hess = TRUE)

all_con_tab <- tidy(all_con)


# MEDIA
#-------------------------------------------------------------------------------
# - Liberal actors
media_lib <- polr(factor(cnn,levels = c("negative", "neutral", "positive")) ~
                    congruence + log1p(mentioned_actor_followers) + 
                    log1p(user_friends) + log1p(user_followers) + Time_of_day +
                    topic + actor_ideoextrem_std  + outgroup_followed,
                  data = db_nomod03 %>%
                    filter(actor_ideocat == "Liberal" &
                             actor_type == "Media"), Hess = TRUE)

media_lib_tab <- tidy(media_lib)

# - Conservative actors
media_con <- polr(factor(cnn,levels = c("negative", "neutral", "positive")) ~
                    congruence + log1p(mentioned_actor_followers) + 
                    log1p(user_friends) + log1p(user_followers) + Time_of_day +
                    topic + actor_ideoextrem_std + outgroup_followed,
                  data = db_nomod03 %>%
                    filter(actor_ideocat == "Conservative" &
                             actor_type == "Media"), Hess = TRUE)

media_con_tab <- tidy(media_con)

# JOURNALISTS
#-------------------------------------------------------------------------------
# - Liberal actors
journalist_lib <- polr(factor(cnn,levels = c("negative", "neutral", "positive")) ~
                         congruence + log1p(mentioned_actor_followers) + 
                         log1p(user_friends) + log1p(user_followers) + Time_of_day +
                         topic + actor_ideoextrem_std  + outgroup_followed,
                       data = db_nomod03 %>%
                         filter(actor_ideocat == "Liberal" &
                                  actor_type == "Journalist"), Hess = TRUE)

journalist_lib_tab <- tidy(journalist_lib)

# - Conservative actors
journalist_con <- polr(factor(cnn,levels = c("negative", "neutral", "positive")) ~
                         congruence + log1p(mentioned_actor_followers) + 
                         log1p(user_friends) + log1p(user_followers) + Time_of_day +
                         topic + actor_ideoextrem_std + outgroup_followed,
                       data = db_nomod03 %>%
                         filter(actor_ideocat == "Conservative" &
                                  actor_type == "Journalist"), Hess = TRUE)

journalist_con_tab <- tidy(journalist_con)


# POLITICIANS (all)
#-------------------------------------------------------------------------------
# - Liberal actors
pol_lib <- polr(factor(cnn,levels = c("negative", "neutral", "positive")) ~
                  congruence + log1p(mentioned_actor_followers) + 
                  log1p(user_friends) + log1p(user_followers) + Time_of_day +
                  topic + actor_ideoextrem_std + outgroup_followed,
                data = db_nomod03 %>%
                  filter(actor_ideocat == "Liberal" &
                           actor_type == "Politician"), Hess = TRUE)

pol_lib_tab <- tidy(pol_lib)

# - Conservative actors
pol_con <- polr(factor(cnn,levels = c("negative", "neutral", "positive")) ~
                  congruence + log1p(mentioned_actor_followers) + 
                  log1p(user_friends) + log1p(user_followers) + Time_of_day +
                  topic + actor_ideoextrem_std + outgroup_followed,
                data = db_nomod03 %>%
                  filter(actor_ideocat == "Conservative" &
                           actor_type == "Politician"), Hess = TRUE)

pol_con_tab <- tidy(pol_con)

# POLITICIANS (no Trump)
#-------------------------------------------------------------------------------
# - Liberal actors
pol_notrump_lib <- polr(factor(cnn,levels = c("negative", "neutral", "positive")) ~
                          congruence + log1p(mentioned_actor_followers) + 
                          log1p(user_friends) + log1p(user_followers) + Time_of_day +
                          topic + actor_ideoextrem_std + outgroup_followed,
                        data = db_nomod03 %>%
                          filter(actor_ideocat == "Liberal" &
                                   actor_type == "Politician" &
                                   mentioned_actor != "realDonaldTrump"), Hess = TRUE)

pol_notrump_lib_tab <- tidy(pol_notrump_lib)

# - Conservative actors
pol_notrump_con <- polr(factor(cnn,levels = c("negative", "neutral", "positive")) ~
                          congruence + log1p(mentioned_actor_followers) + 
                          log1p(user_friends) + log1p(user_followers) + Time_of_day +
                          topic + actor_ideoextrem_std + outgroup_followed,
                        data = db_nomod03 %>%
                          filter(actor_ideocat == "Conservative" &
                                   actor_type == "Politician" &
                                   mentioned_actor != "realDonaldTrump"), Hess = TRUE)

pol_notrump_con_tab <- tidy(pol_notrump_con)


# TRUMP
#-------------------------------------------------------------------------------
# - Liberal actors
trump_lib <- polr(factor(cnn,levels = c("negative", "neutral", "positive")) ~
                    congruence + 
                    log1p(user_friends) + log1p(user_followers) + Time_of_day +
                    topic + actor_ideoextrem_std + outgroup_followed,
                  data = db_nomod03 %>%
                    filter(mentioned_actor == "realDonaldTrump"), Hess = TRUE)

trump_lib_tab <- tidy(trump_lib)


# FIGURE
#===============================================================================
# - combine results for all models, and pull coef of interest
model_table <- rbind(
  all_lib_tab %>% mutate(model = "ALL MESSAGES", group = "Conservative users"), 
  all_con_tab %>% mutate(model = "ALL MESSAGES", group = "Liberal users"), 
  
  media_lib_tab %>% mutate(model = "Media", group = "Conservative users"), 
  media_con_tab %>% mutate(model = "Media", group = "Liberal users"), 
  
  journalist_lib_tab %>% mutate(model = "Journalists", group = "Conservative users"), 
  journalist_con_tab %>% mutate(model = "Journalists", group = "Liberal users"), 
  
  pol_lib_tab %>% mutate(model = "Politicians\n(Including Trump)", 
                         group = "Conservative users"), 
  pol_con_tab %>% mutate(model = "Politicians\n(Including Trump)", 
                         group = "Liberal users"), 
  
  pol_notrump_lib_tab %>% mutate(model = "Politicians\n(No Trump)", 
                                 group = "Conservative users"), 
  pol_notrump_con_tab %>% mutate(model = "Politicians\n(No Trump)", 
                                 group = "Liberal users"), 
  
  trump_lib_tab %>% mutate(model = "Trump", 
                           group = "Liberal users")
  
) %>%
  filter(term == "congruenceoutgroup") %>%
  rename(std_err = `std.error`) %>%
  mutate(prelwr = estimate - (1.96 * std_err),
         preupr = estimate + (1.96 * std_err),
         pe = 1 / exp(estimate),#1 + (1 - exp(estimate)),
         upr = 1 / exp(prelwr), #1 + (1 - exp(prelwr)),
         lwr = 1 / exp(preupr), #1 + (1 - exp(preupr)),
         model = factor(model,
                        levels = rev(c("ALL MESSAGES", "Media", "Journalists",
                                       "Politicians\n(No Trump)",
                                       "Politicians\n(Including Trump)",
                                       "Trump"))))

# OUTPUT
#===============================================================================
pdf("./figures/figure04.pdf", width = 6, height = 5)
model_table <- model_table %>%
  mutate(model = factor(model,
                        levels = rev(c("ALL MESSAGES", "Media", "Journalists",
                                       "Politicians\n(No Trump)",
                                       "Politicians\n(Including Trump)",
                                       "Trump"))))
ggplot(model_table,
       aes(x = model, y = pe, ymin = lwr, ymax = upr)) +
  geom_point(inherit.aes = FALSE,
             data = data.frame(group = c("Conservative users",
                                         "Liberal users"),
                               x = c(20,20), y = c(2, 2)),
             aes(x = x, y = y, color = group), size = 3) +
  geom_segment(inherit.aes = FALSE,
               data = model_table %>% 
                 filter(group == "Conservative users"),
               aes(x = as.numeric(model) + 0.2, xend = as.numeric(model) + 0.2,
                   y = lwr, yend = upr), size = 2.5, color = "#8B4500") +
  geom_segment(inherit.aes = FALSE,
               data = model_table %>% 
                 filter(group == "Liberal users"),
               aes(x = as.numeric(model) - 0.2, xend = as.numeric(model) - 0.2,
                   y = lwr, yend = upr), size = 2.5, color = "#00008B") +
  geom_text(inherit.aes = FALSE,
            data = model_table %>% 
              filter(group == "Conservative users"),
            aes(x = as.numeric(model) + 0.2, y = upr + 0.15, size = 2,
                label = paste0("x", round(pe, 2))), size = 3.5, color = "gray50") +
  geom_text(inherit.aes = FALSE,
            data = model_table %>% 
              filter(group == "Liberal users"),
            aes(x = as.numeric(model) - 0.2, y = upr + 0.15,
                label = paste0("x", round(pe, 2))), size = 3.5, color = "gray50") +
  geom_hline(yintercept = 1, color = "red") +
  coord_flip() +
  scale_x_continuous("", breaks = seq(1, 6, 1),
                     labels = levels(model_table$model),
                     limits = c(0.5, 6.5)) +
  scale_y_continuous("\nHow much more likely to add a negative quote to tweet from\nout-group (v. in-group) elite",
                     breaks = seq(0, 4, 0.5), labels = paste0("x", seq(0, 4, 0.5)),
                     limits = c(0.9, 4)) +
  scale_color_manual("", values = c("#8B4500", "#00008B")) +
  theme(panel.background = element_blank(),
        panel.grid.major.y = element_line(color = "gray90"),
        #panel.grid.major.x = element_line(color = "gray90", linetype = "dotted"),
        legend.position = "bottom",
        axis.line = element_line(),
        axis.title.x = element_text(size = 10),
        axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 11))
dev.off()  

