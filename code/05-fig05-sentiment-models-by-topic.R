#==============================================================================
# 05-fig05-sentiment-models-by-topic.R
# Purpose: to replicate Figure 5 of the paper, in which we plot the marginal
#          effects from multinomial regressions predicting the negative sentiment
#          of quote tweets, distingushing by the policy issue discussed in the
#          original tweets sent by the elite actor.
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


# MAIN
#===============================================================================
# - list of unique topics
topics <- unique(db$topic_label)
# ... exclude tweets for which we don't have a topic prediction (tweet < 5 words
#     after pre-processing) as well as those classified as not touching on any
#     relevant policy issue
topics <- as.character(topics[which(!topics %in% c("not_about_politics", "no_tpoic"))])

# - add information about the ideo extremity of the actors in a {0,1} range
mean_actor_ideo <- mean(as.numeric(db$actor_ideo), na.rm = TRUE)

db <- db %>%
  mutate(actor_ideodiff = mean_actor_ideo - as.numeric(actor_ideo),
         actor_ideoextrem = abs(actor_ideodiff))

range01 <- function(x){(x-min(x))/(max(x)-min(x))}
db$actor_ideoextrem_std <- range01(db$actor_ideoextrem)

# - create a categorical variable indicating time of the day
db$h <- as.numeric(sapply(as.character(db$hour), function(x)
  strsplit(x, split = ":")[[1]][1]))

db <- db %>%
  mutate(
        Time_of_day = ifelse(
          h >= 0 & h < 4, "t1",
          ifelse(h >= 4 & h < 8, "t2",
                 ifelse(h >= 8 & h < 12, "t3",
                        ifelse(h >= 12 & h < 16, "t4",
                               ifelse(h >= 16 & h < 20, "t5",
                                      "t6")))))
  )

# - make sure that all numeric variables are numeric
db <- db %>%
  mutate(
    mentioned_actor_followers = as.numeric(mentioned_actor_followers),
    user_friends = as.numeric(user_friends),
    user_followers = as.numeric(user_followers),
    actor_ideoextrem_std = as.numeric(actor_ideoextrem_std),
    outgroup_followed = as.numeric(outgroup_followed),
    cnn = factor(cnn, levels = c("negative", "neutral", "positive"))
  )

# - estimate multinomial models predicting the sentiment of the quote tweet as
#   a function of the topic of the original tweet sent by the elite actor, plus
#   a set of covariates included in the other models presented in the paper.
#   Distinguish between liberal and conservative users for each topic.
out <- NULL
counter <- 0
total <- length(topics)
full_modeltable <- NULL
for (top in topics) {
  counter <- counter + 1
  print(paste0("[", counter, "/", total, "]"))
  topic_db <- db %>% filter(topic_label == top)
  # - liberals
  topic_out_lib <- polr("cnn ~ congruence + log1p(mentioned_actor_followers) + 
                    log1p(user_friends) + log1p(user_followers) + Time_of_day +
                        actor_ideoextrem_std + outgroup_followed",
                        data = topic_db %>% filter(user_ideocat == "Liberal"), 
                        Hess = TRUE)
  full_modeltable <- rbind(full_modeltable, broom::tidy(topic_out_lib) %>%
                             mutate(topic = top,
                                    user_ideocat = "Liberal"))
  topic_table_lib <- broom::tidy(topic_out_lib) %>%
    filter(term == "congruenceoutgroup") %>%
    mutate(topic = top,
           user_ideocat = "Liberal")
  # - conservatives
  topic_out_con <- polr("cnn ~ congruence + log1p(mentioned_actor_followers) + 
                    log1p(user_friends) + log1p(user_followers) + Time_of_day +
                        actor_ideoextrem_std + outgroup_followed",
                        data = topic_db %>% filter(user_ideocat == "Conservative"), 
                        Hess = TRUE)
  full_modeltable <- rbind(full_modeltable, broom::tidy(topic_out_con) %>%
                             mutate(topic = top,
                                    user_ideocat = "Conservative"))
  topic_table_con <- broom::tidy(topic_out_con) %>%
    filter(term == "congruenceoutgroup") %>%
    mutate(topic = top,
           user_ideocat = "Conservative")
  out <- rbind(out, topic_table_con, topic_table_lib)
}

# - prepare the data to be plotted
out02 <- out %>%
  filter(topic != "other") %>%
  rename(std_err = `std.error`) %>%
  mutate(prelwr = estimate - (1.96 * std_err),
         preupr = estimate + (1.96 * std_err),
         pe = 1 / exp(estimate),
         upr = 1 / exp(prelwr),
         lwr = 1 / exp(preupr)) %>%
  arrange(pe) %>%
  mutate(topic = factor(topic, levels = unique(topic)))


# OUTPUT
#===============================================================================
pdf("./figures/figure05.pdf", width = 8, height = 7)
out02b <- out02 %>%
  # - removing this topic from the analysis, as it's not part of the CAP codebook
  filter(topic != "Political and Partisan Taunting") %>%
  mutate(topic = factor(as.character(topic)),
         topic = factor(topic, levels = unique(topic)))
ggplot(out02b,
       aes(x = topic, y = pe, ymin = lwr, ymax = upr)) +
  geom_point(inherit.aes = FALSE,
             data = data.frame(group = c("Conservative users",
                                         "Liberal users"),
                               x = c(30,30), y = c(2, 2)),
             aes(x = x, y = y, color = group), size = 3) +
  scale_color_manual("", values = c("red", "blue")) +
  geom_point(inherit.aes = FALSE,
             data = out02b %>%
               filter(user_ideocat == "Liberal"),
             aes(x = as.numeric(topic) + 0.1, y = pe),
             color = "blue", alpha = 0.7) +
  geom_segment(inherit.aes = FALSE,
               data = out02b %>%
                 filter(user_ideocat == "Liberal"),
               aes(x = as.numeric(topic) + 0.1, xend = as.numeric(topic) + 0.1,
                   y = lwr, yend = upr),
               color = "blue", alpha = 0.7) +
  geom_point(inherit.aes = FALSE,
             data = out02b %>%
               filter(user_ideocat == "Conservative"),
             aes(x = as.numeric(topic) - 0.1, y = pe),
             color = "red", alpha = 0.7) +
  geom_segment(inherit.aes = FALSE,
               data = out02b %>%
                 filter(user_ideocat == "Conservative"),
               aes(x = as.numeric(topic) - 0.1, xend = as.numeric(topic) - 0.1,
                   y = lwr, yend = upr),
               color = "red", alpha = 0.7) +
  geom_hline(yintercept = 1, color = "red") +
  coord_flip() +
  scale_x_continuous("",
                     breaks = seq(1, length(levels(out02b$topic))),
                     labels =  levels(out02b$topic),
                     limits = c(0.5, 21.5)) +
  scale_y_continuous("\nHow much more likely to add a negative quote to tweet from\nout-group (v. in-group) elite",
                     breaks = seq(0, 2.8, 0.5), labels = paste0("x", seq(0, 2.8, 0.5)),
                     limits = c(0.7, 2.8)) +
  theme(panel.background = element_blank(),
        panel.grid.major.y = element_line(color = "gray90"),
        #panel.grid.major.x = element_line(color = "gray90", linetype = "dotted"),
        legend.position = "bottom",
        axis.line = element_line(),
        axis.title.x = element_text(size = 10),
        axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 11))
dev.off()  
