#==============================================================================
# 02-fig02-following-sharing-barplots.R
# Purpose: to replicate Figure 2 of the paper, in which we show the proportion
#          of in/out-group accounts followed and shared, as well as the type of
#          sharing behavior.
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


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# [ A ] FIGURE 2A
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# (The data/code for FIGURE 2B can be find below)

# DATA
#===============================================================================
# - load data with in/out-group following and sharing information for ordinary
#   users we classifed as Liberals and Conservatives. We exclude in this
#   analysis those users for which we didn't obtain an ideology score, as well
#   as those classfied as Moderates, as they don't have a clear out-group.
#   N = 151,063
db <- read.csv("./data/sharing-following-in-out-elites-no-moderates.csv",
               colClasses = "character")

# DATA WRANGLING
#===============================================================================
# - calculate total Lib + Con FOLLOWED
db <- db %>%
  mutate(followed_lib_con = as.numeric(as.character(followed_lib)) +
           as.numeric(as.character(followed_con)))

# - calculate prop of In v Out group SHARED
db <- db %>%
  mutate(shares_in = as.numeric(as.character(qt_in)) +
           as.numeric(as.character(rt_in)),
         shares_out = as.numeric(as.character(qt_out)) +
           as.numeric(as.character(rt_out)),
         allshares = shares_in + shares_out)

# - calculate, by ideo group, proportion of in(v.out)-group followed, and 
#   in(v. out)-group shared.
plot_db <- db %>%
  mutate(outgroup_followed = as.numeric(as.character(outgroup_followed)),
         shares_out = as.numeric(as.character(db$shares_out))) %>%
  group_by(user_ideocat) %>%
  summarise(prop_out_followed = sum(outgroup_followed) / sum(followed_lib_con),
            prop_in_followed = 1 - prop_out_followed,
            prop_out_shared = sum(shares_out) / sum(allshares),
            prop_in_shared = 1 - prop_out_shared) %>%
  gather(variable, value, -user_ideocat) %>%
  mutate(relation = ifelse(grepl("_out_", variable), "Out-group", "In-group"),
         outcome = ifelse(grepl("_follow", variable), 
                          "Prop. elite accounts followed", 
                          "Prop. elite tweets shared")) %>%
  dplyr::select(-variable) %>%
  mutate(value = round(value, 3) * 100)

# FIGURE
#===============================================================================
pdf("./figures/figure02a.pdf", width = 7, height = 6)
ggplot(plot_db %>%
         mutate(user_ideocat = paste0(user_ideocat, " users")),
       aes(x = relation, y = value, fill = user_ideocat)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.6) +
  geom_text(inherit.aes = FALSE,
            data = plot_db %>%
              filter(user_ideocat == "Conservative"),
            aes(x = as.numeric(as.factor(relation)) - 0.25, y = value + 4,
                label = paste0(value, "%")), size = 4.5) +
  geom_text(inherit.aes = FALSE,
            data = plot_db %>%
              filter(user_ideocat == "Liberal"),
            aes(x = as.numeric(as.factor(relation)) + 0.25, y = value + 4,
                label = paste0(value, "%")), size = 4.5) +
  scale_y_continuous("", breaks = NULL) +
  scale_x_discrete("") +
  scale_fill_manual("", values = c("red4", "blue4")) +
  facet_wrap(~ outcome) +
  theme(panel.background = element_blank(),
        legend.position = "bottom",
        strip.text = element_text(size = 12),
        axis.text = element_text(size = 13),
        legend.text = element_text(size = 12))
dev.off()



#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# [ B ] FIGURE 2B
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------


# DATA
#===============================================================================
# - load this simple frequency table w. information about the number of retweets
#   and quote tweets across Lib/Con/Mod ordinary users and elites
db <- read.csv("~/Downloads/rt-qt-ingroup-outgroup-crosstab-w-mod.csv")

# - load this dataset with further information about individual quote tweets, 
#   such as the ideology of the users and quoted elite actor, as well as the
#   topic of the original tweet and the sentiment of the quote
#   N = 2,973,167
db_qt <- read.csv("../ingroup_filtering_DATA/quote-tweet-db.csv",
                  colClasses = "character")


# DATA WRANGLING
#===============================================================================
# - calculate prop of negative ingroup/outgroup quote tweets. 
# ... exclude quote tweets if:
#       - a Moderate elite or ordinary users involved
#       - we don't have a sentiment prediction for the tweet (short tweets with
#         < 5 words are excluded from sentiment predictions)
db_qt02 <- db_qt %>%
  mutate(relationship = ifelse(user_ideocat == actor_ideocat,
                               "Ingroup", "Outgroup"),
         relationship = ifelse(user_ideocat == "Moderate" |
                                 actor_ideocat == "Moderate",
                               NA, as.character(relationship))) %>%
  filter(!is.na(relationship),
         cnn != "")

# ... get total number of ingroup/outgropu tweets by user ideology
inout_group_total <- db_qt02 %>%
  mutate(cnn = ifelse(cnn == "neutral", "positive", as.character(cnn))) %>%
  group_by(relationship, user_ideocat) %>%
  summarise(total = n())

# ... get total number of positive&neutral/negative quote tweets by in/outgroup,
#     and by user ideology
inout_group_tone <- db_qt02 %>% 
  mutate(cnn = ifelse(cnn == "neutral", "positive", as.character(cnn))) %>%
  group_by(relationship, user_ideocat, cnn) %>% 
  summarise(n = n())

# ... here we calculate the prop. of positive&neutral/negative quote tweets
#     by in/out-group, and by user ideology
inout_summary <- left_join(inout_group_total, inout_group_tone) %>%
  mutate(prop = round(n / total, 2)) %>%
  as.data.frame() %>%
  mutate(tweet_type02 = ifelse(cnn == "positive", "Quote tweet positive",
                               "Quote tweet negative"),
         tweet_type = "quote_tweet",
         relationship = paste0(relationship, " sharing")) %>%
  dplyr::select(-cnn, -total, -n)

# - merge this info with the rest of the descriptive data
# ... exclude moderates from the summary frequency table, and rename values so
#     the data can be merged
db_tomerge <- db %>%
  filter(user_ideocat != "moderate",
         actor_ideocat != "moderate") %>%
  mutate(user_ideocat = recode(user_ideocat,
                               `liberal` = "Liberal",
                               `conservative` = "Conservative"),
         actor_ideocat = recode(actor_ideocat,
                                `liberal` = "Liberal",
                                `conservative` = "Conservative"),
         relationship = ifelse(user_ideocat == actor_ideocat,
                               "Ingroup sharing", "Outgroup sharing")) %>%
  dplyr::select(-actor_ideocat)

# ... perform the merging (info about sentiment will be NA for retweets, as they
#     don't any commentary to the original tweets from the elite account)
db_tomerge$tweet_type <- as.character(db_tomerge$tweet_type)
db_tomerge$user_ideocat <- as.character(db_tomerge$user_ideocat)
db_tomerge$relationship <- as.character(db_tomerge$relationship)
inout_summary$tweet_type <- as.character(inout_summary$tweet_type)
inout_summary$user_ideocat <- as.character(inout_summary$user_ideocat)
inout_summary$relationship <- as.character(inout_summary$relationship)

db02 <- left_join(db_tomerge, inout_summary) %>%
  mutate(n_final = ifelse(is.na(prop), n,
                          round(n * prop)))

# - add the totals for each category of interest (i.e. ingroup tweets by Lib)
totals <- db02 %>%
  group_by(user_ideocat, relationship) %>%
  summarise(total = sum(n_final))

db03 <- left_join(db02, totals) %>%
  mutate(prop02 = round(n_final / total, 2),
         tweet_type02 = ifelse(tweet_type == "retweet",
                               "Retweet", as.character(tweet_type02)))

# PLOT
#===============================================================================
pdf("./figures/figure02b.pdf", width = 5, height = 4)
ggplot(db03 %>%
         mutate(relationship = gsub(" ", "\n", relationship),
                user_ideocat = paste0(user_ideocat, " users"),
                user_ideocat = factor(user_ideocat, 
                                      levels = c("Liberal users", 
                                                 "Conservative users")),
                tweet_type02 = gsub("positive", "neutral/positive", tweet_type02)),
       aes(x = relationship, y = n_final)) +
  geom_bar(stat = "identity", width = 0.3,
           aes(fill = factor(tweet_type02), color = factor(tweet_type02))) +
  facet_wrap(~ user_ideocat) +
  scale_y_continuous("", breaks = NULL, limits = c(0, 14000000)) +
  scale_x_discrete("") +
  scale_color_manual("", values = c("gray20", "gray50", "gray80")) +
  scale_fill_manual("", values = c("gray20", "gray50", "gray80")) +
  theme(legend.position = "bottom",
        panel.background = element_blank(),
        legend.text = element_text(size = 7))
dev.off()

# STATS ADDED TO THE FIGURE
#===============================================================================
x <- db03 %>%
  mutate(relationship = gsub(" ", "\n", relationship),
         user_ideocat = paste0(user_ideocat, " users"),
         user_ideocat = factor(user_ideocat, 
                               levels = c("Liberal users", 
                                          "Conservative users")),
         tweet_type02 = gsub("positive", "neutral/positive", tweet_type02))

# - Num. of Ingroup Liberal Tweets
sum((x %>% filter(user_ideocat == "Liberal users",
                  relationship == "Ingroup\nsharing"))$n_final) 
#... N = 11,207,732 --> 11.2 million reported in the Figure


# - Num. of Ingroup Conservative Tweets
sum((x %>% filter(user_ideocat == "Conservative users",
                  relationship == "Ingroup\nsharing"))$n_final) 
#... N = 7,977,903 --> 7.9 million reported in the Figure

# - Num. of Outgroup Liberal Tweets
sum((x %>% filter(user_ideocat == "Liberal users",
                  relationship == "Outgroup\nsharing"))$n_final) 
# N = 1,113,362 --> 1.1 million reported in the Figure

# - Num. of Outgroup Conservative Tweets
sum((x %>% filter(user_ideocat == "Conservative users",
                  relationship == "Outgroup\nsharing"))$n_final) 
# N = 368,121 --> 0.4 million reported in the Figure

# - % of liberal tweets that are ingroup
round(11207732 / (11207732 + 1113362), 3) * 100 # 91% reported in the Figure

# - % of conservative tweets that are ingroup
round(7977903 / (7977903 + 368121), 3) * 100 # 95.6% reported in the Figure

# - % of ingroup liberal tweets that are Quote tweets
lib_in_qt <- sum((x %>% filter(
  user_ideocat == "Liberal users",
  relationship == "Ingroup\nsharing",
  grepl("Quote", tweet_type02)
))$n_final)
lib_in <- 11207732 + 1113362
round(lib_in_qt / lib_in, 2) * 100 # 8% reported in the Figure


# - % of ingroup conservative tweets that are Quote tweets
con_in_qt <- sum((x %>% filter(
  user_ideocat == "Conservative users",
  relationship == "Ingroup\nsharing",
  grepl("Quote", tweet_type02)
))$n_final)
con_in <- 7977903 + 368121
round(con_in_qt / con_in, 2) * 100 # 6% reported in the Figure

# - % of ingroup liberal tweets that are Negative Quote tweets
lib_in_qt_neg <- sum((x %>% filter(
  user_ideocat == "Liberal users",
  relationship == "Ingroup\nsharing",
  tweet_type02 == "Quote tweet negative"
))$n_final)

round(lib_in_qt_neg / lib_in, 2) * 100 # 4% reported in the Figure

# - % of ingroup conservative tweets that are Negative Quote tweets
con_in_qt_neg <- sum((x %>% filter(
  user_ideocat == "Conservative users",
  relationship == "Ingroup\nsharing",
  tweet_type02 == "Quote tweet negative"
))$n_final)

round(con_in_qt_neg / con_in, 2) * 100 # 3% reported in the Figure
