#==============================================================================
# 01-fig01-ideo-distributions.R
# Purpose: to replicate Figure 1 of the paper, in which we show the distribution
#          of the ideology scores for the three types of political elites we
#          study, as well as the ordinary users.
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
library(ggridges)
library(ggrepel)


# DATA
#===============================================================================
# - ideology scores for actors
actors_ideo <- read.csv("./data/actors-ideo-scores.csv", 
                        colClasses = "character")

# - users ideo
users_ideo <- read.csv(
  "./data/users-ideo-plus-ingroup-sharing-info.csv",
  colClasses = "character")

# DATA WRANGLING
#===============================================================================
# - merge the two files
actors_tomerge <- actors_ideo %>%
  rename(name = actor, user_id = actor_id)

users_tomerge <- users_ideo %>%
  rename(ideo = user_ideo, user_id = user_id_anon) %>%
  mutate(type = "User", name = NA) %>%
  dplyr::select(user_id, type, name, ideo)

ideo_db <- rbind(actors_tomerge, users_tomerge) %>%
  mutate(type = ifelse(type != "Media", paste0(type, "s"), "Media"),
         type = factor(type, levels = rev(c("Politicians", "Media", "Journalists",
                                            "Users")))) 

# - chose some politician, media outlet, and journalist to show in the plot for
#   face validity purposes
ideo_db <- ideo_db %>%
  mutate(label_toshow = ifelse(
    name %in% c( 
      # Politicians
      "realDonaldTrump", "AOC",  "SenSanders", "JoeBiden", "McConnellPress",
      # Media organizations
      "MSNBC", "MotherJones", "FoxNews", "BreitbartNews", "AP", "CNN", "time", 
      "politico", "ABC",
      # Journalists
      "BillOReilly", "seanhannity", "maddow", "ananavarro", "chrislhayes",
      "shaunking"),
    paste0("@", as.character(name)), NA))

# - add categorical ideological variable
ideo_db <- ideo_db %>%
  mutate(ideo_cat = ifelse(ideo < 0, "Liberal", 
                           ifelse(ideo > 1.2, "Conservative",
                                  "Moderate")),
         ideo = as.numeric(as.character(ideo)))

# FIGURE
#===============================================================================
pdf("./figures/figure01.pdf", width = 8, height = 6)
ggplot(ideo_db,
       aes(y = type, x = ideo)) + #, fill = factor(stat(quantile)))) +
  geom_vline(xintercept = 0.6, size = 0.5) +
  geom_density_ridges(bandwidth = 0.5, alpha = 0.7, size = 0.1) +
  annotate("rect", xmin = -3.5, xmax = 0, ymin = 1, ymax = 6,
           alpha = .2, fill = "blue") +
  annotate("rect", xmin = 0, xmax = 1.2, ymin = 1, ymax = 6,
           alpha = .2, fill = "gray") +
  annotate("rect", xmin = 1.2, xmax = 5, ymin = 1, ymax = 6,
           alpha = .2, fill = "red") +
  geom_point(inherit.aes = FALSE,
             data = ideo_db %>% filter(!is.na(label_toshow)),
             aes(y = type, x = ideo)) +
  geom_text_repel(aes(x = ideo, y = type, label = label_toshow),
                  force = 1, nudge_y = 0.3,
                  segment.size = 0.25,
                  segment.alpha = 0.5) +
  geom_text(inherit.aes = FALSE,
            data = data.frame(
              y = seq(1.25, 4.25, 1),
              x = -5,
              text = c("Users", "Journalists", "Media", "Politicians")),
            aes(y = y, x = x, label = text), hjust = 0, size = 5) +
  scale_y_discrete("", expand = c(0,0), breaks = NULL) +
  scale_x_continuous("",
                     breaks = c(-2, 0.6, 3.2),
                     labels = c("Most\nliberal", "Moderate", "Most\nConservative")) +
  geom_hline(yintercept = 1, color = "gray30") +
  geom_hline(yintercept = 2, color = "gray30") +
  geom_hline(yintercept = 3, color = "gray30") +
  geom_hline(yintercept = 4, color = "gray30") +
  theme(panel.background = element_blank(),
        axis.text = element_text(size = 12))
dev.off()
