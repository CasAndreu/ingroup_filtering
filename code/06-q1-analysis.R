#==============================================================================
# 06-q1-analysis.R
# Purpose: to replicate the findings in the paper where we address Q1, so how
#          politically engaged the users in our random sample are (how many elites
#          they follow on Twitter); as well as celebrities (as a point of 
#          comparison)
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


# DATA
#===============================================================================
# - load a frequency table with info about how many elite accounts ALL users
#   in the random sample (N = 1,437,774) follow.
elite_freq = read.csv("./data/elitenum-followed-all-random-users-FREQ-TABLE.csv")

# - load a frequency table with info about how many celebrities the users in our
#   random sample follow. We only run this analysis on about half of the users
#   (N = 720,555)
celeb_freq = read.csv("./data/celebs-followed-half-random-users-FREQ-TAB.csv")



# MAIN
#===============================================================================
# - calculate percentage of all random users that follow at least 1 of the 2,624 
#   elite accounts in our list --> 40.4%
round((sum(elite_freq$n) - elite_freq$n[elite_freq$lib_con_mod == 0]) / 
  sum(elite_freq$n), 3) * 100

# - calculate percentage of all random users that follow at least 3 of the 2,624 
#   elite accounts in our list --> 22.7% --> 23%
round(sum(elite_freq$n[elite_freq$lib_con_mod > 2]) / sum(elite_freq$n), 3) * 100

# - calculate percentage of random users that follow at least 1 of the 1,000
#   celebrities in https://gist.github.com/mbejda/9c3353780270e7298763
#   --> 70.8%
round((sum(celeb_freq$num_users) - celeb_freq$num_users[celeb_freq$celebs_followed == 0]) / 
        sum(celeb_freq$num_users), 3) * 100

# - calculate percentage of random users that follow at least 3 of the 1,000
#   celebrities in https://gist.github.com/mbejda/9c3353780270e7298763
#   --> 53.3%
round(sum(celeb_freq$num_users[celeb_freq$celebs_followed > 2]) / 
        sum(celeb_freq$num_users), 3) * 100
