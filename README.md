# Most users do not engage with political elites on Twitter; Those who do, show overwhelming preferences for ideological congruity

This repository contains the replication material of the article "Most users do not engage with political elites on Twitter; Those who do, show overwhelming preferences for ideological congruity", to be published at _Sience Advances_, by Magdalena Wojcieszak, Andreu Casas, Xudong Yu, Jonathan Nagler, and Joshua Tucker.


## Downloading Large Files from Google Drive

A few files are too large to be stored in this repository. You can find them in the following Google Drive: https://drive.google.com/drive/folders/1EYqaSF-EukTGhanogqaevSnjn7l0koEH?usp=sharing. Before running any code, you should download the files in there and place them in the `data` directory.
  
  - `ingroup-sharing-model-data.csv`
  - `quote-tweet-db.csv`

## Data
The `./data/` directory contains the necessary data to replicate the analytical figures of the paper. Below, we describe each of the datasets in this directory (plus the dataset to be placed there from the Google Drive):

  - `actors-ideo-scores.csv`: (elite-level dataset) contains information about elite `type` (Journalist, Politician, or Media) as well as ideology score (`ideo`) for each of the political elites included in the analysis (`actor` and `actor_id` provide the Twitter screen name and user id of each elite, respectively)

  - `users-ideo-plus-ingroup-sharing-info.csv`: (user-level dataset) contains information about the ideology scores (`user_ideo`) of the ordinary users we tracked for which we were able to get a score (`user_id_anon` contains a unique anonymized identifier for the users that is consistent across datasets). In addition, this file contains information about the number of times a users has shared (including both retweets and quote tweets) from ingroup (`ingroup_shares01`) and outgroup (`outgroup_shares01`) elites; and the proportion of all elite shares that are of ingroup elites (`ingroup_prop01`).

  - `sharing-following-in-out-elites-no-moderates.csv`: (user-level dataset) in addition to containing the anonymized user identifier and ideology scores also provided in the previous dataset (`user_id_anon` and `user_ideo`), this dataset also contains a categorical ideological variable (`user_ideocat`, either Conservative or Liberal, as Moderates are excluded from the dataset) and information about the number liberal (`followed_lib`), conservative (`followed_con`), and out-group (`outgroup_followed` -- this will be equal to `followed_lib` or `followed_con` depending on the `user_ideocat`) elite accounts these users follow (from the list of elite accounts in `actors-ideo-scores.csv`), as well as the number of quote tweets (`qt_in`, `qt_out`) and retweets (`rt_in`, `rt_out`) from in and out-group elite accounts. 

  - `ingroup-sharing-model-data.csv`: (message-level dataset) contains information about the retweet and quote tweets of elite accounts by ordinary users. `tweet_id_anon` is an anonymized identifier for the tweet sent by the ordinary users (the share), `user_id_anon` is an anonymized identifer for the ordinary users (the same used in other dataset), `mentioned_userid` and `mentioned_actor` are the screen name and the Twitter ID of the elite accounts whose tweet is being shared, `tweet_type` indicates whether the tweet is a "retweet" or a "quote_tweet", `actor_ideo` and `user_ideo` contain the ideo scores of teh elite and ordinary account respectively (`user_ideocat` and `actor_ideocat` are categoricla versions of these variables: either Liberal or Conservative), `type` indicates the type of elite account (Journalist, Politician, or Media), `actor_ideoextrem` indicates the ideological extremity of the elite account (absolute difference between the average elite ideology and their own ideology), `ingroup` indicates whether the share is of an in-group elite account (=1, 0 otherwise), and `outgroup_followed` provides information about the number of out-group elite accounts followed by the ordinary user.

  - `quote-tweet-db.csv`: (message-level dataset) contains information further information about the quote tweets. The following variables are already explained in the description of the `ingroup-sharing-model-data.csv` dataset: `tweet_id_anon`, `user_id_anon`, `mentioned_tweetid`, `mentioned_actor`, `actor_ideo`, `user_ideo`, `actor_ideocat`, `user_ideocat`, `actor_type`, and `actor_ideoextrem`. Then, `cnn` provides the sentiment prediction from our CNN model (either negative, neutral, or positive), `topic` contains the topic code predicted by our CNN model, `topic_label` provides a human-readable for the topic code, `mentioned_actor_followers` contains the number of Twitter followers of the shared elite account, `user_followers` contains the number of Twitter followers of the user, `user_friends` the number of Twitter friends, `hour` contains information about the time the tweet was sent, `outgroup_followed` contains information about the number of out-group elites followed by the ordinary user. Finally, `congruence` is a binary variable indicating whether the ordinary user and the shared elite account are of the same ideological group (either ingroup or outgroup).

  - `elitenum-followed-all-random-users-FREQ-TABLE.csv`: a frequency table providing information about the number of elite accounts (out of the 2,624 elite accounts we study) (`lib_con_mod`) followed by the 1,437,774 users under study (`n`).

  - `celebs-followed-half-random-users-FREQ-TAB.csv`: a frequency table providing information about the number of celebrities (out of this list of top 1,000 clebrities on Twitter: https://gist.github.com/mbejda/9c3353780270e7298763) (`celebs_followed`) followed by ordinary users under study (`num_users`). We only conducted this celebrity analysis for half of the sample: 720,555 out of 1,437,774.


## Code
The `./code/` directory contains separate scripts to replicate each analytical figure in the main article. The `./figures/` directory contains a copy of each of the figures generated by these scripts. 

- [`01-fig01-ideo-distributions.R`](https://github.com/CasAndreu/ingroup_filtering/blob/master/code/01-fig01-ideo-distributions.R): to replicate Figure 1 of the paper, in which we show the distribution of the ideology scores for the three types of political elites we study, as well as the ordinary users.

<img src = "https://github.com/CasAndreu/ingroup_filtering/blob/master/figures/figure01.jpg">

- [`02-fig02-following-sharing-barplots.R`](https://github.com/CasAndreu/ingroup_filtering/blob/master/code/02-fig02-following-sharing-barplots.R): to replicate Figure 2 of the paper, in which we show the proportion of in/out-group accounts followed and shared, as well as the type of sharing behavior.

<img src = "https://github.com/CasAndreu/ingroup_filtering/blob/master/figures/figure02a.jpg">
<img src = "https://github.com/CasAndreu/ingroup_filtering/blob/master/figures/figure02b.jpg">

- [`03-fig03-ingroup-sharing-coef-plot.R`](https://github.com/CasAndreu/ingroup_filtering/blob/master/code/03-fig03-ingroup-sharing-coef-plot.R): to replicate Figure 3 of the paper, in which we plot the marginal effects from a logistic regression predicting in-group sharing.

<img src = "https://github.com/CasAndreu/ingroup_filtering/blob/master/figures/figure03.jpg">


- [`04-fig04-sentiment-models.R`](https://github.com/CasAndreu/ingroup_filtering/blob/master/code/04-fig04-sentiment-models.R): to replicate Figure 4 of the paper, in which we plot the marginal effects from multinomial regressions predicting the negative sentiment of quote tweets.

<img src = "https://github.com/CasAndreu/ingroup_filtering/blob/master/figures/figure04.jpg">


- [`05-fig05-sentiment-models-by-topic.R`](https://github.com/CasAndreu/ingroup_filtering/blob/master/code/05-fig05-sentiment-models-by-topic.R): to replicate Figure 5 of the paper, in which we plot the marginal effects from multinomial regressions predicting the negative sentiment of quote tweets, distingushing by the policy issue discussed in the original tweets sent by the elite actor.

<img src = "https://github.com/CasAndreu/ingroup_filtering/blob/master/figures/figure05.jpg">

- [`06-q1-analysis.R`](https://github.com/CasAndreu/ingroup_filtering/blob/master/code/06-q1-analysis.R): to replicate the findings in the paper where we address Q1, so how politically engaged the users in our random sample are (how many elites they follow on Twitter); as well as celebrities (as a point of  comparison)
