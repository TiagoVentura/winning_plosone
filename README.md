## Replication Materials: _Winning! Election Returns and Engagement in Social Media_

Replication materials for Calvo, Ventura, Aruguete and Waisbord "Winning! Election Returns and Engagement in Social Media", PLOS ONE, 2023,

> __Abstract:__
> This article analyzes social media engagement when elections are adjudicated to one of the contending parties. We extend existing models of political dialogue to explain differences in social media engagement (i.e. time-to-retweet) when users support the winner or losers of an election. We show that users who support the winning candidate are more engaged and have a lower time-to-retweet. We also show heterogeneity in Twitter engagement conditional on the number of followers, with accounts with more followers being less sensitive to the election result. We measure the effect of electoral adjudication using a regression discontinuity design, with estimates by winning or losing status, and for accounts with many followers (high authority) or with few followers (low authority). Analyses use Twitter data collected in Argentina (2019), Brazil (2018), the United Kingdom (2019), and the United States (2016).

The latest pre-print can be found [here](). The published version is [here]()

## Tutorial 

This README file provides an overview of the replications materials for the article. The R codes used in the article can be found under the folder **Codes**. The dehydrate tweets that allow for a replication of the data sources used in the articlesis under the folder **data**. All the images for the main paper are available at the folder output

If other researchers are interested in the processed datasets, please contact the authors directly. 

The codes below do not fully replicate the results since we cannot share the full hydrated datasets. However, they should work as a guide for the methods used in the paper, and could be applied to a similar collection of tweets from the Twitter API. 

## Codes

- `code_graph_theory.r`: this code replicates the figure 1a of the paper. 

- `analysis_discontinuity.r`: this code replicates the discontinuity analysis for the case of Brazil. The code for other countries are equivalent, but with different data sources. 

- `analysis_toxicity.r`: this code replicates the toxicity analysis for the case of Brazil. The code for other countries are equivalent, but with different data sources. 


## Data

Tweet ids for all the four cases are available under the folder data. 
