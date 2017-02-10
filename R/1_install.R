#  installing and loading packages

# Installer

install.packages("tm")
install.packages("twitteR")
install.packages("ggplot2")
install.packages("sentimentr")
install.packages("plotly")
install.packages("reshape")
install.packages("stringr")
install.packages("plyr")
install.packages("wordcloud")
install.packages("syuzhet")
install.packages("RColorBrewer")


# Charger
#intiating libraries and packages

library(twitteR)
library(plyr)
library(stringr)
library(reshape)
library(ggplot2)
library(NLP)
library(tm)
library(RColorBrewer)
library(wordcloud)
library(sentimentr)
library(syuzhet)
library(plotly)
library(datasets)

projectDir = getwd()

codeDir = file.path(projectDir, 'R')
dataDir = file.path(projectDir, 'data')



# Authentification to twitter API
twitterAuth <- function()
{

  # Twitter API Settings; you need to create your own account

  consumer_key <- "write key here"

  consumer_secret <- "write secret here"

  access_token <- "write token here"

  access_token_secret <- "write token secret here"

  setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_token_secret)
}




