
# Plot top cooccurrences with color depending on polarity
topCoocPolPlot <- function(cleanTweetsText, topcoocdataf){
  a <- as.numeric(as.character(topcoocdataf[[3]][1]))
  b <- as.numeric(as.character(topcoocdataf[[3]][2]))
  c <- as.numeric(as.character(topcoocdataf[[3]][3]))
  d <- as.numeric(as.character(topcoocdataf[[3]][4]))
  e <- as.numeric(as.character(topcoocdataf[[3]][5]))
  numb <- c(a,b,c,d,e)
  lab <- c(paste(topcoocdataf[1,1], topcoocdataf[1,2], sep = " & "))
  lab <- c(lab, paste(topcoocdataf[2,1], topcoocdataf[2,2], sep = " & "))
  lab <- c(lab, paste(topcoocdataf[3,1], topcoocdataf[3,2], sep = " & "))
  lab <- c(lab, paste(topcoocdataf[4,1], topcoocdataf[4,2], sep = " & "))
  lab <- c(lab, paste(topcoocdataf[5,1], topcoocdataf[5,2], sep = " & "))
  op <- par(mar=c(8,4,4,2)) # Allows the names.arg below the barplot to be visible
  colz <- ifelse(topcoocdataf$polarity<=-1, "indianred1",
                 ifelse(topcoocdataf$polarity>=1, "olivedrab1", "honeydew3"))
  barplot(numb, las = 2, names.arg = lab,
          col = colz,
          main =sprintf("TOP CO-OCCURENCES\nStudy : %s | Number of tweets : %i", keyword, length(tweetsText)),
          ylab = "Co-occurence frequency")
  legend("topright", inset=.05, title="Polarity",
         legend=c("Positive", "Neutral", "Negative"),
         fill=c("olivedrab1", "honeydew3","indianred1"))
  rm(op)
}

# comparison
polarityVerdict <- function(){
  TWPSum <- as.numeric(topWordsPol[1,3])+as.numeric(topWordsPol[2,3])+as.numeric(topWordsPol[3,3])
  TCPSum <- as.numeric(topCoocPol[1,6])+as.numeric(topCoocPol[2,6])+as.numeric(topCoocPol[3,6])
  generalPolarity <- function(PSum){
    if(PSum == 0){
      verdict <- 'neutral'
    }else if (PSum < 0){
      verdict <- 'negative'
    }else if (PSum > 0){
      verdict <- 'positive'
    }else{
      verdict <- 'error'
    }
    return(verdict)
  }
  TWPVerdict <- generalPolarity(TWPSum)
  TCPVerdict <- generalPolarity(TCPSum)

  cat("\nTop 3 words overall polarity: ", TWPVerdict, "\n")
  print(as.character(topWordsPol[1,1]))
  print(as.character(topWordsPol[2,1]))
  print(as.character(topWordsPol[3,1]))
  cat("\nTop 3 co-occurrences overall polarity: ", TCPVerdict,"\n")
  print(paste(topCoocPol[1,1], topCoocPol[1,2], sep = " & "))
  print(paste(topCoocPol[2,1], topCoocPol[2,2], sep = " & "))
  print(paste(topCoocPol[3,1], topCoocPol[3,2], sep = " & "))

  if (TWPVerdict == TCPVerdict){
    cat("\nOverall study polarity: ", TWPVerdict)
  }else {
    cat("\nOverall study polarity: Cannot be defined")
    cat("\nTop 3 Words -> ", TWPVerdict)
    cat("\nTop 3 Co-Occurences -> ", TCPVerdict)
  }
}

#Twitter authentification
twitterAuth()

# Keyword for study
keyword <- "Microsoft"

# Number of tweets for study
numberoftweets <- 5000

# Extracting Tweets
rawTweets <- extractTweets(keyword, numberoftweets)

# Cleaning Tweets (1)
tweetsText <- cleanTweets(rawTweets)

# Searching for top words
topWords <- topWordsFrequency(tweetsText)
topWords[1:15,] # (2)

# Searching for top co-occurences
topfivecooccurrences <- topcooccurences(tweetsText)
topfivecooccurrences # (2)

# Adding polarity to top words and plotting to bar graph
topWordsPol <- wordPol(topWords[1:15,])
topWordsPolPlot(tweetsText,topWordsPol) # (3)

# Adding polarity to top cooccurrences and plotting to bar graph
topCoocPol <- coocPol(topfivecooccurrences)
topCoocPolPlot(tweetsText,topCoocPol) # (3)

# Defining Study's Overall Polarity (3)
polarityVerdict()


# Scoring function version 1 -> english opinion lexicon
scoringV1 <- function(cleanTweetsText){

  print("Loading Hu & Liu opinion lexicon")

  hu.liu.pos = scan(file.path(dataDir, 'opinion-lexicon-English', 'positive-words.txt'), what='character', comment.char=';')
  hu.liu.neg = scan(file.path(dataDir, 'opinion-lexicon-English', 'negative-words.txt'), what='character', comment.char=';')

  polarityScore = function(sentences, pos.words, neg.words, .progress='none'){
    require(plyr)
    require(stringr)

    scores = laply(sentences, function(sentence, pos.words, neg.words) {

      word.list = str_split(sentence, '\\s+')
      words = unlist(word.list)
      pos.matches = match(words, pos.words)
      neg.matches = match(words, neg.words)
      pos.matches = !is.na(pos.matches)
      neg.matches = !is.na(neg.matches)
      score = sum(pos.matches) - sum(neg.matches)

      return(score)
    }, pos.words, neg.words, .progress=.progress )

    scores.df = data.frame(score=scores, text=sentences)
    return(scores.df)
  }

  library(plyr)
  polarityScoreV1 <- polarityScore(cleanTweetsText, hu.liu.pos, hu.liu.neg)$score
  return(polarityScoreV1)
}

scoringV2 <- function(cleanTweetsText){
  library(sentimentr)
  polarityScoreV2 <- sentiment(cleanTweetsText)[,4]$sentiment
  return(polarityScoreV2)
}
scoringV3 <- function(cleanTweetsText){
  library(syuzhet)
  polarityScoreV3 <- get_nrc_sentiment(cleanTweetsText)[,9:10]
  polarityScoreV3 <- cbind(polarityScoreV3, Total = polarityScoreV3[2] - polarityScoreV3[1])
  polarityScoreV3 <- polarityScoreV3[,3]
  return(polarityScoreV3)
}
polarity = function(scoreTable){
  polarityTable <- data.frame()
  for (score in scoreTable){
    if (score > 0){
      temp <- data.frame("polarity" = "Positive")
      polarityTable <- rbind(polarityTable, temp)
    } else if (score < 0) {
      temp <- data.frame("polarity" = "Negative")
      polarityTable <- rbind(polarityTable, temp)
    } else
      {
      temp <- data.frame("polarity" = "Neutral")
      polarityTable <- rbind(polarityTable, temp)
    }
  }
  return(polarityTable$polarity)
}

# plot grouped bars of three sentiment analyses
triplePlot <- function(cleanTweetsText, polarityV1, polarityV2, polarityV3){
  df <- data.frame(text=cleanTweetsText,
                   "english.opinion.lexicon"=polarityV1,
                   "sentimentr.package"=polarityV2,
                   "syuzhet.package"=polarityV3,
                   stringsAsFactors=FALSE)

  # Melting data frame for grouped bar plotting
  library(reshape)
  melteddf <- melt(df, id="text")

  # Plotting 3 scoring methods on one graph
  library(ggplot2)
  ggplot(melteddf, aes(value, fill=variable)) +
    geom_bar(position="dodge", color="black") +
    labs(x="Polarity Categories", y="Number of Tweets", fill="Scoring Method",
         title=sprintf("Tweets Polarity\nKeyword: %s | Number of tweets: %i\nDate: %s", keyword, length(cleanTweetsText), Sys.time())) +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_fill_brewer(palette="Set1")
}
# Function to plot using plot_ly
triplePlotly <- function(cleanTweetsText, polarityV1, polarityV2, polarityV3){
  library(plotly)
  df <- data.frame(text=cleanTweetsText,
                   "english.opinion.lexicon"=polarityV1,
                   "sentimentr.package"=polarityV2,
                   "syuzhet.package"=polarityV3,
                   stringsAsFactors=FALSE)
  countV1 <- table(df$english.opinion.lexicon)
  countV2 <- table(df$sentimentr.package)
  countV3 <- table(df$syuzhet.package) # Ordered by appearance!

  countdf <- data.frame(polarity=c("Negative","Neutral","Positive"),
                        scorecountV1=c(countV1[["Negative"]],countV1[["Neutral"]],countV1[["Positive"]]),
                        scorecountV2=c(countV2[["Negative"]],countV2[["Neutral"]],countV2[["Positive"]]),
                        scorecountV3=c(countV3[["Negative"]],countV3[["Neutral"]],countV3[["Positive"]]),
                        stringsAsFactors=FALSE)
  countdf

  plot_ly(countdf, x = ~polarity, y = ~scorecountV1, type = 'bar',
          name = 'english opinion lexicon', marker = list(color = 'rgba(100,100,100)')) %>%
    add_trace(y = ~scorecountV2, name = 'sentimentr package') %>%
    add_trace(y = ~scorecountV3, name = 'syuzhet package') %>%
    layout(xaxis = list(title = 'Polarity Categories'),
           title = sprintf('Tweets Polarity<br>Keyword: %s | Number of tweets: %i<br>Date: %s', keyword, length(cleanTweetsText), Sys.time()),
           titlefont = list(size = 10, color = "black"),
           legend = list(bgcolor = "#E2E2E2", x = 100, y = 0.5),
           yaxis = list(title = 'Number of Tweets'),
           barmode = 'group')
}

# Processing
scoreV1 <- scoringV1(tweetsText)
scoreV2 <- scoringV2(tweetsText)
scoreV3 <- scoringV3(tweetsText)
polarityV1 <- polarity(scoreV1)
polarityV2 <- polarity(scoreV2)
polarityV3 <- polarity(scoreV3)
triplePlot(tweetsText, polarityV1, polarityV2, polarityV3)

# extract and plot sentiment scoring
sentimentScoring <- function(cleanTweetsText){
  library(syuzhet)
  sentiment <- get_nrc_sentiment(cleanTweetsText)[,1:8]
  sentimentTotals <- data.frame(colSums(sentiment[,c(1:8)]))
  names(sentimentTotals) <- "count"
  sentimentTotals <- cbind("sentiment" = rownames(sentimentTotals), sentimentTotals)
  rownames(sentimentTotals) <- NULL
  ggplot(data = sentimentTotals, aes(x = sentiment, y = count)) +
    geom_bar(aes(fill = sentiment), stat = "identity") +
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
    xlab("Sentiment") + ylab("Frequency") +
    ggtitle(sprintf("SENTIMENTS SCORES\nStudy : %s | Number of tweets : %i", keyword, length(cleanTweetsText)))
}
sentimentScoring(tweetsText)

# plot wordcloud of top words
topWordsWordcloudPlot <- function(freqCount, wordcloudNumber){
  wordcloud(freqCount$word[1:wordcloudNumber], freqCount$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))
}
topWordsWordcloudPlot(topWords,50)

# plot top words used in pool of tweets
topWordsBasicPlotting <- function(cleanTweetsText, freqCount, topNumber){
  op <- par(mar=c(6,4,4,2)) # Allows the names.arg below the barplot to be visible
  barplot(freqCount$freq[1:topNumber], las = 2, names.arg = freqCount$word[1:topNumber],
          col ="lightblue",
          main =sprintf("TOP 15 WORDS\nStudy : %s | Number of tweets : %i", keyword, length(cleanTweetsText)),
          ylab = "Word frequency")
  rm(op)
}
# Plotting top words in bar graph
topWordsBasicPlotting(tweetsText,topWords,15)
# Function to plot top co-occurences
cooccurencesBasicPlotting <- function(topcoocdataf){
  a <- as.numeric(as.character(topcoocdataf[[3]][1]))
  b <- as.numeric(as.character(topcoocdataf[[3]][2]))
  c <- as.numeric(as.character(topcoocdataf[[3]][3]))
  d <- as.numeric(as.character(topcoocdataf[[3]][4]))
  e <- as.numeric(as.character(topcoocdataf[[3]][5]))
  numb <- c(a,b,c,d,e)
  lab <- c(paste(topcoocdataf[1,1], topcoocdataf[1,2], sep = " & "))
  lab <- c(lab, paste(topcoocdataf[2,1], topcoocdataf[2,2], sep = " & "))
  lab <- c(lab, paste(topcoocdataf[3,1], topcoocdataf[3,2], sep = " & "))
  lab <- c(lab, paste(topcoocdataf[4,1], topcoocdataf[4,2], sep = " & "))
  lab <- c(lab, paste(topcoocdataf[5,1], topcoocdataf[5,2], sep = " & "))
  op <- par(mar=c(8,4,4,2)) # Allows the names.arg below the barplot to be visible
  barplot(numb, las = 2, names.arg = lab,
          col = "lightblue",
          main =sprintf("TOP CO-OCCURENCES\nStudy : %s | Number of tweets : %i", keyword, length(tweetsText)),
          ylab = "Co-occurence frequency")
  rm(op)
}
# Plotting top cooccurences in bar graph
cooccurencesBasicPlotting(topfivecooccurrences)

