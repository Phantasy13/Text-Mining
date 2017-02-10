

# Extract tweets
extractTweets <- function(query, numberOfTweets) {
  tweetText <- searchTwitter(query, n = numberOfTweets, lang = "en")
  return(tweetText)
}

# clean text used in cleanTweets function
clean.text <- function(txt){
  txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", txt)
  txt = gsub("@\\w+", "", txt)
  txt = gsub("[[:punct:]]", "", txt)
  txt = gsub("[[:digit:]]", "", txt)
  txt = gsub("http\\w+", "", txt)
  txt = gsub("amp", "", txt) #
  txt = gsub("[ \t]{2,}", " ", txt)
  txt = gsub("^\\s+|\\s+$", "", txt)
  stopwords_regex = paste(stopwords('en'), collapse = '\\b|\\b')
  stopwords_regex = paste0('\\b', stopwords_regex, '\\b')
  txt = stringr::str_replace_all(txt, stopwords_regex, '')

  try.tolower = function(x){
    y = NA
    try_error = tryCatch(tolower(x), error=function(e) e)
    if (!inherits(try_error, "error"))
      y = tolower(x)
    return(y)
  }

  txt = sapply(txt, try.tolower)
  txt = txt[txt != ""]
  names(txt) = NULL
  return(txt)
}

# clean tweets from extracted tweets

cleanTweets <- function(rawTweets){
  tempDF <- twListToDF(rawTweets)
  rawTweets <- unlist(tempDF$text)
  rm(tempDF)
  rawTweets <- clean.text(rawTweets)
  return(rawTweets)
}

# Function to get top frequency words
topWordsFrequency <- function(cleanTweetsText){
  freqCount <- Corpus(VectorSource(cleanTweetsText))
  freqCount <- tm_map(freqCount, removeWords, c(stopwords('english'), keyword)) # removes stopwords and keyword from top words
  freqCount <- TermDocumentMatrix(freqCount)
  freqCount <- as.matrix(freqCount)
  freqCount <- sort(rowSums(freqCount),decreasing=TRUE)
  freqCount <- data.frame(word = names(freqCount),freq=freqCount)
  return(freqCount)
}

# Function to get top 5 word co-occurences, returns dataframe
topcooccurences <- function(cleanTweetsText){
  tweetcorpus = Corpus(VectorSource(cleanTweetsText))
  tweetcorpus <- tm_map(tweetcorpus, removeWords, c(stopwords('english'), keyword))
  tweetTDM = TermDocumentMatrix(tweetcorpus)
  tweetTDM
  ttmatrix = as.matrix(tweetTDM)
  ttmatrix[ttmatrix>=1] <- 1
  ttmatrix <- ttmatrix %*% t(ttmatrix)
  coOccurrence = list(c(0,0,0),c(0,0,0),c(0,0,0),c(0,0,0),c(0,0,0))
  for (row in 1:dim(ttmatrix)[1]){
    for(col in row:dim(ttmatrix)[2]){
      if (row != col){
        for (i in 1:length(coOccurrence)){
          if(ttmatrix[row,col]>coOccurrence[[i]][3]){
            coOccurrence[[i]][1] <- rownames(ttmatrix)[row]
            coOccurrence[[i]][2] <- colnames(ttmatrix)[col]
            coOccurrence[[i]][3] <- ttmatrix[row,col]
            break
          }
        }
      }
    }
  }
  t5c <- coOccurrence
  word1 = c(t5c[[1]][1],t5c[[2]][1],t5c[[3]][1],t5c[[4]][1],t5c[[5]][1])
  word2 = c(t5c[[1]][2],t5c[[2]][2],t5c[[3]][2],t5c[[4]][2],t5c[[5]][2])
  freq = c(t5c[[1]][3],t5c[[2]][3],t5c[[3]][3],t5c[[4]][3],t5c[[5]][3])
  coocDF = data.frame(word1, word2, freq)
  return(coocDF)
}

# Function to assign scores to words
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

print("Loading Hu & Liu opinion lexicon")

hu.liu.pos = scan(file.path(dataDir, 'opinion-lexicon-English', 'positive-words.txt'), what='character', comment.char=';')
hu.liu.neg = scan(file.path(dataDir, 'opinion-lexicon-English', 'negative-words.txt'), what='character', comment.char=';')

# assign polarity and sentiment to top words
wordPol <- function(topWordsDF){

  topWordsDF$polarity <- 'none'
  # Scoring each word
  for (i in 1:nrow(topWordsDF)){
    topWordsDF[i,3] <- polarityScore(topWordsDF[i,1], hu.liu.pos, hu.liu.neg)$score
  }
  return(topWordsDF)
}

# plot top words with color depending on polarity
topWordsPolPlot <- function(cleanTweetsText, topwordsdataf){
  op <- par(mar=c(6,4,4,2))
  colz <- ifelse(topwordsdataf$polarity<=-1, "indianred1",
                 ifelse(topwordsdataf$polarity>=1, "olivedrab1", "honeydew3"))
  barplot(topwordsdataf$freq[1:nrow(topwordsdataf)], las = 2, names.arg = topwordsdataf$word[1:nrow(topwordsdataf)],
          col = colz,
          main =sprintf("TOP 15 WORDS\nStudy : %s | Number of tweets : %i", keyword, length(cleanTweetsText)),
          ylab = "Word frequency")
  legend("topright", inset=.05, title="Word Polarity",
         legend=c("Positive", "Neutral", "Negative"),
         fill=c("olivedrab1", "honeydew3","indianred1"))
  rm(op)
}

# assign polarity and sentiment to top cooccurrences
coocPol <- function(top5cooc){
  # Adding columns to dataframe
  top5cooc$polword1 <- 'none'
  top5cooc$polword2 <- 'none'
  top5cooc$polarity <- 'none'
  # Scoring each word
  for (i in 1:nrow(top5cooc)){
    top5cooc[i,4] <- polarityScore(top5cooc[i,1], hu.liu.pos, hu.liu.neg)$score
    top5cooc[i,5] <- polarityScore(top5cooc[i,2], hu.liu.pos, hu.liu.neg)$score
    top5cooc[i,6] <- as.numeric(top5cooc[i,4]) + as.numeric(top5cooc[i,5])
  }
  return(top5cooc)
}
