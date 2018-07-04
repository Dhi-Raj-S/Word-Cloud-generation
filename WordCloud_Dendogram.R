library(qdap)

#loading coffee tweets data
coffee <- read.csv(file.choose(), stringsAsFactors = FALSE)
str(coffee)

# isolate the text column from coffee to create a vector of tweets
coffee_tweets <- coffee$X.text.


# converting a vector to a corpus (corpus is a collection of documents)
library(tm)

# converting the vector of text to a document by using a source function called vector source
coffee_source <- VectorSource((coffee_tweets))

# making a volatile corpus
coffee_corpus <- VCorpus(coffee_source)

# Print out coffee_corpus
coffee_corpus

# Print data on the 15th tweet in coffee_corpus
print(coffee_corpus[[15]])

# Print the content of the 15th tweet in coffee_corpus
coffee_corpus[[15]][1]

# meta(coffee_corpus) (no meta data in the corpus)

#Cleaning the corpus with pre-processing steps
# custum function for cleaning
clean_corpus <- function(corpus){
                corpus <- tm_map(corpus, stripWhitespace)
                corpus <- tm_map(corpus, removePunctuation)
                corpus <- tm_map(corpus, content_transformer(tolower))
                corpus <- tm_map(corpus, removeWords, c(stopwords("en"), "coffee", "mug"))
                return(corpus)
                                }
# applying custum function to corpus
clean_corp <- clean_corpus(coffee_corpus)

# viewing some corpus documents
clean_corp[[1]][1]


# converting a clean corpus to TDM/DTM for analysis purposes
coffee_tdm <- TermDocumentMatrix(clean_corp)

coffee_tdm # view TDM

# Convert coffee_dtm to a matrix
coffee_mat <- as.matrix((coffee_tdm))

dim(coffee_mat) #checking dimensions


coffee_mat[148:150, 2587:2590] #view a portion of matrix

# performing row sums to get aggregate of all terms
term_frequency <- rowSums((coffee_mat))

# Sort term_frequency in descending order to get top repetative words on the top
term_frequency <- sort(term_frequency, decreasing = TRUE)

term_frequency[1:10] #view top words

# Plot a barchart of the 10 most common words

barplot(term_frequency[1:10], col = "red")

# other way to get frequent terms plot 
# Create frequency
# frequency <- freq_terms(coffee$X.text., top = 10, "Top200Words" , at.least = 3 )

# creating a word cloud
library(wordcloud)

# creating each word frequencies
word_freq <- data.frame(term = names(term_frequency), num = term_frequency)

# unigram wordcloud
# Create a wordcloud for the values in word_freqs
wordcloud(word_freq$term, word_freq$num, max.words = 100, colors = brewer.pal(8, "Dark2"))


# visualizing word networks
# word_associate(coffee$X.text., match.string = c("barista"), 
#                stopwords = c(Top200Words, "coffee", "amp"), 
#                network.plot = TRUE, cloud.colors = c("gray85", "darkred"))

# Make a distance matrix and dendrogram from a TDM
dim(coffee_tdm)
# removing the terms which are not frequent by sparcing
tdm1 <- removeSparseTerms(coffee_tdm, sparse = 0.95)
tdm1

tdm2 <- removeSparseTerms(coffee_tdm, sparse = 0.98)
tdm2
# using tdm2
tdm_m <- as.matrix(tdm2) 
tdm_df <- as.data.frame(tdm_m)

tweets_dist <-dist(tdm_df)

hc <- hclust(tweets_dist)

plot(hc)


# # Bi-gram tokenization
# library(RWeka)
# 
# # Make tokenizer function 
# tokenizer <- function(x){
#   NGramTokenizer(x, Weka_control(min = 2, max = 2))
# }
# 
# bigram_tdm <- TermDocumentMatrix(clean_corp, control = list(tokenize = tokenizer))

# tfidf method 

tfidf_tdm <- TermDocumentMatrix(clean_corp, control = list(weighting = weightTfIdf))
 

 
term_frequency_tfidf <- rowSums(as.matrix(tfidf_tdm))
term_frequency_tfidf <- sort(term_frequency_tfidf, decreasing = TRUE) 
 
word_freq_tfidf <- data.frame(term = names(term_frequency_tfidf), num = term_frequency_tfidf)
 
# unigram word cloud using tfidf
# Create a wordcloud for the values in word_freqs
wordcloud(word_freq_tfidf$term, word_freq_tfidf$num, max.words = 100, colors = brewer.pal(8, "Dark2"))
 
 