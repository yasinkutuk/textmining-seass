## -*- coding: utf-8 -*-
#"""
#Created on Sat Nov 27 10:19:04 2023

#####################################################################
#                       _         _            _           _        #
#    _   _   __ _  ___ (_) _ __  | | __ _   _ | |_  _   _ | | __    #
#   | | | | / _  |/ __|| || '_ \ | |/ /| | | || __|| | | || |/ /    #
#   | |_| || (_| |\__ \| || | | ||   < | |_| || |_ | |_| ||   <     #
#    \__, | \__,_||___/|_||_| |_||_|\_\ \__,_| \__| \__,_||_|\_\    #
#    |___/                                                          #
#    ____                            _  _                           #
#   / __ \   __ _  _ __ ___    __ _ (_)| |    ___  ___   _ __ ___   #
#  / / _  | / _  || '_   _ \  / _  || || |   / __|/ _ \ | '_   _ \  #
# | | (_| || (_| || | | | | || (_| || || | _| (__| (_) || | | | | | #
#  \ \__,_| \__, ||_| |_| |_| \__,_||_||_|(_)\___|\___/ |_| |_| |_| #
#   \____/  |___/                                                   #
#####################################################################
#@author: Yasin KÜTÜK          ######################################
#@web   : yasinkutuk.com       ######################################
#@email : yasinkutuk@gmail.com ######################################
#####################################################################
#"""
#


# Leave empty of environment
rm(list=ls())


#Initials#####
options(digits = 4)
if(.Platform$OS.type=="windows"){
  path='d://DRIVE//Dropbox//_Courses//2023-2024//Fall//TextMining-SEASS//'
  respath='d://DRIVE//Dropbox//_Courses//2023-2024//Fall//TextMining-SEASS//'
  print("Hocam Windows'dasın!")
} else {
  path='/media/DRIVE/Dropbox/_Courses/2023-2024/Fall/TextMining-SEASS/'
  respath='/media/DRIVE/Dropbox/_Courses/2023-2024/Fall/TextMining-SEASS/'
  print("Abi Linux bu!")
}



# Check to see if packages are installed. Install them if they are not, then load them into the R session.
check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}



# Usage example
packages<-c('openxlsx','dplyr','data.table', 'plotly', 'RColorBrewer', 'panelr', 'tidyverse',
            'tidyr', 'tidytext', 'stringr', 'janeaustenr', 'ggplot2', 'scales', 'gutenbergr',
            'ggraph', 'widyr', 'igraph', 'widyr')
check.packages(packages)


# VEYA ###

# Packages ----------------------------------------------------------------

library(dplyr)
library(tidytext)
library(janeaustenr)
library(tidyr)
library(ggplot2)
library(ggraph)
library(igraph)
library(widyr)



############

# Chapter 4 - Relationships Between Words

############



# Tokenizing by n-gram ----------------------------------------------------

# Tokenize by consecutive sequences of words (n-grams). Specify the number of 
# words in the sequence with n. Two-word n-grams are bigrams

austen_bigrams <- austen_books() %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

austen_bigrams

# Counting and filtering n-grams ------------------------------------------

# Most common bigrams
austen_bigrams %>%
  count(bigram, sort = TRUE)



# Lots of stop words. Use separate() from tidyr to split bigrams into two columns 
## and remove stop words

bigrams_separated <- austen_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>% # Filtering out bigrams with stop words
  filter(!word2 %in% stop_words$word)


# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigram_counts # Characters

# Now re-combine with unite() - inverse of separate() function
bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigrams_united


# Trigrams
austen_books() %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE)



# Analyzing bigrams -------------------------------------------------------
# Most common streets
bigrams_filtered %>%
  filter(word2 == "street") %>%
  count(book, word1, sort = TRUE)


# Bu sefer bi-gram icin tf-itf cikaralim
bigram_tf_idf <- bigrams_united %>%
  count(book, bigram) %>%
  bind_tf_idf(bigram, book, n) %>%
  arrange(desc(tf_idf))


bigram_tf_idf

bigram_tf_idf %>%
  arrange(desc(tf_idf)) %>%
  mutate(bigram = factor(bigram, levels = rev(unique(bigram)))) %>% # for the sort?
  group_by(book) %>% 
  top_n(15) %>% 
  ungroup %>%
  ggplot(aes(bigram, tf_idf, fill = book)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~book, ncol = 2, scales = "free") +
  coord_flip()


# Using bigrams to provide context in sentiment analysis ------------------

# Helpful for determining when words are preceded by "not"
bigrams_separated %>%
  filter(word1 == "not") %>%
  count(word1, word2, sort = TRUE)


# Can use bigrams with not to reassign sentiments
## Using AFINN which gives a numeric score to words based on positive/negative
## sentiment.
AFINN <- get_sentiments("afinn")

AFINN

# Examine words most commonly preceded by "not" AND associated with a sentiment
not_words <- bigrams_separated %>%
  filter(word1 == "not") %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word2, value, sort = TRUE)  %>%
  ungroup()

not_words


# Determine which words contributed the most to the "wrong" direction of sentiment
## Multiply by the number of times the word appeared with a "not" preceding it
not_words %>%
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(word2, n * value, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) + # This is a good trick. stat_identity as default
  xlab("Oncesinde 'not' yazan kelimeler by \"not\"") + # escapes for " !
  ylab("Sentiment score * tekrar sayisi") +
  coord_flip()


# Ek olumsuzluk belirten kelimeler ekleyebiliriz, aklima gelenler
negation_words <- c("not", "no", "never", "without")

negated_words <- bigrams_separated %>%
  filter(word1 %in% negation_words) %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word1, word2, value, sort = TRUE) %>%
  ungroup() %>% 
  group_by(word1) %>% 
  arrange(desc(n)) %>% 
  filter(row_number() <= 20) %>% 
  ungroup()

  
negated_words %>% 
  mutate(contribution = n * value) %>%
  arrange(desc(contribution)) %>% 
  # mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(reorder(word2, contribution), contribution, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) + # This is a good trick. stat_identity as default
  xlab("Oncesinde 'not' yazan kelimeler by") + 
  ylab("Sentiment score * number of occurrences") +
  facet_wrap(~word1, ncol = 2, scales = "free") +
  coord_flip()



# Visualizing a network of bigrams with ggraph ----------------------------

# First igraph package - graph_from_data_frame() - takes columns of the 
## node an edge is coming "from" and where it is going "to", 
## as well as edge attributes

bigram_counts

# Filter for most common
bigram_graph <- bigram_counts %>%
  filter(n > 20) %>%
  graph_from_data_frame()

bigram_graph

# Interesting - notes that igraph has graphing functions built in but that 
## they are not its main purpose, so other packages have been built to visualize
## igraph objects. They recommend ggraph because it uses the grammar of graphics
## like ggplot2.

set.seed(2017)

ggraph(bigram_graph, layout = "fr") + # converts to ggraph object
  geom_edge_link2() + # add layers
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)




# Counting and correlating among sections ---------------------------------

# Look at Pride & Prejudice in 10-line sections and see what words tend to 
## co-occur within sections

austen_section_words <- austen_books() %>%
  filter(book == "Pride & Prejudice") %>%
  mutate(section = row_number() %/% 10) %>%
  filter(section > 0) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word)

austen_section_words

# pairwise_count() function will count common pairs of words in each section
word_pairs <- austen_section_words %>%
  pairwise_count(word, section, sort = TRUE)

word_pairs

# Find words that most commonly co-occur with "Darcy"
word_pairs %>%
  filter(item1 == "darcy")

# Pairwise correlation ----------------------------------------------------

# Examining correlations between words tells us how often they occur together
## relative to how often they occur separately

# Will use the "phi coefficient" which is a common measure of binary correlation.
## This is equivalent to Pearson when applied to binary data (I've heard this
## described as point-biserial as well, I believe)

# Use pairwise_cor() function to determine phi coefficient between words
## appearing in the same section

# we need to filter for at least relatively common words first
word_cors <- austen_section_words %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, section, sort = TRUE)

word_cors

# Find words most correlated with "pounds"
word_cors %>%
  filter(item1 == "pounds")

# Let's us identify other interesting words and find correlations with them
word_cors %>%
  filter(item1 %in% c("elizabeth", "pounds", "married", "pride")) %>%
  group_by(item1) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") + # could do geom_col() here with identity as default
  facet_wrap(~ item1, scales = "free") +
  coord_flip()



# Now visualize correlations/clusters of words that co-occur often as a network
set.seed(2016)

word_cors %>%
  filter(correlation > .20) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link2(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

