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
            'ggraph', 'widyr', 'igraph')
check.packages(packages)




# VEYA ###

# Packages ----------------------------------------------------------------

library(tidytext)
library(janeaustenr)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(wordcloud)
library(reshape2)

############

# Chapter 2 - Sentiment Analysis with Tidy Data

############



# The sentiments dataset --------------------------------------------------

# Three general purpose lexicons contained in the sentiments dataset (pre-loaded
## in the tidytext package). These are based on unigrams (single words).

## # AFINN - words scored from -5 (negative) to 5 (positive)
## # bing - binary (positive/negative)
## # nrc - binary (yes/no classes of emotions)


sentiments # A package ready-to-use in tidytext

# Insert 1 to indicate Yes.
get_sentiments("afinn") # pick specific lexicons

get_sentiments("bing")

get_sentiments("nrc")


# If you face an error, try to redownload it (Yasin: 24.7 MB olmali)
textdata::lexicon_nrc(delete=TRUE)

# Sentiment analysis with inner_join() ------------------------------------

# Find the most common "joy" words in Emma

# First get the data to one word per row (tokenize)
## Note that "word" is the output column. This is helpful because the sentiment
## lexicons and stop words have "word" as columns as well (easier for inner_join
## and anti_join)

tidy_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(), chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)


# Filter lexicon for joy
nrcjoy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")


# SKIP
# If error, redownload it (Yasin)
textdata::lexicon_nrc(delete=TRUE)


# Filter Emma and join with joy words
tidy_books %>%
  filter(book == "Emma") %>%
  inner_join(nrcjoy) %>%
  count(word, sort = TRUE)


# Simdi Austen'in tum kitaplarina yapalim!
janeaustensentiment <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>% # positive vs. negative
  count(book, index = linenumber %/% 80, sentiment) %>% # 80 satirlik bloklara bolelim
  spread(sentiment, n, fill = 0) %>% # place negative and positive in sep columns
  mutate(sentiment = positive - negative) # calc net positive


# Plot how sentiment changes throughout the novels. Note that x is the index
## tracking where we are in the novel
ggplot(janeaustensentiment, aes(index, sentiment, fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free_x")



# Comparing the three sentiment dictionaries ------------------------------

# Test the three lexicons on Pride and Prejudice
pride_prejudice <- tidy_books %>% 
  filter(book == "Pride & Prejudice")

pride_prejudice

# Apply the dictionaries. Note that afinn assigns a sentiment score, while 
## bing and nrc are binary / dichotomous. Will need different approach for afinn
afinn <- pride_prejudice %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(index = linenumber %/% 80) %>% 
  summarise(sentiment = sum(value)) %>% # Adding up the score (-5 to 5, neg to pos)
  mutate(method = "AFINN")


bing_and_nrc <- bind_rows(pride_prejudice %>% 
                            inner_join(get_sentiments("bing")) %>%
                            mutate(method = "Bing et al."), pride_prejudice %>% 
                            inner_join(get_sentiments("nrc") %>% 
                            filter(sentiment %in% c("positive", "negative"))) %>%
                            mutate(method = "NRC")) %>%
  count(method, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) # net positive


# Visualize the results from the three dictionaries
bind_rows(afinn,bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")


# Sonuclarin neden farklilastigini anlamak icin pos/neg sayalim
get_sentiments("nrc") %>% 
  filter(sentiment %in% c("positive", "negative")) %>% 
  count(sentiment)

get_sentiments("bing") %>% 
  filter(sentiment %in% c("positive", "negative")) %>% 
  count(sentiment) # Daha yuksek oranda negative sentiment var Bing'de




# Most common positive and negative words ---------------------------------

# Check how much each word contributed to each sentiment
bing_word_counts <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>% # that's a pretty good trick - takes top 10 obs using min_rank
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment", x = NULL) +
  coord_flip()


# Bazi kelimeler gereksiz yere baglam (context) icinde anlamsiz puanlanabilir [miss kelimesi]
# bunlari cikartmamiz lazim, dolasiyisyla stop words e özel kelimeler eklemek istersek
custom_stop_words <- bind_rows(data_frame(word = c("miss"), lexicon = c("custom")), stop_words)

# Artik sonrasinda antijoin'i custom_stop_words üzerinden yapabilirsiniz
custom_stop_words



# Wordclouds -------------------------------------------------------------

# Using wordcloud package to visualize most common words (w/o miss ile yani custom_stop_words ile)
tidy_books %>%
  anti_join(custom_stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 50))



# Comparison cloud of most common positive and negative words
## Note that sizes are not comparable across segements
tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>% # cast to matrix
  comparison.cloud(colors = c("#F8766D", "#00BFC4"), max.words = 50) # bu renklendirme ozel hex koduyla


# hex kodlarini ogrenmek icin:
# https://www.programiz.com/r/colors

# Yuukaridaki hatali (miss'i eklediğimiz custom_stop_words ile filtrelemedik)                   
tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  anti_join(custom_stop_words) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>% # cast to matrix
  comparison.cloud(colors = c("#F8766D", "#00BFC4"), max.words = 50) # bu renklendirme ozel hex koduyla


