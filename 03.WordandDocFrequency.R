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

library(dplyr)
library(janeaustenr)
library(gutenbergr)
library(tidytext)
library(stringr)
library(ggplot2)


############

# Chapter 3 - Analyzing Word and Document Frequency: tf-idf

############



# Intro -------------------------------------------------------------------

# tf (term frequency) - how often a word appears in document

# idf (inverse document frequency) - decreases weight for commonly used words 
## and increases weight for words not used very much in a collection of docs

# tf-idf (tf multiplied by idf) - frequency of a term adjusted for how rarely
## it is used

# tf (term-frequency): terim ağırlıklarını hesaplamak için kullanılan yöntemdir,
# itf (inverse [document] term frequency): idf = log (Dokuman Sayısı / Terimin Geçtiği Dokuman Sayısı )
# Detayli bilgi icin:
# https://medium.com/algorithms-data-structures/tf-idf-term-frequency-inverse-document-frequency-53feb22a17c6

# Kabaca (wiki'den copy-paste):
# Örnek olarak 10 sözcükten oluşan bir metin dili ele alındığı kabul edilsin
# bu metin dilinde hazırlanan tüm metinlerde en fazla sayda kullanılan sözcüğün #
# 100 defa kullanıldığı kabul edilsin; 
# bu halde yapılan en sık kullanılandan 
# az sık kullanılan sözcüğe göre yapılan sözcük sıralaması (Zipf yasası'na göre) şöyle olacaktır:
# 1. sözcük => 100/1 = 100
# 2. sözcük => 100/2 = 50
# 3. sözcük => 100/3 = 33,3
# 4. sözcük => 100/4 = 25
# 5. sözcük => 100/5 = 20
# 6. sözcük => 100/6 = 16,6
# 7. sözcük => 100/7 = 14,3
# 8. sözcük => 100/8 = 12,5
# 9. sözcük => 100/9 = 11,1
# 10. sözcük => 100/10= 10 
# Dolayisiyla f(n) = K/n, daima sabittir.
                                                                                                                                                                                                                                                                                                             
# Term Frequency in Jane Austen Novels ------------------------------------

book_words <- austen_books() %>%
  unnest_tokens(word, text) %>%
  count(book, word, sort = TRUE) %>%
  ungroup()

total_words <- book_words %>% 
  group_by(book) %>% 
  summarize(total = sum(n))

book_words <- left_join(book_words, total_words)

book_words

# Distribution of n/total (i.e., term frequency)
## Shows lots of terms that rarely occur and fewer terms that occur frequently
ggplot(book_words, aes(n/total, fill = book)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~book, ncol = 2, scales = "free_y")



# Zipf’s Law --------------------------------------------------------------
## The frequency that a word appears is inversely proportional to its rank
# https://tr.wikipedia.org/wiki/Zipf_yasas%C4%B1?oldformat=true

freq_by_rank <- book_words %>% 
  group_by(book) %>% 
  mutate(rank = row_number(), `term frequency` = n/total)  # already ordered by n, so can use row_number()

freq_by_rank

# Zipf's law commonly visualized by plotting rank on the x-axis 
## and term frequency on the y-axis, on logarithmic scales
freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = book)) + 
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10() # Negatif (asagi) egimli ise OK!


# Plot it against egim (tanjant alfa)
freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = book)) + 
  geom_abline(intercept = -0.62, slope = -1.1, color = "gray50", linetype = 2) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()


# The bind_tf_idf function ------------------------------------------------
## tf-idf attempts to find the words that are important (i.e., common) in a text, but not too common

book_words <- book_words %>%
  bind_tf_idf(word, book, n) # tokens-documents-counts
book_words

# Check for high tf-idf words
book_words %>%
  select(-total) %>%
  arrange(desc(tf_idf)) # Characters of each novel top the list - nice

# Visualize edelim simdi (6 kitap zipf yasasina uyuyor)
book_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(book) %>% 
  top_n(15) %>% 
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = book)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~book, ncol = 2, scales = "free") +
  coord_flip()

