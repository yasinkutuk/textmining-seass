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

# Hatalı paket kurulumu olursa buradan
# Önce devtools, sonra diğerleri (ben pas geçiyorum)
install.packages('devtools')
library(devtools)
devtools::install_github("thomasp85/ggraph#78")


# VEYA ###

# Packages ----------------------------------------------------------------

library(dplyr)
library(tidyr)
library(tidytext)
library(stringr)
library(janeaustenr)
library(ggplot2)
library(scales)
library(gutenbergr)

############

# Chapter 1 - The Tidy Text Format

############


# Sample text
text <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")

# Convert to data frame with a line variable to track placement
text_df <- data_frame(line = 1:4, text = text)

# Tokenization - break text into one token per line
text_df %>% 
  tidytext:: unnest_tokens(word, # output column into which the text is unnested
                           text) # input column - where the text comes from

text_df %>% 
  tidytext:: unnest_tokens(word, text)


# Her bir kelimeyi, harfi veya isaretciyi biletleştirdik.


# Working with Jane Austen text -------------------------------------------

# Pulling six Jane Austen novels
original_books <- janeaustenr::austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(), # track row number
  chapter = cumsum(str_detect(text,  regex("^chapter [\\divxlc]", # track chapters
  ignore_case = TRUE)))) %>% 
  ungroup()

original_books


# Tokenize
tidy_books <- original_books %>%
  unnest_tokens(word, text)

tidy_books

# Tokenization temel mantigi icin, Table 1 in:
# https://ceur-ws.org/Vol-450/paper9.pdf

# Genellikle stop words işlenmez!

# Remove stop words (e.g., "the", "of")
data(stop_words) # from tidytext paclage


tidy_books <- tidy_books %>%
  anti_join(stop_words) # anti_join ile aslında çıkartıyoruz


# Most common words (basit frekans tablosu)
tidy_books %>% 
  count(word, sort = TRUE)


# Plot it bar gösterim ile
tidy_books %>%
  count(word, sort = TRUE) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() + 
  theme_minimal()


# Gutenbergr Package ------------------------------------------------------

# Gutenberg kitap ID'leri şuradan (Time Machine by HG Wells:
# https://www.gutenberg.org/ebooks/35

# HG Wells
# The Time Machine (tek sevdiğim kitap) - 35,
# The War of the Worlds - 36, 
# The Invisible Man -5230,
# The Island of Doctor Moreau - 159,
hgwells <- gutenberg_download(c(35, 36, 5230, 159))

tidy_hgwells <- hgwells %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_hgwells %>%
  count(word, sort = TRUE)


# Dostoyevsky
# Crime and Punishment - 2554,
# The Brothers Karamazov - 28054, 
# Notes from the Underground - 600, 
# The Gambler - 2197.

dosto <- gutenberg_download(c(2554, 28054, 600, 2197))

tidy_dosto <- dosto %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

count(tidy_bronte, word, sort = TRUE)



# All together now
frequency <- bind_rows(mutate(tidy_dosto, author = "Dostoyevsky"),
                       mutate(tidy_hgwells, author = "H.G. Wells"), 
                       mutate(tidy_books, author = "Jane Austen")) %>%
  # Avoid UTF-8 encoded text with underscores recording the underscores as words
  mutate(word = str_extract(word, "[a-z']+")) %>% 
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  spread(author, proportion) %>% 
  gather(author, proportion, `Dostoyevsky`:`H.G. Wells`)



# Plot 
ggplot(frequency, aes(x = proportion, y = `Jane Austen`, color = abs(`Jane Austen` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  facet_wrap(~author, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "Birikimli Orani", x = NULL)



# İki yazarın üslubu nasil karşılaştırılır?
# Kelime proportion data içinden PCC test ile:

cor.test(data = frequency[frequency$author == "Dostoyevsky",],
         ~ proportion + `Jane Austen`)

cor.test(data = frequency[frequency$author == "H.G. Wells",], 
         ~ proportion + `Jane Austen`)
