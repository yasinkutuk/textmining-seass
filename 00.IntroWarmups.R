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

# The Basics of R

# First Codes

# 1. Class Oriented Programming Language (COP)
# 2. Object Oriented Programming Language (OOP)

# Assignment Symbols (= , <-)

firstobject = 5

firstobject <- 5

firstobject*20 # Now you can multiply

# Packages in R

# 01. Base Packages
# Use of Package can be done by stating 
# "nameofpackage(arg1, arg2, arg3, ....)"

help(solve) # help is a base package, then, you can directly use it!
?solve


# 02. External Packages
# Install at first
?install.packages # Look at the help file
install.packages("bigreadr") # "bigreadr" is a string variable
install.packages('bigreadr') # My style is to use ' (single quotation mark)


# Recall the content of external package
library(bigreadr)



# Removing any object
rm(firstobject)


# Vectors
x <- c(10.4, 5.6, 3.1, 6.4, 21.7)

rm(x)

assign('x', c(10.4, 5.6, 3.1, 6.4, 21.7) )


# Vector arithmetics
v <- (2*x)+1



# Regular sequence
s <- 1:30

s2 <- seq(-5, 5, by=0.2)

s4 <- seq(length=51, from=-5, by=.2)


# Repeat 
s5 <- rep(x, times=10)



# Logical vectors 
logi <- x > 13



# Missing values (# NA : Not Available)
z <- c(1:3,NA)

is.na(z)

z <- c(1:3,rep(NA, times=5))

sum(is.na(z))


# Characters or strings
letter <- c('X', 'Y')

paste0('X', 'Y')

paste0('Good', ' Morning')


# Index vectors
# Logical Vectors: True (T) and/or False (F)
T*10
F*10

y <- x[is.na(x)]
y
x
is.na(x) # Looks the missing values in the object of x (vector)

!is.na(x) # Change the meaning of package



# Positive Integral Quantities
x
length(x)

x[4] # it recalls the 4th element from object x

#0-indexed languages : Python
#1-indexed languages : R

# x[1] # Second element from a vector if Lang is 0-indexed
x[4:5]

x[4:length(x)] # Dynamic Programming


# I want to recall the last two elements of x?
x[(length(x)-1):length(x)] # # Dynamic Programming


# Negative Integral Quantities
x
x[-(3:5)] # Thsi statement is equivalent to x[1:2]


# Vector of character stings
fruit <- c(5, 10, 1, 20)
names(fruit) <- c('orange', 'banana', 'apple', 'peach')

lunch <- fruit[c('apple', 'orange')]


# Replacement with subsetting
x[x<7] <- 0



# The mode / length OR class of the objects
z <- 1:100

mode(z)
class(z)


length(x) # The number(#) of observations
length(fruit) #The number of elements




# The Conversions
z <- 0:9

#is functions
#is.numeric(z)

#as functions
digits <- as.character(z)
dig <- as.integer(digits)



# Intro to DataFrames
Name <- c('Jon', 'Bill', 'Maria', 'Ben', 'Tina')
Age <- c(23, 41, 32, 58, 26)

df <- data.frame(Name, Age)

print(df)

df$Name
df$Age

print(max(df$Age)) # The maximum of the variable
print(mean(df$Age)) # The average of the ages among these five individuals


