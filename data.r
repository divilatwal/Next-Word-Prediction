install.packages("tm")
install.packages("httr")
install.packages("devtools")
install.packages("base64enc")
install.packages("curl")

library(tm)
library(httr)
library(devtools)
library(twitteR)
library(base64enc)
library(curl)


rm(list=ls(all.names = T))
setwd("~/")
options(stringsAsFactors = FALSE)
URL <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
download.file(URL,destfile = "Coursera-SwiftKey.zip", method = "curl")
unzip("Coursera-SwiftKey.zip")
