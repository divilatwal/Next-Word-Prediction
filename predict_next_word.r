# For dataframe manipulations
library(dplyr)

# NLP library
library(tm)
library(qdap)

quadragram.df <- quadragram.df %>% filter(freq > 3)
fivegram.df <- fivegram.df %>% filter(freq > 3)

# List of all the models
modelsList = list(fivegram.df,quadragram.df,trigram.df,bigram.df,unigram.df)

# Predict the next work using backoff method
testline <- "Imdian army"
predict_Backoff(testline,modelsList,F)
