detach(package:ANLP, unload=TRUE)
detach(package:RWeka, unload=TRUE)


# Increase memory as Building model with large dataset required high memory and Default memory is 512 MB so upgrading it io 2GB
options( java.parameters = "-Xmx5g" )

library(ANLP)
library(ggplot2)

isDebugMode <- F
isDataPresent <- T

CorpusData = readLines("final/sample_corpus.txt")


if(!isDataPresent){
  
  print("======== Building 1,2,3,4 Gram Models ========")
  
  # Generate term document matrix for each n gram models
  # Sort these words by frequency and convert into dataframe
  # Dataframes with 2 columns: word :- Each terms and freq :- Frequency of that term
  
  # Unigram model
  unigram.df <- generateTDM(CorpusData,1,T)
  saveRDS(unigram.df,file = "final/unigram.RDS")
  
  # Bigram model
  bigram.df <- generateTDM(CorpusData,2,T)
  saveRDS(bigram.df,file = "final/bigram.RDS")
  
  # Trigram model
  trigram.df <- generateTDM(CorpusData,3,T)
  saveRDS(trigram.df,file = "final/trigram.RDS")
  
  # Quadrigram model
  quadragram.df <- generateTDM(CorpusData,4,T)
  saveRDS(quadragram.df,file = "final/quadrigram.RDS")
  
  # Fivegram model
  fivegram.df <- generateTDM(CorpusData,5,T)
  saveRDS(fivegram.df,file = "final/fivegram.RDS")
  
  
}else{
  
  unigram.df = readRDS("final/unigram.RDS")
  bigram.df = readRDS("final/bigram.RDS")
  trigram.df = readRDS("final/trigram.RDS")
  quadragram.df = readRDS("final/quadrigram.RDS")
  fivegram.df = readRDS("final/fivegram.RDS")
}

# Top 15 words using different uni gram models
ggplot(head(unigram.df,15), aes(x=reorder(word,freq), y=freq)) +
  geom_bar(stat="Identity", fill="green") +
  coord_flip() +
  geom_text(aes(label=freq),nudge_y=0.5) +
  ggtitle("Top 15 words of unigram") +
  ylab("Frequency") +
  xlab("Word")

# Top 15 words using different bi gram models
ggplot(head(bigram.df,15), aes(x=reorder(word,freq), y=freq)) +
  geom_bar(stat="Identity", fill="orchid2") +
  coord_flip() +
  geom_text(aes(label=freq),nudge_y=0.5) +
  ggtitle("Top 15 words of bigram") +
  ylab("Frequency") +
  xlab("Word")

# Top 15 words using different tri gram models
ggplot(head(trigram.df,15), aes(x=reorder(word,freq), y=freq)) +
  geom_bar(stat="Identity", fill="lightsalmon2") +
  coord_flip() +
  geom_text(aes(label=freq),nudge_y=0.5) +
  ggtitle("Top 15 words of trigram") +
  ylab("Frequency") +
  xlab("Word")

# Top 15 words using different Quadri gram models
ggplot(head(quadragram.df,15), aes(x=reorder(word,freq), y=freq)) +
  geom_bar(stat="Identity", fill="lightskyblue2") +
  coord_flip() +
  geom_text(aes(label=freq),nudge_y=0.5) +
  ggtitle("Top 15 words of quadrigram") +
  ylab("Frequency") +
  xlab("Word")