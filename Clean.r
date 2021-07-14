install.packages("ANLP")

library(ANLP)

isDebugMode <-  F
isDataPresent <-  F

if(!isDataPresent)
  {
  data.blog <-  readTextFile("final/en_US/en_US.blogs.txt","UTF-8")
  data.twitter <-  readTextFile("final/en_US/en_US.twitter.txt","UTF-8")
  data.news <-  readTextFile("final/en_US/en_US.news.txt","UTF-8")

  sample.size <-  0.15

  data.blog.sample <-  sampleTextData(data.blog,sample.size)
  data.twitter.sample <-  sampleTextData(data.twitter,sample.size)
  data.news.sample <-  sampleTextData(data.news,sample.size)

  library(tm)
  sampled.data <-  c(data.blog.sample,data.twitter.sample,data.news.sample)

  writeLines(sampled.data,"final/sample_data.txt")

}else{
  sampled.data <-  readLines("final/sample_data.txt")
}

print
myCorpus <-  cleanTextData(sampled.data)

bad.words <- readLines("final/bad_words.txt")
bad.words = bad.words %>% tolower() %>% stripWhitespace()
corpus.cl = tm_map(myCorpus,removeWords,bad.words)

writeCorpus(myCorpus,filenames = "final/sample_corpus.txt")

rm(list = ls())
