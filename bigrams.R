

# Reading the sample ------------------------------------------------------


setwd("Coursera/Capstone/")
require("tm")
require("SnowballC")
library(rJava)
library(RWeka)
library(data.table)

samp = Corpus(DirSource("Processed_data"), 
              readerControl = list(language = "en_EN", load = F))


# Preprocessing the text --------------------------------------------------

# Make lowercase & remove punctuation
samp_clean = tm_map(samp, tolower)

# Remove special characters and punctuation
samp_clean = tm_map(samp_clean, removePunctuation)

for(j in seq(samp_clean))   
{   
  samp_clean[[j]] = iconv(samp_clean[[j]], to="ASCII", sub = "") 
  samp_clean[[j]] <- gsub("/", " ", samp_clean[[j]])   
  samp_clean[[j]] <- gsub("@", " ", samp_clean[[j]])   
  samp_clean[[j]] <- gsub("\\|", " ", samp_clean[[j]])

}  
samp_clean = tm_map(samp_clean, PlainTextDocument)  

samp_clean = tm_map(samp_clean, removeNumbers)

# Remove profanity
#samp_clean = tm_map(samp_clean, removeWords, stopwords("english")) 
#samp_clean = tm_map(samp_clean, removeWords, profanity1) 

# Stem words and strip white space
samp_clean = tm_map(samp_clean, content_transformer(stemDocument)) 
samp_clean = tm_map(samp_clean, stripWhitespace) 


# Generating bigrams and counting freqs -------------

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
tdm <- TermDocumentMatrix(samp_clean, control = list(tokenize = BigramTokenizer))

m = as.matrix(tdm)
m.df = data.frame(m)
m.df$keywords = rownames(m.df)
library(reshape2)
m.df.melted = melt(m.df)
colnames(m.df.melted) = c("Keyword","Source","Freq")
m.df$sums = rowSums(m.df[,1:3]) # calculate the sum of occurences

bif = m.df[,4:5]
bif$probability = bif$sums/length(bif$sums)
bif = as.data.table(bif)
setkey(x=bif, keywords)

# Searching for most probable matches -------------



pred = function(input) {
 
  # add starting caret for search
  inputsearch = paste0 ("^", input) 

  # search for the input word and return 5 with highest probability
 pred = bif[keywords %like% inputsearch][order(-probability)][1:5, keywords]

 pred

}



