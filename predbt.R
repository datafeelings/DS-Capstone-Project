

# Reading the sample ------------------------------------------------------

read.start.time <- Sys.time()
#setwd("Coursera/Capstone/")
require("tm")
require("SnowballC")
library(rJava)
library(RWeka)
library(data.table)
library(reshape2)

samp = Corpus(DirSource("Processed_data"), 
              readerControl = list(language = "en_EN", load = F))


read.end.time <- Sys.time()
read.time.taken <- read.end.time - read.start.time

cat("Libraries loaded, sample loaded in", " ", read.time.taken, "s", "\n")

# Preprocessing the text --------------------------------------------------

prep.start.time <- Sys.time()

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


prep.end.time <- Sys.time()
prep.time.taken <- prep.end.time - prep.start.time

cat("Sample cleaning completed in", " ",prep.time.taken, "s", 
    "\n" , "Start tokenization...")

# Generating bigrams and trigrams and counting freqs -------------

ngram.start.time <- Sys.time()

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))

tdmbi <- TermDocumentMatrix(samp_clean, control = list(tokenize = BigramTokenizer))
tdmtri <- TermDocumentMatrix(samp_clean, control = list(tokenize = TrigramTokenizer))

# Bigram probabilities
m2 = as.matrix(tdmbi)
m2.df = data.frame(m2)
m2.df$keywords = rownames(m2.df)
m2.df.melted = melt(m2.df)
colnames(m2.df.melted) = c("Keyword","Source","Freq")
m2.df$sums = rowSums(m2.df[,1:3]) # calculate the sum of bigram occurences

bif = m2.df[,4:5]
bif$probability = bif$sums/length(bif$sums)
bif = as.data.table(bif)
setkey(x=bif, keywords)

rm(m2, m2.df, m2.df.melted, tdmbi, samp, samp_clean)

# Trigram probabilities
m3 = as.matrix(tdmtri)
m3.df = data.frame(m3)
m3.df$keywords = rownames(m3.df)
m3.df.melted = melt(m3.df)
colnames(m3.df.melted) = c("Keyword","Source","Freq")
m3.df$sums = rowSums(m3.df[,1:3]) # calculate the sum of trigram occurences

trif = m3.df[,4:5]
trif$probability = trif$sums/length(trif$sums)
trif = as.data.table(trif)
setkey(x=trif, keywords)


rm(m3, m3.df, m3.df.melted, tdmtri)

ngram.end.time <- Sys.time()
ngram.time.taken <- ngram.end.time - ngram.start.time

cat("Tokenization finished in", " ",ngram.time.taken, "s", 
    "\n" , "Building model...", "\n")



# Searching for most probable matches -------------



predbt = function(userinput) {
 
  # add starting caret for search
  userinput = tolower(userinput)
  userinput = removePunctuation(userinput)
  inputsearch = paste0 ("^", userinput,"\ ") 

  # search for the input word and return 5 with highest probability
 probbi = bif[keywords %like% inputsearch][order(-probability)][1:5, keywords, probability]
 probtri = trif[keywords %like% inputsearch][order(-probability)][1:5, keywords, probability]
 
 # cut out the search term and return the next possible words only
 ansbi = probbi [, keywords]
 ansbi = gsub(pattern = inputsearch, replacement = "", x = ansbi)
 
 anstri = probtri [,keywords]
 anstri = gsub(pattern = inputsearch, replacement = "", x = anstri)
 
 cat(ansbi,"\n", anstri,sep = "|")
}


cat("Model ready")


