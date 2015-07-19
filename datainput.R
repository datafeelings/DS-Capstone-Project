# Setup project workspace
ifelse(!dir.exists(file.path("~/Coursera/Capstone/Source")),dir.create(file.path("~/Coursera/Capstone/Source")), FALSE)
ifelse(!dir.exists(file.path("~/Coursera/Capstone/Code")),dir.create(file.path("~/Coursera/Capstone/Code")), FALSE)
ifelse(!dir.exists(file.path("~/Coursera/Capstone/Figures")),dir.create(file.path("~/Coursera/Capstone/Figures")), FALSE)
ifelse(!dir.exists(file.path("~/Coursera/Capstone/Processed_data")),dir.create(file.path("~/Coursera/Capstone/Processed_data")), FALSE)
setwd("~/Coursera/Capstone")

#Download the data
# if (!file.exists("Coursera-SwiftKey.zip")) 
# {
#   download.file(url = "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip", destfile = "~/Documents/Disk D/Coursera/Capstone/Source/Coursera-SwiftKey.zip", method = "curl")
#   system("unzip Coursera-SwiftKey.zip")
# }

#Load the data into the workspace
library("tm")
docs = Corpus(DirSource("Source/final/ru_RU"), readerControl = list(language = "ru_RU", load = F))