library(tm)

if(!file.exists("Data/Coursera-SwiftKey.zip")) download.file(
  "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip",
  "Data/Coursera-SwiftKey.zip")
if(list.files("./Data", pattern="txt$")[1] == "") {
  l <- unzip("Data/Coursera-SwiftKey.zip", list = TRUE)
  l <- grep("en_US", l[[1]], value = TRUE)
  unzip("Data/Coursera-SwiftKey.zip", 
  files=l, junkpaths = TRUE, exdir = "./Data") 
}

files <- list.files("./Data", pattern="txt$")
filecon <- file(paste0("Data/",files[[1]]), open = "r")

newtext <- text <- ""
while(length(newtext)) {
  newtext <- scan(filecon, what="character", nlines=100, sep="\n", 
                  skip=ifelse(rbinom(1, 1, .01) == 1), 0, 100)
  text <- c(text, newtext)
}

close(filecon)
