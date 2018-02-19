Question1 <- function()
{
    dat <- read.csv("q1.csv")
    
    logical <- dat$ACR == 3 & dat$AGS == 6
    
    dat[which(logical),]
}

Question2 <- function()
{
  library(jpeg)
  
  download.file('https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg', destfile = 'jeff.jpg')
  
  picture <- readJPEG('jeff.jpg', native=TRUE)
  
  quantile(picture, probs = c(0.3,0.8))
}

Question3 <- function()
{
  gdp <- read.csv("GDP.csv", header=F, skip=5, nrows=190)
  
  fed <- read.csv("Fed.csv")
  
  merged <- merge(gdp, fed, by.x = 'V1', by.y = 'CountryCode', sort = TRUE)
  
  merged[with(merged, order(-V2)),]
}

Question4 <- function()
{
  dat <- Question3()
  
  print(mean(dat[dat$Income.Group =='High income: nonOECD',]$V2))
  
  print(mean(dat[dat$Income.Group =='High income: OECD',]$V2))
}

Question5 <- function()
{
  dat <- Question3()
  
  q <- quantile(dat$V2, probs = c(0.2,0.4,0.6,0.8,1))
  
  ranking <- dat$V2 <= 38
  
  xtabs(ranking ~ dat$Income.Group)
}