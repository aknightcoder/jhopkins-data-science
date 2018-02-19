Question1 <- function()
{
  data <- read.csv("data.csv")

  prices <- data$VAL
  
  prices <- prices[!is.na(prices)]
  
  prices <- prices[prices == 24]
  
  length(prices)
}

Question3 <- function()
{
  library(xlsx)
  
  colIndex <- 7:15
  rowIndex <- 18:23
  
  dat = read.xlsx("data.xlsx",1, colIndex = colIndex, rowIndex = rowIndex)
  
  sum(dat$Zip*dat$Ext,na.rm=T) 
}

Question4 <- function()
{
  library(XML)
  
  doc <- xmlTreeParse("data.xml", useInternalNodes = TRUE)
  
  nodes_with_zipcode <- xpathSApply(doc,"//zipcode[text() = '21231']",xmlValue)
  
  length(nodes_with_zipcode)
}

Question5 <- function()
{
  library(data.table)
  
  DT <- fread("data5.csv")
  
  print(system.time(mean(DT[DT$SEX==1,]$pwgtp15), mean(DT[DT$SEX==2,]$pwgtp15)))
  print(system.time(tapply(DT$pwgtp15,DT$SEX,mean)))
  print(system.time(mean(DT$pwgtp15,by=DT$SEX)))
 ## print(system.time(rowMeans(DT)[DT$SEX==1], rowMeans(DT)[DT$SEX==2]))
  print(system.time(DT[,mean(pwgtp15),by=SEX]))
  print(system.time(sapply(split(DT$pwgtp15,DT$SEX),mean)))
  
}