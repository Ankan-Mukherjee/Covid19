library(jsonlite)
library(tidyverse)

#Source of data: "https://api.covid19india.org"

n<-1:12
dir<-"C:/Users/Ankan Mukherjee/Documents/Covid 19/Data/Raw_India/"

for (i in n)
{
  url<-paste("https://api.covid19india.org/raw_data",i,".json",sep="")
  filename<-paste(dir,"raw_data",i,".json",sep="")
  download.file(url, filename)
}

for (i in n)
{
  filename<-paste(dir,"raw_data",i,".json",sep="")
  data<-fromJSON(filename)%>%as.data.frame
  names(data)<-sapply(strsplit(names(data),split=".",fixed=TRUE),function(x) (x[2]))
  if(i<=2){data<-subset(data,select=-c(backupnotes))}
  write.csv(data,paste(dir,"raw_data",i,".csv",sep=""), row.names = FALSE)
}

for (i in n)
{
  filename<-paste(dir,"raw_data",i,".csv",sep="")
  dat<-read.csv(file=filename,sep=",")
  write.table(dat,paste(dir,"raw_data",".csv",sep=""), row.names = FALSE, col.names=ifelse(i==1,TRUE,FALSE), append=TRUE, sep=",")
}