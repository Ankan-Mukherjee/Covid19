library(jsonlite)
library(tidyverse)

#Source of data: "https://api.covid19india.org"

dir<-"C:/Users/Ankan Mukherjee/Documents/Covid 19/Data/India_Tally/"

url<-"https://api.covid19india.org/data.json"
filename<-paste(dir,"statewise_count.json",sep="")
download.file(url, filename)


filename<-paste(dir,"statewise_count.json",sep="")
data<-fromJSON(filename)$statewise%>%as.data.frame

data<-data[,c("state","statecode","confirmed","deaths","recovered","active","deltaconfirmed","deltadeaths","deltarecovered","lastupdatedtime","migratedother","statenotes")]
data<-subset(data, statecode!="UN" & statecode!="TT")
data<-data[order(data$statecode),]

write.csv(data,paste(dir,"statewise_count.csv",sep=""), row.names = FALSE)
