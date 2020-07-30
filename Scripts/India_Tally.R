library(jsonlite)
library(tidyverse)

#Source of data: "https://api.covid19india.org"

dir<-"C:/Users/Ankan Mukherjee/Documents/Covid 19/Data/India_Tally/"

url<-"https://api.covid19india.org/data.json"
filename<-paste(dir,"case_count.json",sep="")
download.file(url, filename)


filename<-paste(dir,"case_count.json",sep="")
data<-fromJSON(filename)$cases_time_series%>%as.data.frame

start<-as.Date("30 Jan 2020",tryFormats="%d %b %Y")

data$date<-as.Date(paste(data$date,"2020",sep=""),tryFormats="%d %b %Y")
data<-data%>%mutate(t=data$date-start)
data<-data[,c("t",names(data)[!names(data) %in% "t"])]
data<-data[,c("date",names(data)[!names(data) %in% "date"])]

write.csv(data,paste(dir,"case_count.csv",sep=""), row.names = FALSE)
