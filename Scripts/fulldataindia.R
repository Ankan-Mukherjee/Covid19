library(tidyverse)
n<-1:12
dir<-"C:/Users/Ankan Mukherjee/Documents/Covid 19/Data/Raw/"

main<-read.csv(file=paste(dir,"raw_data.csv",sep=""),sep=",")
#head(main)

data<-main%>%select(currentstatus,dateannounced,detectedcity,detecteddistrict,detectedstate,source1,statecode,numcases)
#head(data)

data%>%group_by(detectedstate)%>%
  ggplot(aes(detectedstate,numcases%>%na.omit()%>%sum))+
    geom_bar(stat="identity")
