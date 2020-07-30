#Assumptions are 
#1. Levitt Model is obeyed.
#2. April 25, 2020 is considered as Day 0.
#3. Case numbers follow the current trend.

#Source of data: "https://api.covid19india.org/csv/latest/districts.csv"


library(tidyverse)
library(shiny)

#Start of shift Function
shift <- function(x, n)
{
  c(rep(NA, n), x[(seq(length(x)-n))])
}
#End of shift Function

#Equation of the Best Fit Line
Y<-function(X,m=1,c=0)
{
  m*X+c
}


#Equation of the Best Fit Curve
Z<-function(x,y,N)
{
  lmResult <- (lm(y~poly(x,N,raw=TRUE)))
  predict(lmResult,list(x))
}


dir<-"www/"
filename<-paste(dir,"districtwise_count.csv",sep="")

download<-TRUE

#Downloading District-Wise Data
if(download)
{
  url<-"https://api.covid19india.org/csv/latest/districts.csv"
  download.file(url, filename)
}


#Reading the Data
main<-read.csv(filename)
main$Date<-as.Date(main$Date)

states<-unique(main$State)
districts1<-unique(main$District)
districts2<-unique(main$District)

#Presetting the Parameters
f<-1.001
lowlim_rep<-20


