#Assumptions are 
#1. Case numbers follow the current trend.

#Source of data: "https://api.covid19india.org/csv/latest/districts.csv"


library(tidyverse)
library(gridExtra)
library(grid)

download<-FALSE

#Downloading District-Wise Data
dir<-"C:/Users/Ankan Mukherjee/Documents/Covid 19/Data/India_Tally/"
filename<-paste(dir,"districtwise_count.csv",sep="")
if(download)
{
  url<-"https://api.covid19india.org/csv/latest/districts.csv"
  download.file(url, filename)
}

#Equation of the Best Fit Curve
Y<-function(x,y,N)
{
  lmResult <- (lm(y~poly(x,N,raw=TRUE)))
  predict(lmResult,list(x))
}


#Reading the Data
main<-read.csv(filename)
main$Date<-as.Date(main$Date)

#Presetting the Parameters
f<-1.001
district<-"Thane"
lowlim_calc<-1
lowlim_rep<-1
N<-5
#districts<-unique(main$District)
#head(districts)

data<-main%>%filter(District==district)
start<-data$Date[1]

#Making Calculations on the Data
data<-data%>%
  mutate(t=as.integer(Date-start))
data<-data[,c("Date","t","Confirmed","Deceased","Recovered")]
data<-data[1:length(data$t)-1,]
data<-na.omit(data)
#data

#Presetting Some Variables
l<-data$t[length(data$t)]

#Segregating the Data
y_c<-data$Confirmed[lowlim_calc:l]
y_d<-data$Deceased[lowlim_calc:l]
y_r<-data$Recovered[lowlim_calc:l]
x<-data$t[lowlim_calc:l]

#Polynomial Modeling
lmResult <- summary(lm(y_c~x))
c<-coef(lmResult)[1]
m<-coef(lmResult)[2]


# #Plotting the Data
# plot(y~x,xlim=c(lowlim_rep,l+5),ylim=c(1.00,1.10),main=paste("Covid Cases in ",district,sep=""),xlab="Days since first case(t)",ylab="H(t)=X(t)/X(t-1)",col=rgb(0,0,255,maxColorValue=255),pch=20)
# abline(c,m,col=rgb(255,0,0,maxColorValue=255))
# points(Y(x)~x,xlim=c(lowlim_rep,l+5),ylim=c(1.00,1.10),col=rgb(255,0,0,maxColorValue=255),pch=4)
# grid(nx=20,ny=20)

d<-data.frame(x,y_c,y_d,y_r)

#Plotting the Data
p<-d%>%ggplot()
pc<-p+
  geom_point(aes(x,y_c),color="#FC4E07",shape=18,size=2)+
  stat_smooth(aes(x,y_c),method="lm",formula=y~poly(x,N,raw=TRUE),color=rgb(0,0,255,maxColorValue=255))+
  geom_point(aes(x,Y(x,y_c,N)),color=rgb(0,0,255,maxColorValue=255),shape=4)+
  xlim(lowlim_rep,l+5)+
  xlab("Days since first case(t)")+
  ylab("Cumulative Case Count")+
  # labs(
  #   title=paste("Covid Cases in ",district,sep=""),
  #   caption=paste("First Case On",start)
  #   )+ 
  theme(
    plot.title = element_text(color = "black", face = "bold", hjust=0.5),
    plot.caption = element_text(color = "black", face = "bold", hjust=0.5),
    panel.background = element_rect(fill = "#DFFFDF", colour = "black", size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "gray"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'dashed', colour = "gray"),
  )
pd<-p+
  geom_point(aes(x,y_d),color="#FC4E07",shape=18,size=2)+
  stat_smooth(aes(x,y_d),method="lm",formula=y~poly(x,N,raw=TRUE),color=rgb(0,0,255,maxColorValue=255))+
  geom_point(aes(x,Y(x,y_d,N)),color=rgb(0,0,255,maxColorValue=255),shape=4)+
  xlim(lowlim_rep,l+5)+
  xlab("Days since first case(t)")+
  ylab("Cumulative Death Count")+
  # labs(
  #   title=paste("Covid Cases in ",district,sep=""),
  #   caption=paste("First Case On",start)
  # )+ 
  theme(
    plot.title = element_text(color = "black", face = "bold", hjust=0.5),
    plot.caption = element_text(color = "black", face = "bold", hjust=0.5),
    panel.background = element_rect(fill = "#DFFFDF", colour = "black", size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "gray"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'dashed', colour = "gray"),
  )
grid.arrange(pc,pd,ncol=2,
             top=textGrob(paste("Covid Cases in ",district,sep=""),gp=gpar(fontsize=20,fontface="bold")),
             bottom=textGrob(paste("First Case On",start),gp=gpar(fontsize=10,fontface="bold",col="blue")))
