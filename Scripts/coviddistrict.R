#Assumptions are 
#1. Levitt Model is obeyed.
#2. April 25, 2020 is considered as Day 0.
#3. Case numbers follow the current trend.

#Source of data: "https://api.covid19india.org/csv/latest/districts.csv"


library(tidyverse)

download<-TRUE

#Downloading District-Wise Data
if(download)
{
  url<-"https://api.covid19india.org/csv/latest/districts.csv"
  dir<-"C:/Users/Ankan Mukherjee/Documents/Covid 19/Data/India_Tally/"
  filename<-paste(dir,"districtwise_count.csv",sep="")
  download.file(url, filename)
}



#Reading the Data
main<-read.csv(filename)
data<-main%>%filter(District==district)
data$Date<-as.Date(data$Date)
start<-data$Date[1]

#Start of shift Function
shift <- function(x, n)
{
  c(rep(NA, n), x[(seq(length(x)-n))])
}
#End of shift Function

#Presetting the Parameters
f<-1.001
district<-"Thane"
lowlim_calc<-1
lowlim_rep<-20
districts<-unique(main$District)
head(districts)

#Making Calculations on the Data
data<-data%>%
  mutate(t=as.integer(Date-start),Confirmed_prev=shift(Confirmed,1),Deceased_prev=shift(Deceased,1))
data<-data%>%
  mutate(H_confirmed=Confirmed/Confirmed_prev,H_deceased=Deceased/Deceased_prev)
data<-data[,c("Date","t","Confirmed","Confirmed_prev","H_confirmed","Deceased","Deceased_prev","H_deceased")]
data<-data[1:length(data$t)-1,]
data<-na.omit(data)
#data

#Presetting Some Variables
l<-data$t[length(data$t)]

#Segregating the Data
y<-data$H_confirmed[lowlim_calc:l]
x<-data$t[lowlim_calc:l]

#Linear Modeling
lmResult <- summary(lm(y~x))
c<-coef(lmResult)[1]
m<-coef(lmResult)[2]

#Equation of the Best Fit Line
Y<-function(X)
{
  m*X+c
}

# #Plotting the Data
# plot(y~x,xlim=c(lowlim_rep,l+5),ylim=c(1.00,1.10),main=paste("Covid Cases in ",district,sep=""),xlab="Days since first case(t)",ylab="H(t)=X(t)/X(t-1)",col=rgb(0,0,255,maxColorValue=255),pch=20)
# abline(c,m,col=rgb(255,0,0,maxColorValue=255))
# points(Y(x)~x,xlim=c(lowlim_rep,l+5),ylim=c(1.00,1.10),col=rgb(255,0,0,maxColorValue=255),pch=4)
# grid(nx=20,ny=20)

d<-data.frame(x,y)

#Plotting the Data
p<-d%>%ggplot()
p+
  geom_point(aes(x,y),color=rgb(0,0,255,maxColorValue=255),shape=20,size=2)+
  geom_abline(slope=m,intercept=c,color=rgb(255,0,0,maxColorValue=255))+
  geom_point(aes(x,Y(x)),color=rgb(255,0,0,maxColorValue=255),shape=4)+
  xlim(lowlim_rep,l+5)+
  ylim(1.00,1.10)+
  xlab("Days since first case(t)")+
  ylab("H(t)=X(t)/X(t-1)")+
  labs(title=paste("Covid Cases in ",district,sep=""),
       caption=if(m>=0)
       {
         print("Pandemic is Rising")
       } else
       {
         T<-(f-c)/m
         print(ifelse((T-l)>0,paste("Pandemic Ends Around ",round(T-l)," Days From Today.",sep=""),"Pandemic Has Ended."))
       })+ 
  theme(
         plot.title = element_text(color = "black", size = 12, face = "bold", hjust=0.5, size=100),
         plot.caption = element_text(color = "blue", face = "bold", hjust=0.5, size=20),
         panel.background = element_rect(fill = "#DFFFDF",
                                         colour = "black",
                                         size = 0.5, linetype = "solid"),
         panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                         colour = "gray"), 
         panel.grid.minor = element_line(size = 0.25, linetype = 'dashed',
                                         colour = "gray"),
         text = element_text(size=20)
       )
ggsave(filename=paste(dir,"Thane.jpeg",sep=""),device="jpeg",scale=2.5)
  

#Calculating the Time When the Pandemic Ends
if(m>=0)
{
  print("Pandemic is Rising")
} else
{
  T<-(f-c)/m
  print(ifelse((T-l)>0,paste("Pandemic Ends Around ",round(T-l)," Days From Today.",sep=""),"Pandemic Has Ended."))
}