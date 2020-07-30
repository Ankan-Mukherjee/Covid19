#Assumptions are 
#1. Levitt Model is obeyed.
#2. April 25, 2020 is considered as Day 0.
#3. Case numbers follow the current trend.

#Source of data: "https://www.covid19india.org/state/MH"

#Collecting the Data
filename<-"C:/Users/Ankan Mukherjee/Documents/Covid 19/Data/covthane.csv"
data<-read.csv(filename)
data<-na.omit(data)

#Presetting Some Variables
l<-data$t[length(data$t)]
f<-1.001

#Segregating the Data
y<-data$H
x<-data$t

#Linear Modeling
lmResult <- summary(lm(y~x))
c<-coef(lmResult)[1]
m<-coef(lmResult)[2]

#Equation of the Best Fit Line
Y<-function(X)
{
  m*X+c
}

#Plotting the Data
plot(y~x,xlim=c(20,l+5),ylim=c(1.00,1.10),main="Covid Cases in Thane",xlab="Days since first case(t)",ylab="H(t)=X(t)/X(t-1)",col=rgb(0,0,255,maxColorValue=255),pch=20)
abline(c,m,col=rgb(255,0,0,maxColorValue=255))
points(Y(x)~x,xlim=c(20,l+5),ylim=c(1.00,1.10),col=rgb(255,0,0,maxColorValue=255),pch=4)
grid(nx=20,ny=20)

#Calculating the Time When the Pandemic Ends
T<-(f-c)/m
print(paste("Pandemic ends around ",round(T-l)," days from today.",sep=""))

