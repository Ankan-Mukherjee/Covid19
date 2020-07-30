#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(tidyverse)
library(gridExtra)
library(grid)
library(shiny)


function(input,output,session){

  observe({
    state1<-input$state1
    state2<-input$state2
    districts1<-c("Whole State",unique(main%>%filter(State==state1)%>%.$District))
    districts2<-c("Whole State",unique(main%>%filter(State==state2)%>%.$District))
    updateSelectInput(session,"district1",choices=districts1)
    updateSelectInput(session,"district2",choices=districts2)
  })
  
  output$Cumul<-renderPlot({
  
  #Making Calculations on the Data
  state2<-input$state2
  N<-input$order2
  data<-main%>%filter(State==state2)
  for(date in unique(data$Date)){
      new_data<-data%>%filter(data$Date==date)
      data<-data%>%add_row(Date=unique(new_data$Date),State=state2,District="Whole State",Confirmed=sum(new_data$Confirmed),Deceased=sum(new_data$Deceased),Recovered=sum(new_data$Recovered),Other=sum(new_data$Other),Tested=sum(new_data$Tested))
    }
  district2<-input$district2
  #lowlim_calc<-input$lowlim_calc
  data<-data%>%filter(District==district2)
  start<-data$Date[1]
  data<-data%>%mutate(t=as.integer(Date-start))
  data<-data[,c("Date","t","Confirmed","Deceased","Recovered")]
  data<-data[1:length(data$t)-1,]
  data<-na.omit(data)
  #data
  
  #Presetting Some Variables
  l<-data$t[length(data$t)]
  
  #Segregating the Data
  y_c<-data$Confirmed[1:l]
  y_d<-data$Deceased[1:l]
  y_r<-data$Recovered[1:l]
  x<-data$t[1:l]
  
  d<-data.frame(x,y_c,y_d,y_r)
  
  #Plotting the Data
  p<-d%>%ggplot()
  pc<-p+
    geom_point(aes(x,y_c),color="#FC4E07",shape=18,size=2)+
    stat_smooth(aes(x,y_c),method="lm",formula=y~poly(x,N,raw=TRUE),color=rgb(0,0,255,maxColorValue=255))+
    geom_point(aes(x,Z(x,y_c,N)),color=rgb(0,0,255,maxColorValue=255),shape=4)+
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
    geom_point(aes(x,Z(x,y_d,N)),color=rgb(0,0,255,maxColorValue=255),shape=4)+
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
               top=textGrob(paste("Covid Cases in ",district2,sep=""),gp=gpar(fontsize=20,fontface="bold")),
               bottom=textGrob(paste("First Case On",start),gp=gpar(fontsize=10,fontface="bold",col="blue"))
    )
  
  }, height=500, bg="transparent")
  
  
  
  output$LevMet<-renderPlot({
    
    #Making Calculations on the Data
    state1<-input$state1
    data<-main%>%filter(State==state1)
    for(date in unique(data$Date)){
      new_data<-data%>%filter(data$Date==date)
      data<-data%>%add_row(Date=unique(new_data$Date),State=state1,District="Whole State",Confirmed=sum(new_data$Confirmed),Deceased=sum(new_data$Deceased),Recovered=sum(new_data$Recovered),Other=sum(new_data$Other),Tested=sum(new_data$Tested))
    }
    district1<-input$district1
    lowlim_calc1<-input$lowlim_calc1
    data<-data%>%filter(District==district1)
    start<-data$Date[1]
    data<-data%>%mutate(t=as.integer(Date-start),Confirmed_prev=shift(Confirmed,1),Deceased_prev=shift(Deceased,1))%>%
      mutate(H_confirmed=Confirmed/Confirmed_prev,H_deceased=Deceased/Deceased_prev)
    data<-data[,c("Date","t","Confirmed","Confirmed_prev","H_confirmed","Deceased","Deceased_prev","H_deceased")]
    data<-data[1:length(data$t)-1,]
    data<-na.omit(data)
    #data
    
    #Presetting Some Variables
    l<-data$t[length(data$t)]
    
    #Segregating the Data
    y<-data$H_confirmed[lowlim_calc1:l]
    x<-data$t[lowlim_calc1:l]
    
    #Linear Modeling
    lmResult <- summary(lm(y~x))
    c<-coef(lmResult)[1]
    m<-coef(lmResult)[2]
    
    #Plotting the Data
    d<-data.frame(x,y)
    p<-d%>%ggplot()
    p+
      geom_point(aes(x,y),color=rgb(0,0,255,maxColorValue=255),shape=20,size=4)+
      geom_abline(slope=m,intercept=c,color=rgb(255,0,0,maxColorValue=255))+
      geom_point(aes(x,Y(x,m,c)),color=rgb(255,0,0,maxColorValue=255),shape=4,size=3)+
      xlim(lowlim_rep,l+5)+
      ylim(1.00,1.10)+
      xlab("Days since first case(t)")+
      ylab("H(t)=X(t)/X(t-1)")+
      labs(title=paste("Covid Cases in ",ifelse(district1!="Whole State",district1,state1),sep=""),
           caption=if(m>=0)
           {
             "Pandemic is Rising"
           } else
           {
             T<-(f-c)/m
             ifelse((T-l)>0,paste("Pandemic Ends Around ",round(T-l)," Days From Today.",sep=""),"Pandemic Has Ended.")
           })+ 
      theme(
        plot.title = element_text(color = rgb(3,60,115,maxColorValue=255), size = 40, face = "bold", hjust=0.5),
        plot.caption = element_text(color = rgb(3,60,115,maxColorValue=255), hjust=0.5, size=25),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "#DFFFDF",
                                        colour = "black",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "gray"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'dashed',
                                        colour = "gray"),
        axis.text=element_text(size=15),
        axis.title=element_text(size=20)
      )
  }, height=500, bg="transparent")
  
}

