
library(dplyr)
library(ggplot2)
library(ggthemes)
library(zoo)
library(shiny)
library(shinydashboard)
library(leaflet)
library(lubridate)
library(xts)

shinyServer(function(input, output) {
  bikeshare1<-read.csv('train.csv')
  bikeshare2<-read.csv('test.csv')
  bikeshare2[,10:12]<-NA
  colnames(bikeshare2)[10:12]=c('casual','registered','count')
  bikeshare=rbind(bikeshare1,bikeshare2)
  
  cititripJC=read.csv("JC-201703-citibike-tripdata.csv")
  cititripNY=read.csv('201703-citibike-tripdata.csv')
  colnames(cititripNY)=colnames(cititripJC)
  cititrip=rbind(cititripNY,cititripJC)
  cititrip1<-cititrip
  colnames(cititrip1)[4:7]=c('SID','Sname','Sla','Slo')
  colnames(cititrip1)[8:11]=c('EID','Ename','Ela','Elo')
  cititrip1<-cititrip1[cititrip1$Sla!=0,]
  cititrip1<-cititrip1[cititrip1$Ela!=0,]

  membership=read.csv("membership1.csv")
  colnames(membership)[1:5]=c('Date','dailytrip','totaltrip','dailymiles','totalmiles')
  colnames(membership)[7:9]=c('yearpurchase','daypurchase','weekpurchase')
  membership$totalmiles=as.integer(membership$totalmiles)
  membership2=read.csv('membership2.csv')
  membership2$other<-NA
  colnames(membership2)<-colnames(membership)
  membership2$weekpurchase=as.integer(membership2$weekpurchase)
  membership2$totalmiles=as.integer(membership2$totalmiles)
  membership=rbind(membership2,membership)

  
  for (i in c(3:6)){
    a=read.csv(paste0('membership',i,'.csv'))
    colnames(a)<-colnames(membership)
    a$yearpurchase=as.integer(a$yearpurchase)
    a$daypurchase=as.integer(a$daypurchase)
    a$weekpurchase=as.integer(a$weekpurchase)
    a$totalmiles=as.integer(a$totalmiles)
    membership=rbind(a,membership)
  } 
  
  membership2=read.csv('membership7.csv')
  membership2$insert<-0
  membership2<-membership2[,c(1,2,3,4,5,6,9,7,8)]
  colnames(membership2)=colnames(membership)
  membership2$yearpurchase=as.integer(membership2$yearpurchase)
  
  for (i in c(8:14)){
    a=read.csv(paste0('membership',i,'.csv'))
    a$insert<-0
    a<-a[,c(1,2,3,4,5,6,9,7,8)]
    colnames(a)<-colnames(membership2)
    a$yearpurchase=as.integer(a$yearpurchase)
    a$totaltrip=as.integer(a$totaltrip)
    a$totalmiles=as.integer(a$totalmiles)
    a$weekpurchase=as.integer(a$weekpurchase)
    membership2=rbind(a,membership2)
  }  
  membership$dailymiles=as.integer(membership$dailymiles)
  membership$totalmiles=as.integer(membership$totalmiles)
  
  membership=rbind(membership,membership2)
  membership$Date<-as.Date(membership$Date,format='%m/%d/%Y')
  
  membership<-membership[order(membership$Date),]
  membershiptest<-membership
  membershiptest<-membershiptest[year(membershiptest$Date)!=16,]
  
  membership0016<-membership %>% 
    filter(year(membership$Date)==16) %>%
    mutate(Date=as.Date(paste0('2016','-',month(Date),'-',day(Date))))
  
  membership<-rbind(membershiptest,membership0016)
  

  for (i in 2:1315){
    membership$yearpurchase[i]=membership$Total.Annual.Members[i]-membership$Total.Annual.Members[i-1]
  }

  

#  output$bs1 <- renderPlot({
#    BShiny1<-bikeshare %>%
#      filter(!is.na(count)) %>%
#      mutate(weekday=weekdays(as.Date(datetime))) %>%
#      select(weekday,count) %>%
 #     group_by(weekday) %>%
#      summarise(total_rental=sum(count),avg_rental=mean(count),total_record=n())
#    
#    BShiny1$weekday<-factor(bs1$weekday,levels=c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'))
#    
#    ggplot(BShiny1,aes(x=weekday,y=avg_rental,fill=weekday)) +
#      geom_bar(stat='identity')

    
#  })
  
  output$Avgage<-renderInfoBox({
    ctt1<- cititrip %>%
      filter(!is.na(Birth.Year)) %>%
      select(Start.Time, Trip.Duration, Birth.Year) %>%
      mutate(Year=year(Start.Time)) %>%
      mutate(Age=Year-Birth.Year)
   valueBox(
     as.integer(mean(ctt1$Age)),'Average Age',
     icon=icon('address-book-o'),color = "green"
   ) 
    
  })
  output$Gender<-renderInfoBox({
    ctt3<-cititrip %>%
      filter(!is.na(Gender) & Gender!=0) %>%
      select(Gender) %>%
      mutate(Gender=factor(Gender)) %>%
      group_by(Gender) %>%
      summarise(total=n())%>%
      mutate(percentage=100*total/sum(total))
    valueBox(
      paste0(as.integer(ctt3$percentage[1]),'%'),'Male',icon = icon("list"),
      color = "purple")
  })
  
  output$User<-renderInfoBox({
    bss3<-
      data.frame(user_type=c('casual','registered'), 
                 total=c(sum(bikeshare$casual,na.rm=T),
                         sum(bikeshare$registered, na.rm=T))) %>%
      mutate(percentage=100*total/sum(total))
    valueBox(
      paste0(as.integer(bss3$percentage[2]),'%'),'Registered',icon=icon('credit-card'),
      color='red'
    )
    
  })
  
  output$c1<-renderPlot({
    ct2<- cititrip %>%
      filter(!is.na(Birth.Year)) %>%
      select(Start.Time, Trip.Duration, Birth.Year) %>%
      mutate(Year=year(Start.Time)) %>%
      mutate(Age=Year-Birth.Year) %>%
      select(Age,Trip.Duration) 
    
    ct21<- ct2 %>%
      group_by(Age) %>%
      summarise(total=n())
    
    ggplot(ct21,aes(x=Age,y=total,fill=Age))+
      geom_bar(stat='identity',fill="#009E73")+
      ggtitle('Total Trip Duration of Different Age')+
      theme_bw()+
      scale_x_continuous(breaks = scales::pretty_breaks(n = 15),
                         limits=c(10,80))
  })
  output$c2<-renderPlot({
    ct3<-cititrip %>%
      filter(!is.na(Gender) & Gender!=0) %>%
      select(Gender) %>%
      mutate(Gender=factor(Gender)) %>%
      group_by(Gender) %>%
      summarise(total=n())%>%
      mutate(percentage=100*total/sum(total))
    
    ggplot(ct3,aes(x='',y=percentage,fill=Gender))+
      geom_bar(width=1,stat='identity')+
      coord_polar('y',start=0) + scale_fill_brewer(labels=c('Male','Female'),palette = "Greens")+
      ggtitle('Gender')+
      theme_bw()+
      geom_text(aes(label=paste0(as.integer(percentage),'%')))
  })
  output$c3<-renderPlot({
    bs3<-
      data.frame(user_type=c('casual','registered'), 
                 total=c(sum(bikeshare$casual,na.rm=T),
                         sum(bikeshare$registered, na.rm=T))) %>%
      mutate(percentage=100*total/sum(total))
    
    ggplot(bs3,aes(x='',y=percentage,fill=user_type)) +
      geom_bar(width=1,stat='identity')+
      coord_polar('y',start=0) + scale_fill_brewer(name='User Type',palette = "Greens")+
      ggtitle('User Type')+
      theme_bw()+
      geom_text(aes(label=paste0(as.integer(percentage),'%')))
  })
  output$c4<-renderPlot({
    mbdate2<-membership %>%
      filter(!is.na(Date) & !is.na(Total.Annual.Members)) %>%
      mutate(month=month(Date),day=day(Date)) %>%
      filter(month %in% c(6,12) & day==30) %>%
      select(1,6)
    mbdate2<-mbdate2[2:8,]
    rownames(mbdate2)<-c(1:7)
  
    
    mbdate<-membership %>%
      filter(!is.na(Date) & !is.na(Total.Annual.Members)) %>%
      mutate(month=month(Date),day=day(Date),year=year(Date)) %>%
      filter(month %in% c(1,7) & day==1 & year!=2013) %>%
      select(1,6)
    
    mbdate1<-rbind(mbdate,membership[membership$Date=='2013-5-27',c(1,6)])
    
    
    mbdate1<-mbdate1[order(mbdate1$Date),]
    rownames(mbdate1)<-c(1:7)
    
    mbdate3<-cbind(mbdate1,mbdate2)
    colnames(mbdate3)<-c('begin','beginnum','end','endnum')
    mbdate3<- mbdate3 %>%
      mutate(Newcustomer=endnum-beginnum,yearmon=as.character(as.yearmon(end))) %>%
      select(yearmon,Newcustomer)
    
    mbdate3<-mbdate3[order(mbdate3$yearmon),]
    ggplot(mbdate3,aes(x=yearmon,y=Newcustomer,fill=yearmon)) +
      geom_bar(stat='identity')+
      scale_fill_brewer(name='Month Year',palette = "Greens")+
      ggtitle('New Customer Amount')+xlab("")+
      ylab('New Customer Number')+
      theme_bw()+
      geom_text(aes(label =Newcustomer))
  })
  
  
  output$c5<-renderPlot({
    bs4<-bikeshare %>%
      filter(!is.na(casual) & !is.na(registered) & !is.na(count)) %>%
      mutate(year_month=as.yearmon(datetime)) %>%
      select(year_month,casual,registered,count) 
    
    bs4$year_month=factor(bs4$year_mont) 
    
    bs4<-bs4 %>%
      group_by(year_month) %>%
      summarise(tcasual=sum(casual),tregistered=sum(registered),
                total=sum(count))
    
    
    ggplot(bs4,aes(x=year_month,group=1))+
      geom_line(aes(y=tcasual,colour='casual')) +
      geom_line(aes(y=tregistered,colour='registered')) +
      geom_line(aes(y=total,colour='total'))+
      ggtitle('Users Rentals Performance')+xlab("")+
      ylab('Rental Amount')+
      theme_bw()+
      theme(axis.text.x = element_text(angle=45))+
      scale_y_continuous(labels = function(x) format(x, scientific = F))
  })


  output$c6<-renderPlot({
    ct12<-cititrip %>%
      filter(!is.na(User.Type) & User.Type!='' & !is.na(Start.Time)) %>%
      select(Start.Time,User.Type) 
    
    ct12$Start.Time=as.Date(ct12$Start.Time)
    ct12<- ct12 %>%
      group_by(Start.Time,User.Type) %>%
      summarise(total=n())
    
    ggplot(ct12,aes(x=Start.Time,y=total,group=User.Type,colour=User.Type)) +
      geom_line()+
      ggtitle('Customer Performance')+xlab("")+
      ylab('Amount')+
      theme_bw()+
      theme(axis.text.x = element_text(angle=45))
      
    
  
  })
  
 # output$ct1 <-renderPlot({
#    ct3<-cititrip %>%
 #     filter(!is.na(Gender) & Gender!=0) %>%
  #    select(Gender) %>%
   #   mutate(Gender=factor(Gender)) %>%
    #  group_by(Gender) %>%
     # summarise(total=n())%>%
     # mutate(percentage=100*total/sum(total))
    
  #  ggplot(ct3,aes(x='',y=percentage,fill=Gender))+
   #   geom_bar(width=1,stat='identity')+
    #  coord_polar('y',start=0)+scale_fill_brewer()
  #})
  output$map1<-renderLeaflet({
   
    citiuni1<-cititrip1 %>% 
      select(SID,Sla,Slo) %>%
      distinct
    
    citiuni2<-cititrip1 %>%
      select(EID,Ela,Elo) %>%
      distinct
    
    colnames(citiuni1)<-colnames(citiuni2)
    citiuni<-rbind(citiuni1,citiuni2)
    
    citiuni<- citiuni %>% distinct
    
    
    leaflet(citiuni) %>%
      addTiles() %>% 
      addCircleMarkers(~Elo,~Ela,
                       radius=6,color='green',stroke = FALSE, fillOpacity = 0.3)
    
  })
  output$map2<-renderLeaflet({
    citistart<-cititrip1 %>% select(Sname,SID,Sla,Slo)
    colnames(citistart)<-c('Name','ID','Lat','Long')
    citiend<-cititrip1 %>% select(Ename,EID,Ela,Elo)
    colnames(citiend)<-colnames(citistart)
    citimap22<-rbind(citistart,citiend)
    citimap3<-citimap22 %>%
      group_by(ID,Lat,Long,Name) %>%
      summarise(total=n())
    citimap3<-citimap3[order(citimap3$total,decreasing=TRUE),]
    citimap3<-head(citimap3,20)
    
    leaflet(citimap3) %>%
      addTiles() %>%
      addMarkers(data=citimap3,~Long,~Lat,label=citimap3$Name)
  })
  output$map3<-renderLeaflet({

    
     citimap2<-cititrip1 %>% 
      mutate(route=paste0(Sname,'-',Ename)) %>%
      filter(SID!=EID) %>%
      select(SID,EID,route,Sla,Slo,
             Ela,Elo) %>%
      group_by(route,SID,EID,Sla,Slo,
               Ela,Elo) %>%
      summarise(total=n()) 
    
    citimap2<-(citimap2[order(citimap2$total,decreasing=TRUE),])
    citimap2<-head(citimap2,20)
    
    topocolor=topo.colors(20)
    routes<-
      leaflet(citimap2) %>%
      addProviderTiles("Esri.WorldTopoMap")  %>% 
      addPolylines(lng=c(citimap2$Slo[1],citimap2$Elo[1]),
                   lat=c(citimap2$Sla[1],citimap2$Ela[1]),
                   color=topocolor[1],
                   opacity = 1, label=citimap2$route)
    
    for (i in 2:20){
      routes<-routes %>%
        addPolylines(lng=c(citimap2$Slo[i],citimap2$Elo[i]),
                     lat=c(citimap2$Sla[i],citimap2$Ela[i]),
                     color=topocolor[i],
                     opacity = 1,label=citimap2$route)
    }
    routes
  })
  
  output$Year<-renderInfoBox({
    
    valueBox(
      sum(membership$yearpurchase,na.rm=T),'Total Annual Member',
      icon=icon('address-book-o'),color='teal'
    ) 
    
  })
  output$Week<-renderInfoBox({
    
    valueBox(
      sum(membership$weekpurchase,na.rm=T),'Total Week Purchase',icon = icon("list"),
      color = "purple")
  })
  
  output$Day<-renderInfoBox({
    
    valueBox(
      sum(membership$daypurchase,na.rm=T),'Total Day Purchase',icon=icon('credit-card'),
      color='red'
    )
    
  })
  
  output$s1<-renderDygraph({
    s1<-membership %>% select(Date,yearpurchase,daypurchase,weekpurchase) 
    zoo1<-as.xts(s1,order.by=s1$Date)
    dygraph(zoo1) %>%
      dyRangeSelector() 
  })
  
  output$t1<-renderPlot({
    bs2<-bikeshare %>%
      filter(!is.na(count)) %>%
      group_by(season) %>%
      summarise(total_rental=sum(count),avg_rental=mean(count),total_record=n())
    
    bs2$season<-factor(bs2$season,levels=c(1,2,3,4))
    levels(bs2$season)<-c('Spring','Summer','Fall','Winter')
    
    ggplot(bs2,aes(x=season,y=avg_rental,fill=season)) +
      geom_bar(stat='identity')+scale_fill_brewer(palette = "Greens") +
      ggtitle('New Customer Amount')+xlab("")+
      ylab('Average Daily Rental of Season')+
      theme_bw()+
      geom_text(aes(label =as.integer(avg_rental)))
  })
  output$t2<-renderPlot({
    bs5<-bikeshare %>%
      filter(!is.na(count)) %>%
      group_by(season,weather) %>%
      summarise(total_rental=sum(count))
    
    bs5$season<-factor(bs5$season,levels=c(1,2,3,4))
    levels(bs5$season)<-c('Spring','Summer','Fall','Winter')
    bs5$weather<-factor(bs5$weather,levels=c(1,2,3,4))
    levels(bs5$weather)<-c('Clear','Mist','Light Snow or Rain','Heavy')
    
    ggplot(bs5,aes(x=weather,y=total_rental,group=season,colour=season)) +
      geom_line()+
      ggtitle('Total Rental of Season/Weather')+
      ylab('Total Rental Amount')+
      theme_bw()
      
    
  })
output$t3<-renderPlot({
  bs1<-bikeshare %>%
    filter(!is.na(count)) %>%
    mutate(weekday=weekdays(as.Date(datetime))) %>%
    select(weekday,count) %>%
    group_by(weekday) %>%
    summarise(total_rental=sum(count),avg_rental=mean(count),total_record=n())
  
  bs1$weekday<-factor(bs1$weekday,levels=c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'))
  
  ggplot(bs1,aes(x=weekday,y=avg_rental,fill=weekday)) +
    geom_bar(stat='identity')+scale_fill_brewer(palette = "Greens")+
    ggtitle('Weekly Performance')+xlab("")+
    ylab('Average Daily Rental of Weekday')+
    theme_bw()+
    geom_text(aes(label =as.integer(avg_rental)))
})

output$t4<-renderPlot({
  bs6<-bikeshare %>%
    filter(!is.na(count)) %>%
    mutate(daytime=format(as.POSIXct(datetime,
                                     format='%Y-%m-%d %H:%M:%S'),
                          format='%H:%M')) %>%
    select(daytime,count)
  
  bs6$daytime<-factor(bs6$daytime)
  
  bs6<-bs6 %>%
    group_by(daytime) %>%
    summarise(total=sum(count),avg=mean(count))
  
  ggplot(bs6,aes(x=daytime, y=avg, group=1)) +
    geom_line()+
    ggtitle('Daytime Performance')+xlab("")+
    ylab('Average Rental of Daytime')+
    theme_bw()+
    geom_text(aes(label =as.integer(avg)))
})


})
  
  
  
  
