
library(dplyr)
library(ggplot2)
library(ggthemes)
library(zoo)
library(shinydashboard)
library(leaflet)
library(lubridate)
library(dygraphs)
library(shiny)
library(xts)



shinyUI(dashboardPage(
  skin='green',
  dashboardHeader(
    title=p('Bike Share')
  ),
 
  

  dashboardSidebar(
    
    sidebarMenu(
      menuItem('Start',tabName = 'Start',icon = icon('comment-o')),
      menuItem('Customer',tabName='Customer',icon=icon('user-circle')),
      menuItem('Map',tabName='Map',icon=icon('map')),
      menuItem('Service',tabName='Service',icon=icon('home')),
      menuItem('Time',tabName='Time',icon=icon('bicycle'))
    )
  ),

    dashboardBody(
      tabItems(
        tabItem(
          tabName='Start',
          h2('Bike Share Customer Behavior Analysis'),
          fluidRow(
            box(
              img(src='http://www.albany.com/images/bike-share-800.jpg',width='100%'),
              width=6,
              align='center'
            ),
            box(
              img(src='http://bikeshare.com/wp-content/uploads/2014/03/GREENbikes-for-WFRC.jpg',width='100%'),
              width=6,
              align='center'
            )
          ),
          fluidRow(
            h1('Data'), 
            p('1.Bike Sharing Demand-Bike Sharing System(1/1/2011-12/31/2012)'),
            p('2.Bike System Data-City Bike (3/1/2017-3/31/2017')),
            width=12
          ),
                
        
        
        tabItem(tabName='Customer',
                fluidRow(
                  valueBoxOutput('Avgage',width=4),
                  valueBoxOutput('Gender',width=4),
                  valueBoxOutput('User',width=4)
                ),
                tabsetPanel(
                  tabPanel(
                    'Overview',
                    fluidRow(
                      box(
                          plotOutput('c1',height=200)),
                      box(
                          plotOutput('c2',height=200))
                  
                  
                  #box(plotOutput("bs1",height=250)),
                 # box(title='first image',
                  #    sliderInput('bins','Number of bins:',
                  #                min=1,max=50,value=30))
                
                  
                  ),
                fluidRow(
                  box(
                      plotOutput('c3',height=250)),
                  box(
                      plotOutput('c4',height=250))
                  
                 #box(plotOutput('ct1',height=250))
                  
                  
                )),
                tabPanel(
                  'Type',
                  fluidRow(
                    box(plotOutput('c5'),width=12)),
                  fluidRow(box(plotOutput('c6'),width=12))
                  )
                )
                ),
        
        
        tabItem(tabName='Map',
                
                
                tabsetPanel(
                  tabPanel(
                    'Station',
                    fluidRow(
                      box(
                    leafletOutput(
                      'map1',width='100%',
                      height=600
                    ),
                    width=12
                  )
                )
        ),
        tabPanel(
          'Top 20 Station',
          fluidRow(
            box(
              leafletOutput(
                'map2',width='100%',
                height=600
            ),
            width=12
          )
        )
        ),
        tabPanel(
          'Top 20 Route',
          fluidRow(
            box(
              leafletOutput(
                'map3',width='100%',
                height=600
              ),
              width=12
            )
          )
        )
        )
        ),
        tabItem(tabName='Service',
                fluidRow(
                  valueBoxOutput('Year',width=4),
                  valueBoxOutput('Week',width=4),
                  valueBoxOutput('Day',width=4)
                ),  
                fluidRow(
                  box(dygraphOutput('s1'),width=12)
                )
        ),
        
        tabItem(tabName='Time',
                tabsetPanel(
                  tabPanel(
                  'Season',
                  fluidRow(
                    box(
                        plotOutput('t1')),
                    box(plotOutput('t2'))    
                  )
                ),
                tabPanel(
                  'Week',
                  fluidRow(
                    box(plotOutput('t3'),width=12)),
                  fluidRow(box(plotOutput('t4'),width=12))
                  )
                )
                )
       
        )

)
)
)


