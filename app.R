#library ####
library(shiny)
library(data.table)
library(lubridate)
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
# library(plotly)

# state_stat=data.frame(state.name=rownames(state.x77), state.x77)
# rownames(state_stat)=NULL
# 
# choice <- colnames(state_stat)[-1]


#monthly total unique listings

#data import####
listing_detail_profit = read_csv('ListingDetailProfit.csv')


# ui ####
ui=fluidPage(
 
  dashboardPage(
    dashboardHeader(title='Dashboard'),
    dashboardSidebar(
      sidebarUserPanel('Arbnb in Seattle',
                       image = 'https://pbs.twimg.com/profile_images/1272918797410762752/ZeHt0zMP.jpg'),
      sidebarMenu(
        # menuItem('Map',tabName = 'map',icon=icon('map')),
          menuItem('Introduction',tabName = 'introduction',icon=icon('hand-paper')),
          menuItem('Obervation',tabName = 'obv',icon=icon('chart-bar')),
          menuItem('Exploritory Visual',tabName = 'explore',icon=icon('globe-americas')),  
          menuItem('Data',tabName = 'data',icon=icon('database'))  
      ) 
    
    ),
    dashboardBody(
      tabItems(
        # tabItem(tabName = 'map',
        #         fluidRow(
        #           infoBoxOutput('max'),
        #           infoBoxOutput('minBox'),
        #           infoBoxOutput('avgBox')
        #         ),
        #         fluidRow(
        #           box(htmlOutput('hist'),height = 300),
        #           box(htmlOutput('map'),height = 300)
        #         )
        # ),


        tabItem(tabName = 'introduction',
          fluidRow('tbd')),
        
        tabItem(tabName = 'obv',
                fluidRow( plotlyOutput("vacant"))),
        
        tabItem(tabName = 'explore',
              fluidRow(
                        fluidRow(
                          infoBoxOutput('max')
                          # infoBoxOutput('minBox'),
                          # infoBoxOutput('avgBox')
                        ),
                #         fluidRow(
                #           box(htmlOutput('hist'),height = 300),
                #           box(htmlOutput('map'),height = 300)
                #         )
                #titlePanel("NYC Flights 2014"),
                sidebarLayout(
                  sidebarPanel = sidebarPanel(

                    selectizeInput(inputId = "neighborhood",
                                   label = "Neighborhood",
                                   choices = unique(listing_detail_profit$neighbourhood_group_cleansed)),
                    
                    # verbatimTextOutput("value")
                    
                    selectizeInput(inputId = "prop_type",
                                   label = "Property Type",
                                   choices = NULL)
                    
                  ),
                  mainPanel = mainPanel(
                    plotOutput("neighborhoodProf")
                  )
                  
              )
            )
        ),

        tabItem(tabName = 'data',
              'tbd data table'
                # fluidRow(box(dataTableOutput('table'),width=12))
              
              )
        

        
      )
    )
  )
  
)


# Server ####
server=function(input, output, session){
  
  
  observeEvent(input$neighborhood, {
    choices = unique(listing_detail_profit$property_type[listing_detail_profit$neighbourhood_group_cleansed == input$neighborhood])
    updateSelectizeInput(session, inputId = "prop_type", choices = choices)
  })
  
  output$neighborhoodProf=renderPlot(
      {listing_detail_profit %>% 
          filter(neighbourhood_group_cleansed == input$neighborhood) %>% 
          group_by(neighbourhood_group_cleansed) %>% 
          summarise(avg_profit = mean(profit)) %>% 
          ggplot(aes(reorder(neighbourhood_group_cleansed, avg_profit), avg_profit)) + 
          geom_bar(stat = 'identity') +
          labs(title='Neighborhood Profitability', x='Neighborhood', y='Average Profit') +
          coord_flip() +
          theme_bw()} 
  )

  
  
  output$max=renderInfoBox({
    max_value = max(listing_detail_profit$accommodates[listing_detail_profit$neighbourhood_group_cleansed == input$neighborhood])
    # max_state <- 
    #   state_stat$state.name[state_stat[,input$selected]==max_value]
    # infoBox(max_state,max_value,icon=icon('hand-o-up'))
    infoBox("Max Accomodation", max_value, icon=icon('hand-o-up'))
  })
  
  output$minBox <- renderInfoBox({
    min_value <- min(state_stat[,input$neighborhood])
    # min_state <- 
    #   state_stat$state.name[state_stat[,input$selected]==min_value]
    infoBox(min_state, min_value, icon = icon("hand-o-down"))
  })
  
  output$avgBox <- renderInfoBox(
    infoBox(paste("AVG.", input$selected),
            mean(state_stat[,input$selected]), 
            icon = icon("calculator"), fill = TRUE))
  
}

shinyApp(ui,server)

