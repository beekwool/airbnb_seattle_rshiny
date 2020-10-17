library(shiny)
library(shinydashboard)
library(googleVis)
#install.packages(plotly)
library(plotly)
library(DT)

# state_stat=data.frame(state.name=rownames(state.x77), state.x77)
# rownames(state_stat)=NULL
# 
# choice <- colnames(state_stat)[-1]


#monthly total unique listings

#Listing vacancy by day
listing_by_day = arbnb.raw %>% 
  mutate(year = year(date), 
         month = month(date),
         day_of_year = yday(date),
         price = as.numeric(price)) %>% 
  filter(!is.na(price), year == 2020) %>% 
  group_by(day_of_year, 
           available) %>% 
  summarise(list_cnt = n()) %>% 
  filter(available == TRUE)


ggplot(listing_by_day, aes(day_of_year, list_cnt)) +
  geom_freqpoly(stat='identity') +
  #  coord_cartesian(xlim=c(1,12)) +
  labs(title='Count of Vacant Listing by day in Seattle', x='Day', y='Listings') +
  #scale_x_continuous(breaks = 1:12) +
  theme_bw() 

choice =  colnames(arbnb_list)



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
      ),
      selectizeInput('selected','Select Item to Display',
                     choices=choice)
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
              fluidRow( plotlyOutput("vacant"))),

        tabItem(tabName = 'data',
              fluidRow(box(dataTableOutput('table'),width=12)))
        

        
      )
    )
  )
  
)




# server ####
server=function(input,output){
  
  output$hist=renderGvis({
    gvisHistogram(state_stat[,input$selected,drop=FALSE])
  })
  
  # output$map <- renderGvis(
  #   {gvisGeoChart(state_stat, "state.name", input$selected,
  #                 options=list(region="US", displayMode="regions", 
  #                              resolution="provinces",
  #                              width="auto", height="auto"))}
  # )
  # 
  # output$table=renderDataTable(
  #   datatable(arbnb_list) %>% 
  #     formatStyle(input$selected,
  #                 background="skyblue", 
  #                 fontWeight='bold')
  # )
  # 
  output$vacant=renderPlotly(
    ggplotly(
      {ggplot(listing_by_day, aes(day_of_year, list_cnt)) +
          geom_freqpoly(stat='identity') +
          #  coord_cartesian(xlim=c(1,12)) +
          labs(title='Count of Vacant Listing by day in Seattle', x='Day', y='Listings') +
          #scale_x_continuous(breaks = 1:12) +
          theme_bw()},
      tooltip='text'
    )
  )
  
  
  output$max=renderInfoBox({
    max_value <- max(state_stat[,input$selected])
    max_state <- 
      state_stat$state.name[state_stat[,input$selected]==max_value]
    infoBox(max_state,max_value,icon=icon('hand-o-up'))
  })
  
  output$minBox <- renderInfoBox({
    min_value <- min(state_stat[,input$selected])
    min_state <- 
      state_stat$state.name[state_stat[,input$selected]==min_value]
    infoBox(min_state, min_value, icon = icon("hand-o-down"))
  })
  
  output$avgBox <- renderInfoBox(
    infoBox(paste("AVG.", input$selected),
            mean(state_stat[,input$selected]), 
            icon = icon("calculator"), fill = TRUE))
  
}

shinyApp(ui,server)

