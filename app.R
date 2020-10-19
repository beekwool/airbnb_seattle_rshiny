#library ####
library(shiny)
library(data.table)
library(lubridate)
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(scales)

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
                       image = 'https://img.icons8.com/bubbles/50/000000/city.png'),
      sidebarMenu(
          menuItem('Introduction',tabName = 'introduction',icon=icon('hand-paper')),
          menuItem('Obervation',tabName = 'obv',icon=icon('chart-bar')),
          menuItem('Exploritory Visual',tabName = 'explore',icon=icon('globe-americas')),  
          menuItem('Data',tabName = 'data',icon=icon('database'))  
      ) 
    
    ),
    dashboardBody(
      tabItems(

#intro ####
        tabItem(tabName = 'introduction',
                fluidRow(
                  titlePanel("Where does the Arbnb listing stand in Seattle?"),
                  sidebarLayout(
                    sidebarPanel(
                      img(src = "SEA.jpg", height = 300, width = 200)
                    ),
                    mainPanel(
                      h3("Project goal:"),
                      h4("This Shiny app aims to provide Arbnb hosts in Seattle a tool to discover how 
                         their listings compare to similar listings in Seattle."),
                      h3("Data in scope:"),
                      h4("Listings with a review at least within six months of data gathering"),
                      h3("Note on listing profit:"),
                      h4("1. Arbnb doesn not differenciate date booked by customers or date blocked out by hosts,
                         therefore the profit listed here would be an over estimation of actual gains by hosts."),
                      h4("2. Maximum occupency is capped at 70%, the cap is reached, 
                         profit is adjusted down by 30% as a rough adjustment to accomodate the overestimation")
                                  
                    )
                  )
                  
                  
                  
                  
                )),
#obv ####
        tabItem(tabName = 'obv',
                titlePanel("Listings vs. Profit"),
                
                fluidRow(
                  
                  
                  # h4("Selected bin size:"),
                  # column(4, verbatimTextOutput("value")),
                  
                  h4("There is a significant gap between the top quantile and bottom quantile"),
                  plotOutput("profdiff"),
                  
                  sliderInput("slider", label = h3("Profit Bin Size"), min = 0, 
                              max = 100, value = 4)
                  
                  
                )),
#explore ####
        tabItem(tabName = 'explore',
                titlePanel("Listings by Neighborhood and Property Type"),
                sidebarLayout(
               
                    
                    selectizeInput(inputId = "neighborhood",
                                   label = "Choose Listing Neighborhood",
                                   choices = unique(listing_detail_profit$neighbourhood_group_cleansed)),

                    selectizeInput(inputId = "prop_type",
                                   label = "Choose Listing Property Type",
                                   choices = NULL)
                    # plotOutput("neighborhoodProf")
                    
                  
                ),
                fluidRow(
                        fluidRow(
                          infoBoxOutput("avgProf")),
                          tags$style("#avgProf {width:1000px;}"),
                        
                        fluidRow(
                          infoBoxOutput('avgRes')),
                        tags$style("#avgRes {width:1000px;}"),
                        
                        fluidRow(
                          infoBoxOutput('supHo')),
                        tags$style("#supHo {width:1000px;}"),
                        
                        fluidRow(
                          infoBoxOutput('avgAcc')),
                        tags$style("#avgAcc {width:1000px;}"),
                        
                        fluidRow(
                          infoBoxOutput('avgBed')),
                        tags$style("#avgBed {width:1000px;}"),
                        
                        fluidRow(
                          infoBoxOutput('avgPrice')),
                        tags$style("#avgPrice {width:1000px;}"),
                        
                        fluidRow(
                          infoBoxOutput('avgScore')),
                        tags$style("#avgScore {width:1000px;}"),
                        
                #         fluidRow(
                #           box(htmlOutput('hist'),height = 300),
                #           box(htmlOutput('map'),height = 300)
                #         )
               
                
            )
        ),

# data ####
        tabItem(tabName = 'data',
                titlePanel("Dataset: Listing Details in Scope"),
                h3("This data set is attained by joining lising detail information and listing abailability information"),
                fluidRow(box(dataTableOutput('table'),width=12))

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
  
  #obv ####
  #Profit difference top vs lower
  output$value = renderPrint({ input$slider })
  
  
  output$profdiff=renderPlot(
    {listing_detail_profit %>% 
    mutate(profit_quartile = ntile(-profit, input$slider)) %>% 
    group_by(profit_quartile) %>% 
    summarise(average_profit = mean(profit)) %>% 
    ggplot(aes(profit_quartile, average_profit)) + 
    geom_bar(stat = 'identity') +
    labs(title='Average Profit in Listings', x='Quantile', y='Average Profit') +
    theme_bw() +
    geom_text(aes(label = round(average_profit, 0)), nudge_y= 4)
    } 
  )
  

    
  #explore info boxes ####  
  
  output$avgProf <- renderInfoBox({
    metrics = listing_detail_profit %>% 
      filter(property_type == input$prop_type, 
             neighbourhood_group_cleansed == input$neighborhood, 
             host_response_time != 'N/A') %>% 
      top_n(10, profit) %>% 
      summarise(avg_profit = mean(profit))

    infoBox(
      title = tags$b("Average Profit (Potential Max)"),
      value = tags$b(round(metrics$avg_profit, 0)), #tags$p(style = "font-size: 10px;", "my info box message")
      color = "orange",
      fill = TRUE,
      icon = icon("dollar-sign")
    )    
  })
  
  output$avgRes <- renderInfoBox({
    metrics = listing_detail_profit %>% 
      filter(property_type == input$prop_type, 
             neighbourhood_group_cleansed == input$neighborhood, 
             host_response_time != 'N/A') %>% 
      top_n(10, profit) %>% 
      summarise(avg_response_rate = mean(host_response_rate/100))
    
    infoBox(
      title = tags$b("Average Response Rate"),
      value = tags$b(percent(round(metrics$avg_response_rate, 2))), #tags$p(style = "font-size: 10px;", "my info box message")
      color = "aqua",
      fill = TRUE,
      icon = icon("edit")
    )    
  })
  
  output$supHo <- renderInfoBox({
    metrics = listing_detail_profit %>% 
      filter(property_type == input$prop_type, 
             neighbourhood_group_cleansed == input$neighborhood, 
             host_response_time != 'N/A') %>% 
      top_n(10, profit) %>% 
      summarise(super_host_pct = sum(host_is_superhost ==TRUE)/n())

    infoBox(
      title = tags$b("Super Host Percentage"),
      value = tags$b(percent(round(metrics$super_host_pct, 0))), 
      color = "aqua",
      fill = TRUE,
      icon = icon("superpowers")
    )    
  })
  
  output$avgAcc <- renderInfoBox({
    metrics = listing_detail_profit %>% 
      filter(property_type == input$prop_type, 
             neighbourhood_group_cleansed == input$neighborhood, 
             host_response_time != 'N/A') %>% 
      top_n(10, profit) %>% 
      summarise(avg_accomodation = mean(accommodates))
    
    infoBox(
      title = tags$b("Average Accomodation Number"),
      value = tags$b(round(metrics$avg_accomodation, 0)), 
      color = "aqua",
      fill = TRUE,
      icon = icon("user-friends")
    )    
  })
  
  output$avgBed <- renderInfoBox({
    metrics = listing_detail_profit %>% 
      filter(property_type == input$prop_type, 
             neighbourhood_group_cleansed == input$neighborhood, 
             host_response_time != 'N/A') %>% 
      top_n(10, profit) %>% 
      summarise(avg_bedrooms = mean(bedrooms))
    
    infoBox(
      title = tags$b("Average Bedrooms"),
      value = tags$b(round(metrics$avg_bedrooms, 0)), 
      color = "aqua",
      fill = TRUE,
      icon = icon("bed")
    )    
  })
  
  output$avgPrice <- renderInfoBox({
    metrics = listing_detail_profit %>% 
      filter(property_type == input$prop_type, 
             neighbourhood_group_cleansed == input$neighborhood, 
             host_response_time != 'N/A') %>% 
      top_n(10, profit) %>% 
      summarise(avg_price = mean(price))
    
    infoBox(
      title = tags$b("Average Listing Price"),
      value = tags$b(round(metrics$avg_price, 0)), 
      color = "aqua",
      fill = TRUE,
      icon = icon("money-check-alt")
    )    
  })
  
  output$avgScore <- renderInfoBox({
    metrics = listing_detail_profit %>% 
      filter(property_type == input$prop_type, 
             neighbourhood_group_cleansed == input$neighborhood, 
             host_response_time != 'N/A') %>% 
      top_n(10, profit) %>% 
      summarise(avg_pavg_score = mean(review_scores_rating))
    
    infoBox(
      title = tags$b("Average Rating"),
      value = tags$b(round(metrics$avg_pavg_score, 0)), 
      color = "aqua",
      fill = TRUE,
      icon = icon("calculator")
    )    
  })

  
  #data ####
  output$table=renderDataTable(
    datatable(listing_detail_profit) %>% 
      formatStyle(input$selected,
                  background="skyblue", 
                  fontWeight='bold')
  )
  
  
  
  
  
}

shinyApp(ui,server)

