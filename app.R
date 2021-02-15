#library ####
library(shiny)
library(data.table)
library(lubridate)
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(scales)
library(ggthemes)
library(DT)
library(shinydashboard)

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
    skin = "yellow",
    
    dashboardHeader(title='Dashboard'),
    dashboardSidebar(
      sidebarUserPanel('Arbnb in Seattle',
                       image = 'https://img.icons8.com/bubbles/50/000000/city.png'),
      sidebarMenu(
          menuItem('Introduction',tabName = 'introduction',icon=icon('info')),
          menuItem('Obervation',tabName = 'obv',icon=icon('chart-bar')),
          menuItem('Listing Comparison',tabName = 'explore',icon=icon('globe-americas')),  
          menuItem('Data',tabName = 'data',icon=icon('database')),  
          menuItem('About Me',tabName = 'me',icon=icon('hand-paper'))  
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
                      img(src = "SEA.jpg", height = 400, width = 200)
                    ),
                    mainPanel(
                      h3("Project goal:"),
                      h4("This Shiny app aims to provide Arbnb hosts in Seattle profitibility insights and a tool to discover how 
                         their listings compare to similar listings in Seattle."),
                      h3("Data in scope:"),
                      h4("1. Listings with a review at least within six months of data gathering"),
                      h4("2. Date range is June 2020 - November 2020"),
                      h3("Note on listing profit:"),
                      h4("1. Arbnb doesn not differenciate date booked by customers or date blocked out by hosts,
                         therefore the profit listed here would be an over estimation of actual gains by hosts."),
                      h4("2. Maximum occupency is capped at 70%, the cap is reached, 
                         profit is adjusted down by 30% as a rough adjustment to accomodate the overestimation"),
        
                    )
                  )
                  
                  
                  
                  
                )),
#obv ####
        tabItem(tabName = 'obv',
                tabsetPanel(type = "tabs",
                            tabPanel("Profit Gap", 
                                     titlePanel("Listings vs. Profit"),
                                     
                                     fluidRow(
                                       
                                       
                                       # h4("Selected bin size:"),
                                       # column(4, verbatimTextOutput("value")),
                                       column(12,
                                         h4("There is a significant gap between the top quantile and bottom quantile listings"),
                                         plotOutput("profdiff"),
                                       
                                         sliderInput("slider", label = h3("Average Profit Bin Size"), min = 2, 
                                                   max = 5, value = 4),
                                       
                                         h4("By finding what contributes to profitibility, 
                                            hosts could potentially increase their profits."), 
                                         
                                         h4("Many factors were considered to see its incluence in profitability:
                                            and they could be found the following tabs."),
                                         
  
                                        
                                         ),
                                            )
                                    ),
                            tabPanel("Lower Influence Metrics", 
                                     h4("Super Host status: overrated"),
                                     plotOutput("superhost"),
                                     h4("Listing score: overrated"),
                                     plotOutput("listscore"),
                                     
                                     ),
                            tabPanel("Higher Influence Metrics", 
                                     h4("Accomodation: the more the better"),
                                     plotOutput("accomodation"),
                                     h4("Room Number: 3, 4, 5 are the best"),
                                     plotOutput("numBed"),
                                     h4("Minimum Nights: monthly most profitable"),
                                     plotOutput("minNight"),
                                     h4("Cancellation Policy: the stricker the better"),
                                     plotOutput("cancelPol"),
                                     h4("Response time: as long as it's within a day"),
                                     plotOutput("resptime"),

                                     
                                     )
                          )
                          
              ),
              
#explore ####
        tabItem(tabName = 'explore',
                titlePanel("Listings by Neighborhood and Property Type"),
                h3("Learning from the best: key metrics of the top 10 profitable listings in the area with the same property type"),
                
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
    
                
                fluidRow(box(
                  #dataTableOutput('table'),width=12
                  DT::dataTableOutput("table"),style = "height:500px; overflow-y: scroll;overflow-x: scroll;",
                  width = 12
                  ))

              ),

# About Me ####
tabItem(tabName = 'me',
        titlePanel("About Me"),
        fluidRow(
          
          sidebarLayout(
            sidebarPanel(
              img(src = 'IMG_1065.jpg', height="95%", width="95%")
            ),
            mainPanel(
              h4("I am a data entheusiest and have had extensive experience 
                 in logistics and supply chain."),
              h4("Being able to extract valueable information from data for 
                 valueable insights brings me great enjoyment."),
              h4("Tools I am familiar with are SQL, Tableau, and R."),
              h4("Please feel free to reach out to me:"),
              tags$a(href="https://www.linkedin.com/in/brianlkuo/", h4("LinkedIn")), 
              #tags$a(href="https://www.linkedin.com/in/brianlkuo/", h4("Github"))
            )
          )
        
 
        )
        
) #tab name "me"

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
  
 #obv graphs #### 
  output$profdiff=renderPlot(
    {   listing_detail_profit %>% 
        mutate(profit_quartile = as.character(ntile(-profit,input$slider))) %>% 
        select(profit_quartile, profit) %>% 
        ggplot(aes(profit_quartile, profit)) + 
        geom_boxplot()+
        labs( x='Quartile', y='Profit') +
        scale_y_continuous(labels = comma)+
        theme_hc()+ scale_colour_hc()

      } 
  )
  
  output$resptime=renderPlot(
    {   listing_detail_profit %>% 
        filter(host_response_time != 'N/A') %>% 
        group_by(host_response_time) %>% 
        #summarise(avg_profit = mean(profit)) %>% 
        #ggplot(aes(reorder(host_response_time, -avg_profit), avg_profit)) +#, col = Weekend/Weekday)) + 
        ggplot(aes(host_response_time, profit)) + 
        geom_boxplot() +
        labs(title='Profit vs. Response Time', x = '', y='Profit') +
        coord_flip()+ 
        scale_y_continuous(labels = comma)+
        theme_hc()+ scale_colour_hc() +
        theme(plot.title = element_text(hjust = 0.5))
      
    } 
  )
  
  output$superhost=renderPlot(
    {   
      listing_detail_profit %>% 
      ggplot(aes(host_is_superhost, profit)) + 
      geom_boxplot() +
      labs(title='', x = 'Super Host', y='Profit') +
      coord_flip()+ 
      scale_y_continuous(labels = comma)+
      theme_hc()+ scale_colour_hc()
      
    } 
  )

  output$listscore=renderPlot(
    {   
      listing_detail_profit %>% 
        filter(!is.na(review_scores_rating)) %>% 
        mutate(score_quartile = as.character(ntile(-review_scores_rating,4))) %>%  
        # mutate(minimum_nights = as.character(review_scores_rating)) %>% 
        ggplot(aes(score_quartile, profit)) + 
        geom_boxplot() +
        labs(title='', x = 'Score by Quartile', y='Profit') +
        coord_flip()+ 
        scale_y_continuous(labels = comma)+
        theme_hc()+ scale_colour_hc()  
      
    } 
  )
  output$accomodation=renderPlot(
    {   
      listing_detail_profit %>% 
        mutate(accommodate = as.character(accommodates)) %>% 
        ggplot(aes(reorder(accommodate, accommodates), profit)) + 
        geom_boxplot() +
        labs(title='', x = 'Accomodation', y='Profit') +
        coord_flip()+ 
        scale_y_continuous(labels = comma)+
        theme_hc()+ scale_colour_hc()  
      
    } 
  )
  output$numBed=renderPlot(
    {   
      listing_detail_profit %>% 
        filter(!is.na(bedrooms)) %>%
        mutate(bedrooms = as.character(bedrooms)) %>% 
        ggplot(aes(bedrooms, profit)) + 
        geom_boxplot() +
        labs(title='', x = 'Bedrooms', y='Profit') +
        coord_flip()+ 
        scale_y_continuous(labels = comma)+
        theme_hc()+ scale_colour_hc()  
      
    } 
  )
  output$minNight=renderPlot(
    {   
      listing_detail_profit %>% 
        filter(!is.na(minimum_nights)) %>%
        mutate(minimum_night = as.character(minimum_nights)) %>% 
        ggplot(aes(reorder(minimum_night, minimum_nights), profit)) + 
        geom_boxplot() +
        labs(title='', x = 'Min Nights', y='Profit') +
        coord_flip()+ 
        scale_y_continuous(labels = comma)+
        theme_hc()+ scale_colour_hc()   
      
    } 
  )
  
  output$cancelPol=renderPlot(
    {   
      listing_detail_profit %>% 
        ggplot(aes(cancellation_policy, profit)) + 
        geom_boxplot() +
        labs(title='', x = 'Policy', y='Profit') +
        coord_flip()+ 
        scale_y_continuous(labels = comma)+
        theme_hc()+ scale_colour_hc()  
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
      value = tags$b(percent(round(metrics$super_host_pct, 2))), 
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

