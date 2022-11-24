library(tidyverse)
library(janitor)
library(tidytext)
library(plotly)
library(shiny)
library(dplyr)


readRenviron(path = '.Renviron')
source("sendmail.R",local = T)


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Sample Aplication which on error sent email"),
  
  # # Sidebar with a slider input for number of bins 
  # sidebarLayout(
  #     sidebarPanel(
  #         sliderInput("bins",
  #                     "Number of bins:",
  #                     min = 1,
  #                     max = 50,
  #                     value = 30)
  #     ),
  
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("irisPlot"),
    plotOutput("iris_geom_point"),
    plotOutput("iris_geom_line"),
    plotlyOutput("sin")
    
  )
)
# )

# Define server logic required to draw a histogram
server <- function(input, output,session) {
  
  user<- "dinesh@sirpi.io"
  
  tryCatch({
    ghhhhfg
    output$irisPlot <- renderPlot({
      iris %>% 
        as_tibble() %>% 
        clean_names() %>% 
        pivot_longer(-species,
                     names_to = "flower_feature",
                     values_to = "value") %>% 
        group_by(species,flower_feature) %>% 
        summarise(mean_feature_dimension = value %>% mean(),
                  .groups = "drop") %>% 
        mutate(flower_feature = flower_feature %>% factor()) %>% 
        #mutate(flower_feature = flower_feature %>% factor()) %>% 
        ggplot(aes(x = species %>% reorder_within(-mean_feature_dimension,
                                                  flower_feature),
                   y = mean_feature_dimension)) + 
        geom_col(aes(fill = species))+ 
        geom_label(aes(label = mean_feature_dimension %>% round(2))) +
        facet_wrap(~flower_feature, ncol = 2, scales = "free")+ 
        labs(y = "Mean Feature Dimension",
             title = "Iris Dimension Mean Summary",
             x = "Species")
      
    })
    
    output$iris_geom_point<-renderPlot({
      iris %>% 
        ggplot(aes(Sepal.Length,Sepal.Width))+
        geom_point()
    })
    output$iris_geom_line<-renderPlot({
      iris %>% 
        ggplot(aes(Sepal.Length,Sepal.Width))+
        geom_line()
      
    })
    
    output$sin<-renderPlotly({
      # x<-10
      # sincx <- reactive(x)({
      #   sin(x)/x
      # }) 
      
      tibble(x = c(-360:360)*3,
             x_rad = x/180*pi,
             sin = sin(x_rad),
             cos = cos(x_rad),
      ) %>% 
        pivot_longer(-c(1:2),
                     names_to = "trig_fn",
                     values_to = "value") %>% 
        ggplot(aes(x = x,
                   y = value)) + 
        geom_line() + 
        geom_vline(xintercept = seq(-360*3, 360*3, by = 360),
                   color = "red") +
        facet_wrap(~ trig_fn, scales = "free", ncol = 1) -> trig_plot
      
      trig_plot %>% 
        ggplotly()
    })
  }, error= function(e) {
    print("in error block")
    shinyalert("Unexpected Error!", "Something went wrong in  the app", type = "warning")
    error_msg <- "Something went wrong in the app "
    send_notification_email(user=user,error_msg = error_msg)
    print(paste0("EMAIL SENT TO ",user))
    
    
    #stopApp()
  })
  
  
} 


# Run the application 
shinyApp(ui = ui, server = server)
