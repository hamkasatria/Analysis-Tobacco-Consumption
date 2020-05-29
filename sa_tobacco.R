
#mealkukan pemanggilan semua library
library(vroom)
library(here)
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)

##meakukan inport dataset
tobacco <- vroom(here("R","project","tobacco.csv"))
##menyiapkan option
option_state <- unique(tobacco$State)
option_parameter<- colnames(tobacco)[-c(1, 2,7,8)]


#user interface
ui <- fluidPage(
  title = "Tobacco Consumption",
  headerPanel("Tobacco Consumption"),
  sidebarLayout(
    ##panel input
    sidebarPanel(
      selectInput(inputId = "parameter",
                  label =  "category",
                  choices = option_parameter,
                  selected = "Never_smoked"),
      selectInput(inputId = "state",
                  label =  "state",
                  choices = option_state,
                  multiple = TRUE,
                  selected = option_state[[1]]),
    ),
    ##panel output
    mainPanel(
      plotlyOutput(outputId = "aq_plot")
    )
  )
)


##bagian server
server <- function(input, output, session) {
##melalakukan filter state dan plot dengan parameter categorynya
    aq_plot <- reactive({
    tobacco %>%
      filter(State %in% input$state) %>%
      ggplot(aes_string(x = "Year", y = input$parameter,colour="State")) +
      geom_point()+
      labs(
        x = "Year",
        y = "Category",
        colour = "State",
        title = "Tobacco Consumption Graph"
      ) +
      theme_light()
  })
#hasil yang ditampilkan    
  output$aq_plot <- renderPlotly({
    ggplotly(aq_plot())
  })
}

#runn_app
shinyApp(ui = ui, server = server, options = list(height = "500px"))