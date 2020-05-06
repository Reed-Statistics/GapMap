library(shiny)
library(shinydashboard)
library(shinythemes)

ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(title = "Gap Map"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "introduction", icon = icon("dashboard")),
      menuItem("US Health Insurance", tabName = "US County-Wise Health Insurance Coverage", icon = icon("hospital")),
      menuItem("US COVID19", tabName = "US County-wise COVID19 Cases", icon = icon("ambulance")),
      menuItem("Correlation", tabName = "Correlation between Insurance and COVID Cases", icon = icon("chart-line")),
      menuItem("More Readings and Sources", tabName = "More readings and sources", icon = icon("newspaper")),
      menuItem("Source code", tabName="Source code", icon = icon("file-code-o"))
    )
  ),
  ## Body content
  dashboardBody(
    tabItems(
      tabItem(tabName = "introduction",
              fluidRow(
                tags$img(src ="https://c8.alamy.com/comp/DGPTGG/word-cloud-in-the-shape-of-the-united-states-showing-words-dealing-DGPTGG.jpg",
                         width = "300px", height = "200px", align="right")
              )
      ),
      
      tabItem(tabName = "US County-Wise Health Insurance Coverage",
          h2("hell0")
              
      ),
      
      tabItem(tabName = "US County-wise COVID19 Cases",
              h1("COVID19")
        
      ),
      
      tabItem(tabName = "Correlation between Insurance and COVID Cases",
              h1("hello")
      ),
      
      tabItem(tabName = "More readings and sources",
             h1("hello")
      ), 
      
      tabItem(tabName = "Source code",
              h1("Code"))
    )
    
  ) # end dashboardBody
  
)# end dashboardPage

server <- function(input, output) { }

shinyApp(ui, server)
