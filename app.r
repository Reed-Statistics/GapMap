library(shiny)
library(shinydashboard)
library(shinythemes)

ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(title = "Gap Map"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "introduction", icon = icon("dashboard")),
      menuItem("US Health Insurance", tabName = "health", icon = icon("hospital")),
      menuItem("US COVID19", tabName = "covid", icon = icon("ambulance")),
      menuItem("Correlation", tabName = "corr", icon = icon("chart-line")),
      menuItem("More Readings and Sources", tabName = "readings", icon = icon("newspaper")),
      menuItem("Source code", tabName="source", icon = icon("file-code-o"))
    )
  ),
  ## Body content
  dashboardBody(
    tabItems(
      tabItem(tabName = "introduction",
              fluidRow(
                div(
                  tags$img(src ="https://www.evaluate.com/sites/default/files/media/images/teasers/Virus%20Image.jpeg",
                           width = "110px", height = "110px", class="WrapText", align="right", style=" border: 1px solid #ddd;border-radius: 4px; padding: 4px; margin-left: 5px;margin-bottom: 0px;"),
                  h2("COVID19 and the Health System in the US", align="center"),
                  tags$p(
                    "The COVID-19, also known as the coronavirus is the ongoing",
                    HTML(paste0(tags$a(href = "https://en.wikipedia.org/wiki/COVID-19_pandemic", "pandemic", target = "_blank"))),
                    "in 2019.It is caused by severe acute respiratory syndrome",
                    HTML(paste0(tags$a(href = "https://en.wikipedia.org/wiki/Severe_acute_respiratory_syndrome_coronavirus_2", " coronavirus 2 (SARS‑CoV‑2)", target = "_blank"), ".")),
                    " As of 6 May 2020,more than 3.66 million cases of COVID-19 have been reported 
                    all throughout the world of which 1.23 million cases are in the US",
                    HTML(paste0(tags$a(href = "https://www.who.int/emergencies/diseases/novel-coronavirus-2019", "(WHO)"), ".")),
                    "As of May 6, 2020, the U.S. has the most confirmed active cases and deaths in the world."),
                  tags$p(
                    align="right",
                    style="padding: 2px",
                    tags$strong("Source:"),
                    HTML(paste0(tags$a(href = "https://www.evaluate.com/sites/default/files/media/images/teasers/Virus%20Image.jpeg", "Evaluate")))
                  ),
                  tags$img(src ="https://c8.alamy.com/comp/DGPTGG/word-cloud-in-the-shape-of-the-united-states-showing-words-dealing-DGPTGG.jpg",
                           width = "320px", height = "235px", align="right", class="WrapText", style=" border: 1px solid #ddd;border-radius: 4px; padding: 4px; margin-left: 3px;"),
                  p("Amidst this crisis, the health system and management in the US has been 
an important and recurring topic in the larger discourse. Of the seemingly endless tragic effects of the novel 
coronavirus, the sever oublic health crisis is an urgent one. Locksdowns and social distancing have caused people to stay home, 
causing businesses to shutter and lay off workers. And with",HTML(paste0(tags$a(href = "https://www.theguardian.com/commentisfree/2020/mar/27/coronavirus-pandemic-americans-health-insurance", "roughly half of Americans", target = "_blank"))),
                    "getting their health insurance from their employer, these layoffs mean not only losing their income but also their medical coverage. In other words, just as our need for medical care
skyrockets in the face of a global pandemic, fewer will have health insurance or be able to afford it. According to one recent report,
the cost of treatment for Covid-19 can run" ,HTML(paste0(tags$a(href = "https://time.com/5806312/coronavirus-treatment-cost/", "around $35,000.", target = "_blank"))), "So now is especially urgent time to talk about the accessibility and affordability
of health insurance in the US.") ,tags$br(),
                  p("The very pandemic that is threatening to infect and kill millions is simultaneously
  causing many to also lose their health insurance. Though there seems to be some relation between one's accessibility
of health care and the severity of the pandemic, we decided to look more into if there is any direct relation between the two.
Here, we have tried to look into the relation between health insurance coverage and severity (confirmed cases and deaths)
in all the counties in the US. We can also look at an interactive chart showing any direct correlation between the two.")
                )
                
              )
              ),
      
      tabItem(tabName = "health",
          h2("hell0")
              
      ),
      
      tabItem(tabName = "covid",
              h1("COVID19")
        
      ),
      
      tabItem(tabName = "corr",
              h1("hello")
      ),
      
      
      tabItem(tabName = "source",
              h1("Code"))
    )
    
  ) # end dashboardBody
  
)# end dashboardPage

server <- function(input, output) { }

shinyApp(ui, server)
