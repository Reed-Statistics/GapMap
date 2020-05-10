
library(shiny)
library(shinydashboard)
library(shinythemes)
library(tidycensus)
library(tidyverse)
library(tidyr)
library(leaflet)
library(glue)
library(sf)
library(viridis)
library(leaflet.extras)

ui <- 
  dashboardPage(
  skin = "purple",
  dashboardHeader(title = "Gap Map"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "introduction", icon = icon("dashboard")),
      menuItem("US COVID19 & Health Insurance", tabName = "covid", icon = icon("ambulance")),
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
                    " As of 6 May 2020, more than 3.66 million cases of COVID-19 have been reported 
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
                    coronavirus, the severe public health crisis is an urgent one. Locksdowns and social distancing have caused people to stay home, 
                    causing businesses to shutter and lay off workers. And with",HTML(paste0(tags$a(href = "https://www.theguardian.com/commentisfree/2020/mar/27/coronavirus-pandemic-americans-health-insurance", "roughly half of Americans", target = "_blank"))),
                    "getting their health insurance from their employer, these layoffs mean not only losing their income but also their medical coverage. In other words, just as our need for medical care
                    skyrockets in the face of a global pandemic, fewer will have health insurance or be able to afford it. According to one recent report,
                    the cost of treatment for Covid-19 can run" ,HTML(paste0(tags$a(href = "https://time.com/5806312/coronavirus-treatment-cost/", "around $35,000.", target = "_blank"))), "So now is especially urgent time to talk about the accessibility and affordability
                    of health insurance in the US.") ,tags$br(),
                  p("The very pandemic that is threatening to infect and kill millions is simultaneously
                    causing many to also lose their health insurance. Though there seems to be some relation between one's accessibility
                    of health care and the severity of the pandemic, this app is designed to look more into the direct correlation between the two  in all the counties in the US.
                    One can explore the relation between the health insurance coverage and COVID19 severity (confirmed cases and deaths) either through the spatial data
in the US map under US COVID19 & Health Insurance tab or through the correlation plot under the Correlation tab. We have also added more readings on the topic in the More Readings and Sources tab. Finally, the R
code used to built the app is in the Source code tab.")
                  )
                  )
                  ),
      
      tabItem(tabName = "covid",
              h2("US COVID19 and Health Insurance Coverage", align="center"),
              box(title = "COVID-19 Cases and Health Insurance Coverage in US Counties", 
                  status = "primary", 
                  leafletOutput("map_covid"),
                  width = 20,
                  selectInput("datatype", label = "Absolute numbers or by population?",
                    choices = c("Absolute Numbers" = "abs", "Per Capita" = "cap"),
                    selected = "cap"))
      ),
      
      tabItem(tabName = "corr",
              h3("Relationship between the US COVID19 & Health Insurance Coverage", align="center"),
              box(title = "COVID-19 Cases and Deaths versus Health Insurance Coverage in US Counties", 
                  status = "primary", 
                  plotOutput("corr"),
                  width = 20,
                  selectInput("cases_or_deaths", label = "Confirmed Cases or Deaths Per Capita",
                              choices = c("Cases", "Deaths"),
                              selected = "Deaths", multiple = FALSE),
                  sliderInput("howmany", label = "Counties with at least how many cases",
                              1, 100, 50))
      ),
      tabItem(tabName = "readings",
              fluidRow(
                div(
                  tags$img(src ="https://cdn2.iconfinder.com/data/icons/e-education/2048/6079_-_Read_Book_on_Laptop-512.png",
                           width = "200px", height = "200px", class="WrapText", align="right", style=" border: 1px solid #ddd;border-radius: 4px; padding: 4px; margin-left: 5px;margin-bottom: 0px;"),
                  h2("More Readings and Sources:", align="center"),
                  tags$p(
                    "For further readings on the topic, please check out the following sources:",
                    tags$ul(
                      tags$li(HTML(paste0(tags$a(href = "https://www.vox.com/policy-and-politics/2020/3/16/21173766/coronavirus-covid-19-us-cases-health-care-systemc", "Coronavirus is exposing all of the weaknesses in the US health system", target = "_blank")))),
                      tags$li(HTML(paste0(tags$a(href = "https://www.latimes.com/opinion/story/2020-04-16/coronavirus-health-insurance-unemployment", "COVID-19 has broken the U.S. health system. Now what?", target = "_blank", target = "_blank")))),
                      tags$li(HTML(paste0(tags$a(href = "https://www.theguardian.com/commentisfree/2020/mar/27/coronavirus-pandemic-americans-health-insurance", "Millions of Americans are about to lose their health insurance in a pandemic")))),
                      tags$li(HTML(paste0(tags$a(href = "https://www.theguardian.com/commentisfree/2020/mar/21/medicare-for-all-coronavirus-covid-19-single-payer", "America's extreme neoliberal healthcare system is putting the country at risk", target = "_blank")))),
                      tags$li(HTML(paste0(tags$a(href = "https://www.nytimes.com/2020/04/14/opinion/sunday/covid-inequality-health-care.html", "America Can Afford a World-Class Health System. Why Don’t We Have One?", target = "_blank")))),
                      tags$li(HTML(paste0(tags$a(href = "https://www.commonwealthfund.org/publications/podcast/2020/mar/coronavirus-reveals-flaws-us-health-system", "Coronavirus Reveals Flaws in the U.S. Health System", target = "_blank")))),
                      tags$li(HTML(paste0(tags$a(href = "https://www.forbes.com/sites/patriciagbarnes/2020/03/20/on-the-frontlines-of-covid-19-with-no-health-insurance/#2cdb42be2570", "On The Frontlines Of COVID-19 With No Health Insurance", target = "_blank")))),
                      tags$li(HTML(paste0(tags$a(href = "https://www.marketwatch.com/story/lost-your-job-based-health-insurance-during-the-coronavirus-pandemic-heres-how-to-get-coverage-2020-04-04", "Did you lose your job and your health insurance due to coronavirus?", target = "_blank")))),
                      tags$li(HTML(paste0(tags$a(href = "https://www.healthcaredive.com/news/covid-19-puts-unprecedented-strain-on-us-health-system/574275/", "COVID-19 puts unprecedented strain on US health system")))),
                      tags$li(HTML(paste0(tags$a(href = "https://time.com/5810260/coronavirus-will-have-long-lasting-impacts-on-the-u-s-health-care-system-and-the-poorest-will-suffer-most/", "Coronavirus Will Have Long-Lasting Impacts on the U.S. Health Care System—And the Poorest Will Suffer Most", target = "_blank")))),
                      tags$li(HTML(paste0(tags$a(href = "https://promarket.org/americas-broken-health-care-system-is-the-biggest-obstacle-to-containing-the-coronavirus/", "America’s Broken Health Care System Is the Biggest Obstacle to Containing the Coronavirus", target = "_blank")))),
                      tags$li(HTML(paste0(tags$a(href = "https://qz.com/1809382/us-health-care-costs-could-help-coronavirus-spread/", "The cost of American health care could help coronavirus spread in the US", target = "_blank")))),
                      tags$li(HTML(paste0(tags$a(href = "https://newrepublic.com/article/157101/health-insurance-crisis-coronavirus", "The Health Insurance Crisis at Our Doorstep", target = "_blank")))),
                      tags$li(HTML(paste0(tags$a(href = "https://www.usnews.com/news/health-news/articles/2020-04-01/as-unemployment-and-covid-19-cases-rise-who-will-pay-for-care", "As Unemployment and COVID-19 Cases Rise, Who Will Pay for Care?", target = "_blank")))),
                      tags$li(HTML(paste0(tags$a(href = "https://www.marcumllp.com/insights/impact-of-covid-19-on-the-health-insurance-industry", "Impact of COVID-19 on the health Insurance Industry", target = "_blank")))),
                      tags$li(HTML(paste0(tags$a(href = "https://abcnews.go.com/Health/make-covid-19-tests-treatment-easy-access-black/story?id=70048090", "Black Americans 'epicenter' of coronavirus crisis made worse by lack of insurance", target = "_blank")))),
                      tags$li(HTML(paste0(tags$a(href = "https://www.institutmontaigne.org/en/blog/how-covid-19-unveiling-us-healthcare-weaknesses", "How Covid-19 is Unveiling US Healthcare Weaknesses", target = "_blank")))),
                      tags$li(HTML(paste0(tags$a(href = "https://www.medicaleconomics.com/news/coronavirus-new-report-shows-grim-view-health-insurance-loss", "Coronavirus: New report shows grim view of health insurance loss", target = "_blank")))),
                      tags$li(HTML(paste0(tags$a(href = "https://www.cnn.com/2020/03/11/us/us-health-care-system-coronavirus/index.html", "Here's how the US health care system makes it harder to stop coronavirus", target = "_blank")))),
                      tags$li(HTML(paste0(tags$a(href = "https://pnhp.org/news/surprise-medical-bills-coronavirus-and-bad-insurance-3-arguments-for-medicare-for-all/", "COVID-19 and Medicare for All", target = "_blank")))),
                      tags$li(HTML(paste0(tags$a(href = "https://www.axios.com/coronavirus-health-care-costs-medical-bills-9ee15bd0-ed92-449c-8c17-f7d13a9a6aea.html", "Coronavirus could expose the worst parts of the U.S. health system", target = "_blank")))),
                      tags$li(HTML(paste0(tags$a(href = "https://www.al.com/news/2020/04/alabama-uninsured-patients-with-most-severe-covid-19-cases-could-face-huge-bills-possible-bankruptcy.html", "Alabama uninsured patients with most severe COVID-19 cases could face huge bills, possible bankruptcy"))))
                    )
                  )
                ))),
      tabItem(tabName = "source",
              tags$h1(
                align="left",
                style="padding: 2px",
                HTML(paste0(tags$a(href = "https://github.com/Reed-Statistics/GapMap","View Code", target = "_blank")))
              ))
                )
    
              ) # end dashboardBody
  
    )# end dashboardPage

server <- function(input, output) {
  comb <- st_read("covid.shp")
  pal <- colorNumeric(palette = "viridis", domain = comb$uninsured_percent)
  
  #Reactive content for Leaflet
  content <- reactive ({
    paste(
    comb_reactive_2()$NAME, '<br>',
    "Confirmed Cases", if (input$datatype == "cap") {" Per Million People"}, ": ",
    round(comb_reactive_2()$cases_capita_or_not), '<br>',
    "Deaths", if (input$datatype == "cap") {" Per Million People"}, ": ",
    round(comb_reactive_2()$deaths_capita_or_not))
  })
  
  output$map_covid <- renderLeaflet({
    comb_reactive_2() %>%
      sf::st_transform(crs = "+init=epsg:4326") %>%
      leaflet() %>%
      addProviderTiles(provider = "CartoDB.Positron") %>%
      addPolygons(popup = content(), fillColor = ~pal(unnsrd_),
                  stroke = FALSE, fillOpacity = 0.9,  smoothFactor = 0) %>%
      addLegend("bottomright", pal = pal, 
                values = ~unnsrd_, title = "Uninsured Percentage of 18-44yrs",
                opacity = 1)
  })
  
  #reactive dataframe for correlation
  comb_reactive <- reactive ({ 
    comb %>%
      mutate(deaths_or_cases = case_when(
        input$cases_or_deaths == "Cases" ~ Cnfrmd_,
        input$cases_or_deaths == "Deaths" ~ Dths_cp
      )) %>%
      filter(Confrmd > input$howmany)
  })
  
  #reactive dataframe for leaflet
  comb_reactive_2 <- reactive ({ 
    comb %>%
      mutate(cases_capita_or_not = case_when(
        input$datatype == "abs" ~ Confrmd,
        input$datatype == "cap" ~ Cnfrmd_
      )) %>%
      mutate(deaths_capita_or_not = case_when(
        input$datatype == "abs" ~ Deaths,
        input$datatype == "cap" ~ Dths_cp
      ))
  })
  
  output$corr <- renderPlot({
    
    ggplot(data = comb_reactive(), mapping = aes(x = unnsrd_, y = log(deaths_or_cases))) +
      geom_point(alpha = 0.7) +
      xlab("Uninsured Percentage") +
      ylab(glue("Log ", input$cases_or_deaths, " Per Million People")) +
      geom_smooth(method = "lm")
  })
}

shinyApp(ui, server)
