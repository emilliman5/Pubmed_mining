library(shiny)
library(rCharts)
options(RCHART_LIB = 'polycharts')

shinyUI(fluidPage(
    titlePanel("Kelly and Eric's super-awesome data mining adventure"),
    
    sidebarLayout(
        sidebarPanel(h1("Navigation"),
                     p("This will be a place to select your dataset, whether it 
                       be by FY, date range, Topic assignment, PMID, Grant ID, 
                       Journal"),
                     fileInput("file",label=h3("File Upload"),accept="txt"),
                     checkboxGroupInput("fy",selected = 2010,
                                        label=h3("Fiscal Years"),
                                        choices=list("ALL"=c(2009,2010,2011,2012,2013,2014,2015),"FY2009"=2009, "FY2010"=2010,
                                                     "FY2011"=2011,"FY2012"=2012,
                                                     "FY2013"=2013,"FY2014"=2014,
                                                     "FY2015"=2015)),
                     radioButtons("topicK",selected = 2,label = "Topic Model Selection",choices = 
                                      list("25 Topics"=1,"50 Topics"=2,"100 Topics"=3,
                                           "250 Topics"=4,"500 Topics"=5,"1000 Topics"=6)),
                     textInput("words",label = "Enter keywords here:",value = "")
                     ),
        mainPanel(
            sliderInput("slider",label=h3("Max Number of Words"),min=10, max=500, value=25),
            plotOutput("wordcloud"),
            showOutput("topics", "Nvd3"),
            forceNetworkOutput("network")
#             tableOutput("assoc"),
#             sliderInput("corr",label=h3("Minimum Correlation for Term associations"), min=0, max=1, value=0.3)
            ))
    ))