library(shiny)
library(rCharts)
library(visNetwork)
#library(networkD3)
options(RCHART_LIB = 'nvd3')

shinyUI(fluidPage(
    titlePanel("Kelly and Eric's super-awesome data mining adventure"),
    
    sidebarLayout(
        sidebarPanel(h1("Search"),
                     p("This will be a place to select your dataset, whether it 
                       be by FY, date range, Topic assignment, PMID, Grant ID, or 
                       Journal"),
                     h3("File Upload:"),
                     fileInput("file",label="You can uplod a file of PMIDs or Grant IDs (but not both) for searching the 
                               Corpus. Grant IDs need to be in the form of ES######. There should only be one ID per line. 
                               Plain text files only, no *.docx, *.xlsx, etc.",accept="txt"),
                     checkboxGroupInput("fy",selected = 2010,
                                        label=p(h3("Fiscal Years"),"Selection of FYs will select the data used to make plots 
                                                in the \"Topic Plots\" tab."),
                                        choices=list("ALL"="ALL","FY2009"=2009, "FY2010"=2010,
                                                     "FY2011"=2011,"FY2012"=2012,
                                                     "FY2013"=2013,"FY2014"=2014,
                                                     "FY2015"=2015)),
                     radioButtons("topicK",selected = 2,label = p(h3("Topic Model Selection"),"This will select the 
                                                                  topic model for viewing topic usage and topic-topic 
                                                                  connections in the network, both on the \"Topic Plots\"
                                                                  tab."), choices = 
                                      list("25 Topics"=1,"50 Topics"=2,"100 Topics"=3,
                                           "250 Topics"=4,"500 Topics"=5,"1000 Topics"=6)),
                     textInput("words",label = "Enter keywords here:",value = ""),
                     width=3),
        mainPanel(
            #img(src="christmas.jpeg",align="center"),
          tabsetPanel(
            tabPanel("About",
                    h2("Notes"),
                    h3("This site aims to provide access to Kelly and Eric's 
                      super-awesome text mining adventure. This page will 
                      contain a description of methods and the data."),
                    h3("Please be gentle with the site. There is a lot of data behind the scenes that
                       needs to be operated on when parameters are changed. I suggest
                       not trying to viusalize all the data (do not select \"ALL\") without first 
                       restricting the data by grant or PMIDs."),
                    h2("The Corpus"),
                    p("Publications were reteived from Pubmed (accessed on:2015-10-02) using their
                      advanced search. Publications with a grant ID beginning with \"ES\" and published 
                      between 2008-10-01 and 2015-09-30 were downloaded in XML format."),
                    p("Publication titles and abstracts were combined to create the body of text to be mined. 
                      Before mining a number of cleaning steps were preformed. 1) Numbers, puncuation, 
                      non-ASCII characters,and extra white space were removed. 2) Very common words were removed. 
                      This includes: gene, environment, cell, expression, control, chemical, etc. These words are 
                      removed because thy do not provide any classification power because they show up in so many publications"),
                    plotOutput("pubs", width="100%"),                
                    plotOutput("pubs.q", width="100%"),
                    sliderInput("slider",label=h3("Max Number of Words"),min=10, max=500, value=50),
                    plotOutput("wordcloud",height = "100%")),
            tabPanel("Topic Plots",
              h3("Topic Usage"),
              p("This plot shows how much a topic was discussed in the dataset selected. This was 
                calculated by summing each documents topic probability (gamma value) for a given topic and then divided by 
                the number documents in the corpus group (number of publications in the FY)."),
              showOutput("topics", "nvd3"),
              sliderInput("dist",label=p(h4("Distance Measure Threshold"), "The slider represents the top x % of connections to retain"),min=0, max=0.5, value=0.15),
              visNetworkOutput("force",height="800px")),
            tabPanel("Publications",
                     dataTableOutput("papers")
            ),
#              tabPanel("DendroArcs",
#                       checkboxGroupInput("dendrofy",selected = 2010,inline=T,
#                                         label=h3("Fiscal Years"),
#                                         choices=list("ALL"="ALL","FY2009"=2009, "FY2010"=2010,
#                                         "FY2011"=2011,"FY2012"=2012,
#                                         "FY2013"=2013,"FY2014"=2014,
#                                         "FY2015"=2015)),
#                       showOutput("dendroarc")
#                       ),
            tabPanel("Word Assoc",
                br(),
                h4("This chart shows the importance (or weight/probability) a term has for each topic
                   of a given model (known as the beta value)."),
                radioButtons("K",selected = 2,label = "Topic Model Selection",choices = 
                               list("25 Topics"=1,"50 Topics"=2,"100 Topics"=3,
                                    "250 Topics"=4,"500 Topics"=5,"1000 Topics"=6), inline=T),
                showOutput("keywordTopic","nvd3"),
                sliderInput("corr",label=h3("Minimum Correlation for Term associations"), 
                            min=0, max=1, value=0.25),
                dataTableOutput("assoc")
            )
          )            
        ))
))