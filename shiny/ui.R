library(shiny)
library(rCharts)
library(visNetwork)
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
                     radioButtons("topicK",selected = 5,label = p(h3("Topic Model Selection"),"This will select the 
                                                                  topic model for viewing topic usage and topic-topic 
                                                                  connections in the network, both on the \"Topic Plots\"
                                                                  tab."), choices = 
                                      list("25 Topics"=6,"50 Topics"=5,"100 Topics"=4,
                                           "250 Topics"=1,"500 Topics"=2,"1000 Topics"=3)),
                     textInput("words",label = "Enter keywords here:",value = ""),
                     width=3),
        mainPanel(
          tabsetPanel(
            tabPanel("About",
                    h2("Notes"),
                    h3("This site aims to provide access to Kelly and Eric's 
                      super-awesome text mining adventure. This page will 
                      contain a description of methods and the data."),
                    h3("Please be gentle with the site. There is a lot of data behind the scenes that
                       needs to be operated on when parameters are changed. I suggest
                       not trying to visualize all the data (do not select \"ALL\") without first 
                       restricting the data by grant or PMIDs."),
                    h2("The Corpus"),
                    p("Publications were reteived from Pubmed (accessed on:2015-12-28) using their
                      advanced search. Publications with a grant ID beginning with \"ES\" and published 
                      between 2008-10-01 and 2015-09-30 were downloaded in XML format. This has resulted 
			          in ~25,000 publications incorporated into our text collection (the corpus)."),
                    p("Publication titles and abstracts were combined to create the body of text to be mined. 
                      Before mining a number of cleaning steps were preformed. 1) Numbers, puncuation, 
                      non-ASCII characters,and extra white space were removed. 2) Very common words were removed. 
                      This includes: gene, environment, cell, expression, control, chemical, etc. These words are 
                      removed because thy do not provide any classification power because they show up in so many 
		              publications. 3) Words that show up in less than 10% of the documents were also 
			          removed because they are too sparse.", br(),br(), "The corpus was then explored using 
			                word clouds and various plots of vocabulary complexiety to assess cleanliness and processing.", br(), 
			                "Topics were modeled across the corpus in two ways. 1) Using the entire corpus various numbers 
			                of topics were modeled (25, 50, 10, 250 ,500, and 1000). 2) For each fiscal year represented 
			                topics were modeled at various levels (25, 50, 100, 250, 250, 500, 1000).", br(),br(), "Topics were 
			                modeled using Latent Dirichlet Allocation. (Blei", tags$i("et al")," 2003) This machine learning algorithm 
			                is a mixture model, mixed membership classification. That means in a collection of documents there 
			                exists multiple groups (topics) and that each document can be a member of multiple groups 
			                (a document discusses more than one topic). Model fit was assessed by plotting the logliklihood 
			                across the number of topics modeled as well as using our pre-existing domain knowledge (human sanity check).
			                The alpha and beta parameters were optimized using variational expectation maximization (VEM). LDA \
			                topic modeling produces two distributions: 1) the distribution of topics for each document (gamma), 
			                this represents the probability a topic exists in the document. 2) the word distribution for each 
			                topic, this assigens the importance of each word to a topic. LDA assigns a non-zero value for each 
			                combination of document-topic and word-topic distributions. Thus cutoffs need to be applied to 
			                both beta and gamma based on the distributions across the entire corpus. For example, gamma distributions 
			                for 50 topics may have generally higher values than for 1000 topics, because the pie has to be distributed 
			                more with 1000 topics. Generally we set the gamma threshold such that each document is assigned 2-3 topics 
			                with few documents being assigened up to 7. This seems intuitive for scientific literature, but is arbitrary."),
                    showOutput("pubs", "nvd3"),                
                    sliderInput("slider",label=h3("Number of Words to Display"),min=10, max=500, value=100),
                    plotOutput("wordcloud",height = "100%")),
            tabPanel("Topic Plots",
              h3("Topic Usage"),
              p("This plot shows how much a topic was discussed in the dataset selected. This was 
                calculated by summing the topic probabilities (gamma value) for a given topic. Only the top 5 topics per document were used, in an effort to capture only the significant topic assigents. This plot may still be sensitive to the number of documents in the the selected corpus"),
              showOutput("topics", "nvd3"),
              sliderInput("dist",label=p(h4("Distance Measure Threshold"), "The slider represents the top x % of connections to retain"),min=0, max=0.25, value=0.1),
              h4(textOutput("text")),
              visNetworkOutput("force",height="1600px", width="1600px")),
            tabPanel("Publications Table",
                     dataTableOutput("papers")
            ),
            tabPanel("DendroArcs",
                        div(style="display:inline-block", sliderInput("treeDist",label="Minimum Distance for Topic-Topic Associations:", 
                                 min=0.5, max=1, value=0.90)),
                        div(style="display:inline-block", selectInput("proxy", label=p(br(),"Topic-topic distance calculation method:",br()), 
                                    selected="cosine", choices=list("cosine","hellinger","euclidean","bhjattacharyya"))),
                        p(""),
                        div(style="display:inline-block", radioButtons("treeK",selected = 5,label = "Topic Model Selection",choices = 
                                                    list("25 Topics"=6,"50 Topics"=5,"100 Topics"=4,
                                                         "250 Topics"=3,"500 Topics"=2,"1000 Topics"=1), inline=T)),
                        div(style="display:inline-block", radioButtons("topicTree", selected=4, label="Beta-term Tree Method", 
                                  choices=list("Cosine"=1, "Hellinger"=2, "Pearson's"=3, "Bhjattacharyya"=4), inline=TRUE)),
                     selectInput("topicN", label=h4("Anchor Topic"),selected=1, choices=list(Topic1=1, Topic2=3)),
                     uiOutput("dendroArc.ui")
                     ),
            tabPanel("Word Assoc",
                br(),
                h4("This chart shows the importance (or weight/probability) a term has for each topic
                   of a given model (known as the beta value)."),
                radioButtons("K",selected = 5,label = "Topic Model Selection",choices = 
                                                    list("25 Topics"=6,"50 Topics"=5,"100 Topics"=4,
                                                         "250 Topics"=3,"500 Topics"=2,"1000 Topics"=1), inline=T),
                showOutput("keywordTopic","nvd3"),
                sliderInput("corr",label=h3("Minimum Correlation for Term associations"), 
                            min=0, max=1, value=0.25),
                dataTableOutput("assoc")
            ),
            tabPanel("Classifications",
                     tabsetPanel(
                         tabPanel("Topic Assignment",
                                h3("Classify a document based on our exisiting topic models"),
                                radioButtons("Ktopic",selected = 5,label = "Topic Model Selection",choices = 
                                                    list("25 Topics"=6,"50 Topics"=5,"100 Topics"=4,
                                                         "250 Topics"=3,"500 Topics"=2,"1000 Topics"=1), inline=T),
                                h4("Copy and paste document text and press submit. This will likely work best with abstracts or abstract-length texts."),
                                tags$textarea(id="abstract",value = "", cols=150, rows=5),
                                actionButton("submit", "Submit"),
                                showOutput("classify", "polycharts")),
                     tabPanel("Closest Pubs",
                              dataTableOutput("closestPubs"))
                     )
                ),
            tabPanel("Topic Evolution",
                     h4("Topic Models, modeled by FY"),
                     radioButtons("Ktopic2",selected = 5,label = "Topic Model Selection",choices = 
                                                    list("50 Topics"=5,"100 Topics"=4,
                                                         "250 Topics"=3,"500 Topics"=2,"1000 Topics"=1), inline=T),
                     sliderInput("riverThresh", label="Distance Threshold",min=0, max=1, value=0.5),
                     sliderInput("dateRange",label="FY Range", min=2009,max=2015,step=1, value=c(2010,2012)),
                     radioButtons("riverDist", selected=3, label="Distance Measure Calculation", choices=list("Cosine"=1,"Correlation"=2,"Hellinger"=3), inline=T),
                     chartOutput("river", "d3/rCharts_d3_sankey"),
                     br(),br(),br()
                     ) #Close Topic Evolution tabPanel
            )#tabset panel
        ) #Main Panel
    ) #sidebar Layout
    ) #Fluid Page
)#ShinyUI
