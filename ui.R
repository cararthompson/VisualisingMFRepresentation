
library(shiny)
library(shinythemes)
library(plotly)

# Define UI 
shinyUI(fluidPage(theme = shinytheme("sandstone"),  
                  title = "Visualising Male and Female Representation in Text",
                  # Application title
                  titlePanel(h1("Visualising Male and Female Representation in Text")),
                  
                  # Sidebar with a slider input for number of bins 
                  sidebarLayout(
                    sidebarPanel(h2("Your text"),
                                 
                                 textAreaInput(
                                   inputId = "extract",
                                   label = "Please type or copy your text into this box.",
                                   value = "",
                                   width = NULL,
                                   height = '500px',
                                   cols = NULL,
                                   rows = NULL,
                                   placeholder = NULL,
                                   resize = "vertical"
                                 )
                                 ,
                                 
                                 selectInput(
                                   inputId = "n_topics",
                                   label = "How many topics does your text explore?",
                                   choices = c("No idea" = NA,
                                               "2" = 2,
                                               "3" = 3,
                                               "4" = 4,
                                               "5" = 5,
                                               "More than 5" = 6
                                   ),
                                   selected = "No idea"
                                 ),
                                 h4("Want to keep a copy of this analysis?"),
                                 downloadButton("report",
                                                "Export analysis"),
                                 p(""),
                                 p("Clicking this button will save a copy of this analysis as an html file to a location of your choice, 
                                   which you can open using your web browser.")
                    ),
                    
                    # Outputs
                    mainPanel(
                      p("This app analyses the text you enter in the side panel and produces three sets of graphs. 
                        It does not store any of your data."),
                      p(tags$ul(tags$li("The first set of graphs focuses on gender balance, showing the relative proportion 
                                        of explicitly Male and Female words within each sentence, and the words used within sentences
                                        which contained predominantly Male or predominantly Female words."),
                                tags$li("The second set presents a Sentiment analysis, showing the different emotions detected in your text
                                        and the evolution of positive and negative emotions throughout your text. Once again, the graphs
                                        allow you to see how the Male and Female words in your text are tied to the emotions."),
                                tags$li("The third set presents a Topic analysis, showing you how your words 
                                        clustered together, and highlighting again where those topics were tied to words 
                                        in predominantly Male or Female sentences."))),
                      p("The aim is not to force a perfect balance (there may be a perfectly valid reason why you have more
                      predominantly Female sentences, for example if the main character in your text is Female). The aim is to allow writers 
                      and speakers to see the balance within their text, and take steps to redress it where necessary."),
                      p("Happy exploration!
                      
                      "),
                      
                      tabsetPanel(type = "tabs",
                                  # tabPanel("Print", verbatimTextOutput("text1"))
                                  # ,
                                  tabPanel("Gender balance",
                                           fluidRow(column(12,
                                                           div(
                                                             style = "position:relative",
                                                             h3("What was the proportion of words referring to Males and Females in each of your sentences?"),
                                                             p("Each square represents a sentence from your text. The colour of each square is determined
                                                                by counting how many words refer to Male or Female persons in each sentence. Words are 
                                                               categorised as referring to Male or Female persons using two lists from ",
                                                               tags$a(href = "http://ucrel-api.lancaster.ac.uk/usas/tagger.html",
                                                                      "the USAS system"),
                                                               ". The app makes no attempt at guessing the gender of names. Sentences which didn't contain any words 
                                                               referring to Males or Females are grey. Those with a mix of the two are purple. The higher
                                                               the proportion of Female vs. Male words, the more pink the square is; the higher the proportion 
                                                               of Male vs. Female words, the more blue the square is. Throughout the text in this app, a 
                                                               \"predominantly Female sentence\" is one in which the proportion of Females mentioned in the 
                                                               sentence was higher than the proportion of Males."),
                                                             plotOutput("gender_waffle",
                                                                        hover = hoverOpts("plot_hover",
                                                                                          delay = 100,
                                                                                          delayType = "debounce")),
                                                             uiOutput("hover_info")
                                                           )),
                                                    column(12,
                                                           div(
                                                             style = "position:relative",
                                                             h3("Which words were associated with predominantly Female, Neutral, and predominantly Male sentences?"),
                                                             p("The bigger the word, the more often it featured in your text. If your text is very long, only the most 
                                                    frequent words will be shown. 
                                                    If the wordcloud for any given gender just says \"N/A\", the app didn't classify any sentences as 
                                                    predominantly featuring that gender. As an illustration, if you copy the text from this page 
                                                    into the box, the Male cloud says \"N/A\" because all the sentences are classed as predominantly Female or 
                                                    Neutral."),
                                                    plotOutput("gender_clouds")
                                                           ))
                                                    ,
                                                    width = 7)),
                                  tabPanel("Sentiment analysis", 
                                           h3("Which emotions were associated with the words in the predominantly Female, predominantly Male and 
                                              Neutral sentences?"),
                                           p("Using the sentence split performed in the Gender Balance tab, this first graph shows the emotions 
                                             associated with words in each of the three sentence groups. This is an imperfect tool. The emotions 
                                             associated with each word are taken from the commonly used ",
                                             tags$a(href = "https://www.saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm",
                                                    "NRC lexicon. "),
                                             "As you hover over each bar, the app shows you which words the lexicon associated with each emotion (and, 
                                             in brackets, how many times each of those words were used within each sentence group). 
                                             Use this feature to see for yourself whether you think the words have been appropriately classified. 
                                             For example, if you copy the text from this page, you will see that many of the negative emotions are 
                                             tied to the NRC lexicon's interpretation of the word \"sentence\"."),
                                           plotlyOutput("sa_plot"),
                                           h3("How do the emotions evolve throughout your text?"),
                                           p("Using the",
                                             tags$a(href = "http://www2.imm.dtu.dk/pubdb/pubs/6010-full.html", 
                                                    "AFINN lexicon,"),
                                             "which assigns a score from -5 to +5 to words according to how negative or 
                                             positive the emotions associated with that word are, this next graph shows a negative to positive
                                             emotional score for each sentence in your text. In assigning these scores, the immediate context of each 
                                             word was taken into account (e.g. \"not happy\" has a negative score but \"happy\" has a positive score). 
                                             The sentences are coloured according to the relative proportions of Male and Female words contained 
                                             within them, to allow you to visualise not only the progression of emotions through your text but also 
                                             any associations between associations between the predominant gender of your sentences and the positive 
                                             or negative emotions they convey."),
                                           plotOutput("sa_progress",
                                                      hover = hoverOpts("plot_hover",
                                                                        delay = 100,
                                                                        delayType = "debounce")),
                                           uiOutput("sa_hover")
                                  )
                                  
                                  ,
                                  tabPanel("Topic analysis",
                                           h3("How did your words cluster into topics, and what type of sentences did they feature in?"),
                                           p("Using",
                                             tags$a(href = "https://en.wikipedia.org/wiki/Latent_Dirichlet_allocation",
                                                    "Latent Dirichlet allocation (LDA),"), 
                                             "one of the most widely used algorithms for topic modelling, 
                                             this tab presents a series of wordclouds of the top 10 words in each word cluster (topic) in your text.
                                             The number of clusters comes from the number of topics you selected in the dropdown menu. If you selected \"no idea\",
                                             the algorithm has gone with three. If you look at the clouds and think that some of them contain a mix 
                                             of topics, try increasing the number of topics until they show a cohesive set of words. 
                                             If you see the same word being the biggest word across several topics, 
                                             try reducing the number of topics so there is less overlap between them."),
                                           p("These wordclouds also allow you to explore gender balance in the topics, by colour coding each word 
                                             according to whether it featured in neutral sentences only (grey words), sentences in which there
                                             as a higher proportion of Male words, or sentences in which there was a higher proportion of Female words. 
                                             If one of your important topics is mostly one gender colour, you may wish to revisit the sentences in 
                                             which it is explored."),
                                           plotOutput("topic_plot")),
                                  tabPanel("Resources",
                                           h4("Resources used in building this app"),
                                           p(tags$ul(
                                             tags$li(tags$a(href = "https://www.tidytextmining.com/index.html", 
                                                            "Text Mining with R: A Tidy Approach - Julia Silge & David Robertson")),
                                             tags$li("Categories S2.1 and S2.2 from",
                                                     tags$a(href = "http://ucrel-api.lancaster.ac.uk/usas/tagger.html", 
                                                            "the USAS system - Paul Rayson")),
                                             tags$li(tags$a(href = "https://onlinelibrary.wiley.com/doi/abs/10.1111/j.1467-8640.2012.00460.x",
                                                            "NRC Lexicon")),
                                             tags$li(tags$a(href = "https://arxiv.org/abs/1103.2903",
                                                            "AFINN Lexicon")))),
                                           h4("Further reading"),
                                           p(tags$ul(
                                             tags$li(tags$a(href = "https://www.bloomsbury.com/uk/an-introduction-to-religious-language-9781350095755/",
                                                            "An Introduction to Religious Language: Exploring Theolinguistics in Contemporary Contexts - Valerie Hobbs")),
                                             tags$li("Preaching to Women and Sitting under the Word (Ch. 10), from ", 
                                                     tags$a(href = "https://www.prpbooks.com/book/no-little-women",
                                                            "No Little Women - Aimee Byrd")))),
                                           h4("With thanks to"),
                                           p(tags$ul(tags$li("Dr Valerie Hobbs, Aimee Byrd, Andrew Thompson and Mark Magill for their feedback on the prototype"),
                                                     tags$li("Prof Paul Rayson for providing access to the lists of Male and Female words used by the USAS system"))),
                                           h4("And finally... "),
                                           p(tags$ul(
                                             tags$li("To report a bug or request a new feature, click on \"New Issue\" ",
                                                     tags$a(href = "https://github.com/cararthompson/VisualisingMFRepresentation/issues",
                                                            "here"), "."),
                                             tags$li("If you found this app useful, please share it with others and reference it using my name (Dr Cara R. Thompson) and
                                                     the apps's url if you use it in a publication."),
                                             tags$li("For any other queries, to provide feedback, or to request further bespoke dataviz work, please ",
                                                     tags$a(href = "https://twitter.com/cararthompson",
                                                            "get in touch"), "!")))
                                  )
                      )
                    )
                  )
)
)