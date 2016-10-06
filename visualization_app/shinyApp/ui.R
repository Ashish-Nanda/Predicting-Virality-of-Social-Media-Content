library(shiny)
library(networkD3)
library(dplyr)
library(DT)

data_path <- "../server_get/twitterdata/"

# merge multiple results by desc indexing on retweets amount
tweets <- dir(paste(data_path, "tweets/", sep = "/"))
top20 <- read.csv(paste(data_path, "influencers(20 posts).csv", sep = "/"))
top100 <- read.csv(paste(data_path, "top100.csv", sep = "/"))
tweets_top20 <- unique(top20$t_id)
tweets <- intersect(tweets,tweets_top20)
tweets <- top100 %>%
  filter(t_id %in% tweets) %>%
  arrange(desc(t_retweets))
tweets <- tweets$t_id


shinyUI(navbarPage(theme="darkly.css",
                   "RETWEET GRAPH",

                   tabPanel("Tweet Analysis",
                            fluidRow(
                                column(3,
                                       htmlOutput("website"),
                                       h4("Select A Tweet Id"),
                                       selectInput("tweet",
                                                   NULL,
                                                   tweets,612121494232145920),
                                       
                                       textOutput("info2"),
                                       textOutput("info3")
                                       
                                       
                                       ),
                                column(9,
                                       tabsetPanel(
                                           tabPanel("Information flow graph",
                                                    br(),
                                                    fluidRow(
                                                        column(8,
                                                               simpleNetworkOutput("rtNetwork"))))
#                                                     simpleNetworkOutput("rtNetwork")),
                                           
                                           )
                                       )
                                )
                            )
                   
))
