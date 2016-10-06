library(shiny)
library(igraph)
library(networkD3)
library(googleVis)
library(dplyr)
source("helper.R")

data_path <- "../server_get/twitterdata"

# library(twitteR)
# oauth <- read.csv(paste(data_path, "oauth", sep = "/"), sep = " ", header = FALSE)[,2]
# setup_twitter_oauth(consumer_key = oauth[1], 
#                     consumer_secret = oauth[2], 
#                     access_token = oauth[3], 
#                     access_secret = oauth[4])

rters <- fromDB(paste(data_path, "twitter.db", sep = "/"), "users")
tweets <- fromDB(paste(data_path, "twitter.db", sep = "/"), "tweets")
fusion <- rters[1,]
influence <- read.csv(paste(data_path, "influencers(20 posts).csv", sep = "/"))
colnames(influence)[3] = 'screenName'

df <- read.csv(paste(data_path, "top100.csv", sep = "/"))
df <- df[1:20, ]
df$t_date <- as.Date(df$t_date)
df2 <- read.csv(paste(data_path, "mapping.csv", sep = "/"))
userdf <- read.csv(paste(data_path, "influencers(20 posts).csv", sep = "/"))
userdf <- as.tbl(userdf)
userdf <- userdf[which(!duplicated(userdf$influencer)), ]
userdf <- arrange(userdf, desc(score))

shinyServer(function(input, output) {
    
    rter_id <- reactive({
        rter_id <- read.csv(paste(data_path, "tweets", 
                                  input$tweet, sep = "/"))$id
        rter_id
    })
    
    

    rtLinks <- reactive({
        rter_id <- c(fusion$id, rev(intersect(rter_id(), 
                                              dir(paste(data_path, "friends", sep = "/")))))
        friendShip <- c()
        for(i in 2:length(rter_id)) {
            friend <- intersect(rter_id[1:(i-1)], 
                                  read.csv(paste(data_path, "friends", rter_id[i], sep = "/"))$id)
            if(length(friend)) {
                friendShip <- rbind(friendShip, cbind(friend, rep(rter_id[i], length(friend))))
            }
        }
        friendShip <- data.frame(matrix(sapply(friendShip, 
                                               function(x) rters$screenName[rters$id == x]), ncol = 2))
    })
    
     
    
    
    
    output$rtNetwork <- renderSimpleNetwork({
        simpleNetwork(rtLinks())
    })
        
    
    
    
    output$info2 <- renderPrint({
      cat(paste('Retweets:',as.character(df[df$t_id==input$tweet,]$t_retweets)))
    })    
    output$info3 <- renderPrint({
      cat(paste('Favorites:',as.character(df[df$t_id==input$tweet,]$t_favorites)))
    }) 
    output$info4 <- renderPrint({
      cat(as.character(df[df$t_id==input$tweet,]$t_text))
    }) 
    
    tweetsdf <- reactive({ df[df$t_date > input$date, ][df$X < input$slider, ] }) 
    from.where <- reactive({ input$subject.source })
    subj <- reactive({ input$subject })
    
    
    
   
    output$subject <- renderUI({
      subjects <- switch(from.where(),
                         "section" = unique(tweetsdf()$w_tags_section),
                         "topic" = unique(tweetsdf()$w_tags_topic),
                         "hashtag" = unique(tweetsdf()$t_hashtags))
      subjects <- gsub('\"', '', subjects)
      subjects <- gsub(' ', '.', subjects)
      subjects <- sort(unique(unlist(strsplit(subjects, ','))))
      radioButtons("subject", label = "Subject",
                   choices = subjects,
                   selected = subjects[1])
    })
    
    
   
    
})
