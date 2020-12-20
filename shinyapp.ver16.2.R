# install_github("juliasilge/tidytext")
# remotes::install_github("r-hub/rhub")
library(tidyverse)
library(class)
library(data.table)
library(datasets)
library(geojsonio)
library(leaflet)
library(writexl)
library(knitr)
library(lubridate)
library(remotes)
# install.packages("tidytext")
library(tidytext)
library(dplyr)
library(tidyr)
library(broom)
library(devtools)
library(fmsb)
library(geniusr)
library(genius)
library(spotifyr)
library(ggplot2)
library(quanteda)
library(stm)
library(ggridges)
library(scales)
library(DT)
library(xml2)
library(rvest)
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(shinydashboard)
library(wesanderson)
library(caret)
library(ipred)
library("e1071")
library(fastAdaboost)
library(rpart)
library(randomForest)
library(caret) #predicts in a trained function
library(caretEnsemble)
library(lattice)
library(fmsb)
library(rattle)
library(MASS)
library(ISLR)
library(class)
library(data.table)
library(readr)
library(caretEnsemble)
library(lattice)
library(fmsb)
library(rattle)
library(MASS)
library(ISLR)
library(class)
library(data.table)
library(caret)
library(ipred)
library("e1071")
library(fastAdaboost)
library(rpart)
library(randomForest)
library(caret) #predicts in a trained function
library(caretEnsemble)
library(lattice)
library(fmsb)
library(rattle)
library(MASS)
library(ISLR)
library(class)
library(data.table)
library(tidyverse)
# retrieving Spotify access token
id <- Sys.setenv(SPOTIFY_CLIENT_ID = 'SECRET')
secret <- Sys.setenv(SPOTIFY_CLIENT_SECRET = 'SECRET')
access_token = get_spotify_access_token(client_id = Sys.getenv('SPOTIFY_CLIENT_ID'), client_secret = Sys.getenv('SPOTIFY_CLIENT_SECRET'))

# setwd("/Volumes/external/fall 2020/spotify project/spotify R")
# combine both test and train data from before
test.data <- read_csv("friends_joined.test.csv")
train.data <- read_csv("friends_joined.train.csv")
model.data <- rbind(test.data, train.data)
indexes <- sample(nrow(model.data),
                  size = nrow(model.data) * 0.7)

model.data.train <- model.data[indexes, ] #grabbing rows from the indexes
model.data.test <- model.data[-indexes, ]
tree.model2 <- rpart(factor(like) ~ + acousticness + liveness + tempo + valence,
                     data = model.data.train)

radarchart2 <- function (df, axistype = 0, seg = 4, pty = 16, pcol = 1:8, plty = 1:6, 
                         plwd = 1, pdensity = NULL, pangle = 45, pfcol = NA, cglty = 3, 
                         cglwd = 1, cglcol = "navy", axislabcol = "blue", vlabcol = "black", title = "", 
                         maxmin = TRUE, na.itp = TRUE, centerzero = FALSE, vlabels = NULL, 
                         vlcex = NULL, caxislabels = NULL, calcex = NULL, paxislabels = NULL, 
                         palcex = NULL, ...) 
{
  if (!is.data.frame(df)) {
    cat("The data must be given as dataframe.\n")
    return()
  }
  if ((n <- length(df)) < 3) {
    cat("The number of variables must be 3 or more.\n")
    return()
  }
  if (maxmin == FALSE) {
    dfmax <- apply(df, 2, max)
    dfmin <- apply(df, 2, min)
    df <- rbind(dfmax, dfmin, df)
  }
  plot(c(-1.2, 1.2), c(-1.2, 1.2), type = "n", frame.plot = FALSE, 
       axes = FALSE, xlab = "", ylab = "", main = title, asp = 1, 
       ...)
  theta <- seq(90, 450, length = n + 1) * pi/180
  theta <- theta[1:n]
  xx <- cos(theta)
  yy <- sin(theta)
  CGap <- ifelse(centerzero, 0, 1)
  for (i in 0:seg) {
    polygon(xx * (i + CGap)/(seg + CGap), yy * (i + CGap)/(seg + 
                                                             CGap), lty = cglty, lwd = cglwd, border = cglcol)
    if (axistype == 1 | axistype == 3) 
      CAXISLABELS <- paste(i/seg * 100, "(%)")
    if (axistype == 4 | axistype == 5) 
      CAXISLABELS <- sprintf("%3.2f", i/seg)
    if (!is.null(caxislabels) & (i < length(caxislabels))) 
      CAXISLABELS <- caxislabels[i + 1]
    if (axistype == 1 | axistype == 3 | axistype == 4 | 
        axistype == 5) {
      if (is.null(calcex)) 
        text(-0.05, (i + CGap)/(seg + CGap), CAXISLABELS, 
             col = axislabcol)
      else text(-0.05, (i + CGap)/(seg + CGap), CAXISLABELS, 
                col = axislabcol, cex = calcex)
    }
  }
  if (centerzero) {
    arrows(0, 0, xx * 1, yy * 1, lwd = cglwd, lty = cglty, 
           length = 0, col = cglcol)
  }
  else {
    arrows(xx/(seg + CGap), yy/(seg + CGap), xx * 1, yy * 
             1, lwd = cglwd, lty = cglty, length = 0, col = cglcol)
  }
  PAXISLABELS <- df[1, 1:n]
  if (!is.null(paxislabels)) 
    PAXISLABELS <- paxislabels
  if (axistype == 2 | axistype == 3 | axistype == 5) {
    if (is.null(palcex)) 
      text(xx[1:n], yy[1:n], PAXISLABELS, col = axislabcol)
    else text(xx[1:n], yy[1:n], PAXISLABELS, col = axislabcol, 
              cex = palcex)
  }
  VLABELS <- colnames(df)
  if (!is.null(vlabels)) 
    VLABELS <- vlabels
  if (is.null(vlcex)) 
    text(xx * 1.2, yy * 1.2, VLABELS, col = vlabcol)
  else text(xx * 1.2, yy * 1.2, VLABELS, cex = vlcex, col = vlabcol)
  series <- length(df[[1]])
  SX <- series - 2
  if (length(pty) < SX) {
    ptys <- rep(pty, SX)
  }
  else {
    ptys <- pty
  }
  if (length(pcol) < SX) {
    pcols <- rep(pcol, SX)
  }
  else {
    pcols <- pcol
  }
  if (length(plty) < SX) {
    pltys <- rep(plty, SX)
  }
  else {
    pltys <- plty
  }
  if (length(plwd) < SX) {
    plwds <- rep(plwd, SX)
  }
  else {
    plwds <- plwd
  }
  if (length(pdensity) < SX) {
    pdensities <- rep(pdensity, SX)
  }
  else {
    pdensities <- pdensity
  }
  if (length(pangle) < SX) {
    pangles <- rep(pangle, SX)
  }
  else {
    pangles <- pangle
  }
  if (length(pfcol) < SX) {
    pfcols <- rep(pfcol, SX)
  }
  else {
    pfcols <- pfcol
  }
  for (i in 3:series) {
    xxs <- xx
    yys <- yy
    scale <- CGap/(seg + CGap) + (df[i, ] - df[2, ])/(df[1, 
                                                         ] - df[2, ]) * seg/(seg + CGap)
    if (sum(!is.na(df[i, ])) < 3) {
      cat(sprintf("[DATA NOT ENOUGH] at %d\n%g\n", i, 
                  df[i, ]))
    }
    else {
      for (j in 1:n) {
        if (is.na(df[i, j])) {
          if (na.itp) {
            left <- ifelse(j > 1, j - 1, n)
            while (is.na(df[i, left])) {
              left <- ifelse(left > 1, left - 1, n)
            }
            right <- ifelse(j < n, j + 1, 1)
            while (is.na(df[i, right])) {
              right <- ifelse(right < n, right + 1, 
                              1)
            }
            xxleft <- xx[left] * CGap/(seg + CGap) + 
              xx[left] * (df[i, left] - df[2, left])/(df[1, 
                                                         left] - df[2, left]) * seg/(seg + CGap)
            yyleft <- yy[left] * CGap/(seg + CGap) + 
              yy[left] * (df[i, left] - df[2, left])/(df[1, 
                                                         left] - df[2, left]) * seg/(seg + CGap)
            xxright <- xx[right] * CGap/(seg + CGap) + 
              xx[right] * (df[i, right] - df[2, right])/(df[1, 
                                                            right] - df[2, right]) * seg/(seg + 
                                                                                            CGap)
            yyright <- yy[right] * CGap/(seg + CGap) + 
              yy[right] * (df[i, right] - df[2, right])/(df[1, 
                                                            right] - df[2, right]) * seg/(seg + 
                                                                                            CGap)
            if (xxleft > xxright) {
              xxtmp <- xxleft
              yytmp <- yyleft
              xxleft <- xxright
              yyleft <- yyright
              xxright <- xxtmp
              yyright <- yytmp
            }
            xxs[j] <- xx[j] * (yyleft * xxright - yyright * 
                                 xxleft)/(yy[j] * (xxright - xxleft) - 
                                            xx[j] * (yyright - yyleft))
            yys[j] <- (yy[j]/xx[j]) * xxs[j]
          }
          else {
            xxs[j] <- 0
            yys[j] <- 0
          }
        }
        else {
          xxs[j] <- xx[j] * CGap/(seg + CGap) + xx[j] * 
            (df[i, j] - df[2, j])/(df[1, j] - df[2, 
                                                 j]) * seg/(seg + CGap)
          yys[j] <- yy[j] * CGap/(seg + CGap) + yy[j] * 
            (df[i, j] - df[2, j])/(df[1, j] - df[2, 
                                                 j]) * seg/(seg + CGap)
        }
      }
      if (is.null(pdensities)) {
        polygon(xxs, yys, lty = pltys[i - 2], lwd = plwds[i - 
                                                            2], border = pcols[i - 2], col = pfcols[i - 
                                                                                                      2])
      }
      else {
        polygon(xxs, yys, lty = pltys[i - 2], lwd = plwds[i - 
                                                            2], border = pcols[i - 2], density = pdensities[i - 
                                                                                                              2], angle = pangles[i - 2], col = pfcols[i - 
                                                                                                                                                         2])
      }
      points(xx * scale, yy * scale, pch = ptys[i - 2], 
             col = pcols[i - 2])
    }
  }
}

ui <- fluidPage(
  theme = shinytheme("cyborg"), # setBackgroundColor("#E5F4DC"),
  titlePanel(tags$h1(strong(em("MOOD SWINGS")), style = "color:#1DB954")),
  h6("by Jennifer Ko | Middlebury College '20.5", style = "color:#EAFAEF"),
  headerPanel(tags$h3("Redefine your mood with new music.", style = "color:#1DB954")),
  tabsetPanel(type = "tabs", 
              tabPanel("Recommendation",
                       sidebarLayout(
                         sidebarPanel(
                           actionButton("save", label = "Save selected song"),
                           actionButton("rec", label = "Find new songs!"),
                           actionButton("reset", label = "Reset selection"),
                           searchInput(
                             inputId = "search", 
                             label = "1. Search by artist or track", 
                             placeholder = "Type here", 
                             btnSearch = icon("search"), 
                             btnReset = icon("remove"), 
                             width = "100%"),
                           selectizeInput(inputId = "picker", label = "2. Choose 3-5 songs. * Please save one song at a time. The app will not function properly otherwise.", 
                                          choices = NULL,
                                          multiple = TRUE,                        
                                          options = list(create = TRUE, placeholder = "Click here",
                                                         maxItems = 5
                                          )),
                           h3("Selected Songs", style = "color:#1DB954"),
                           tableOutput("testtable"),
                           htmlOutput(outputId = "res")),
                         mainPanel(
                           h5("Instructions & Notes", style = "color:#1DB954"),
                           h6("Once you type in the artist name and select a song, click 'Save selected song' to see data visualizations you will see as you scroll down.", style = "color:#EAFAEF"),
                           h6("Only one click of 'Find new songs!' button is necessary for the first recommendation", style = "color:#EAFAEF"),
                           h6("For your convenience, 'Recommended Songs' section will automatically update when you click 'Save selected songs'. However, you can click the 'Find new songs!' button for another round of recommendation from the same list of songs or by adding more songs.", style = "color:#EAFAEF"),
                           h6("You may restart the app by clicking 'Reset selection' or reloading the page.", style = "color:#EAFAEF"),
                           h6("Allow 10 sec. (max) for graphics to appear. 'Recommended Songs' may take longer due to data size and running time of code.", style = "color:#EAFAEF"),
                           h6("Ignore errors, if any. The app may shut down if you click 'Save selected song' twice.", style = "color:#EAFAEF"),
                           h3("Recommended Songs", style = "color:#1DB954"),
                           tableOutput("rec_table"),
                           h3("Overview of Audio Features", style = "color:#1DB954"),
                           h6("Please select at least three songs to view audio features graph", style = "color:#EAFAEF"),
                           plotOutput(outputId = "selected_songs_geom_density_ridges"),
                           plotOutput(outputId = "selected_songs_audio_radar_chart"),
                           h3("Lyrical Content", style = "color:#1DB954"),
                           plotOutput(outputId = "selected_tf_idf")
                         ))), 
              tabPanel("Our Analysis",
                       tags$head(
                         tags$style("description {white-space: nowrap;}")),
                       mainPanel(
                         h3("Intro", style = "color:#1DB954"),
                         h6("This fall semester, I created a web app of data visualizations and tree-based machine learning algorithm in order to recommend users songs based on their selection of five songs. I used spotifyr package as well as geniusr package in order to retrieve various information about songs, let alone audio features and lyrics.", style = "color:#EAFAEF"),
                         h3("Design", style = "color:#1DB954"),
                         h6("[Data Visualization]", style = "color:#EAFAEF"),
                         h6("Creating a radar chart was relatively new to me, especially with eight different audio features of each song I wanted to showcase. So, I used a function radarchart2 that I found online and scaled each audio feature from 0 to 100 for a holistic standard. I also used geom_ridges() to create a different way to compare the audio features. The “lyrical content” section was created with bind_tf_idf() that calculates the frequency of important words, pulled by unnest_tokens(), across the song(s) selected. Ultimately, each time a user clicks the “Save selected song” button, these visualizations are updated."
                            , style = "color:#EAFAEF"),
                         br(),
                         h6("[Tree-based model recommendation system]", style = "color:#EAFAEF"),
                         h6("1. Modeling with training and testing datasets", style = "color:#EAFAEF"),
                         h6("For training and testing datasets, I asked nine friends to give me a list of 10-15 songs that they like to listen. Then, using get_related_artists() function from those lists, I created dataset of “related artists” whose popularity and total followers are within the first and third quartiles, respectively. This allows to create reasonably sized data, from which I used get_artist_audio_features() function to pull audio features of every song from these artists. Given the large number of observations from this new dataset, I then randomly selected 15 songs using sample_n(), which my friends were asked to listen to and mark which ones they enjoyed and which ones they did not.",
                            style = "color:#EAFAEF"),
                         h6("Once they followed up, I compiled all the data, with the binary variable “like” that indicates whether they liked the song (1) or not (0). Because I want to incorporate both audio features and lyrics frequency to predict and recommend users new songs, I created a new variable ‘success rate’ calculated as follows, by each friend: (number of times a word appeared across the songs my friend A liked) / (total number of words across all songs that A listened to)."
                            , style = "color:#EAFAEF"),
                         h6("I used randomForest() and varImpPlot() to identify six most important variables that determine the “like” variable: lyrics frequency, tempo, valence, acousticness, loudness, and liveness. I then used bagging, boosting, and regression trees to decide which of the three to ue for the modeling. Boosting, for which I used adaboost(), unfortunately rendered errors in multiple tries, so between the remaining two functions, I decided to use regression trees rpart() that gave a higher accuracy rate on average than bagging. However, lyrical content took way too long. Although lyrical content (or ‘succes rate’) was the most important variable in creating the algorithm model, it could not be accounted for every song, because geniusr() package does not cover as much as spotifyr() in terms of song lyrics availability. With that reason, and more importantly to reduce the running time of code, I decided to not account for the ‘sucess rate’ variable and only consider the other five variables, all of which are selected audio features. The resulting accuracy rate from regression trees typically ranged from around 20% to 60%.",
                             style = "color:#EAFAEF"),
                         h6("2. Implementation" , style = "color:#EAFAEF"),
                         h6("In creating the actual function, I designated a table of artist and their Spotify ID to be its input. From there, the function would retrieve data in the order that I did with my friends. 
                            ", style = "color:#EAFAEF"),
                         h3("Difficulties & Solutions", style = "color:#1DB954"),
                         h6("[Initial design]", style = "color:#EAFAEF"),
                         h6("There are quite a few data visualization features that could not be part of the final app, as spotifyr package could only pull so much data. For example, I wanted users to be able to log in to their accounts, so that, using their streaming history with and without personalized playlists, the app could easily detect and identify top 5 songs in the past six months and the most recently played songs, through both of which the users could find their “mood”. I attempted to use JavaScript on Visual Studio Code, as recommended on the Spotify developers website. However, because it exclusively uses Web API that does not cover some features that the spotifyr() package that I want to use, I decided to not include the login feature"
                            , style = "color:#EAFAEF"),
                         h6("Instead, users are asked to choose between 3 and 5 songs. Among the many perks of this feature is that they are not required to log in -- that is, non-Spotify users are able to freely use the web app as well, increasing its accessibility and their music interests. Another benefit is that they can learn something new about the songs they have not noticed before. For example, by audio features chart, they can find which song(s) that they choose has/have higher acousticness or danceability. Also, if the initial design was executed, the recommended songs might not fully reflect the users’ preference, because the sentiment/mood from top 5 songs could range more widely than that from the most recently played songs, so the tree model would cover too much variation of audio features. Here, however, because the users have the option to choose their songs, they have more freedom to choose songs they want to select in the moment, increasing the chance of accuracy in the recommendation system.
", style = "color:#EAFAEF"),
                         h6("[Lyrical features]", style = "color:#EAFAEF"),
                         h6("Geniusr pulls data from genius.com, a song lyrics platform that unfortunately does not cover many non-English songs. Given this lack of lyrical content availability, not every song that my friends chose and that I recommended them had lyrics, with which I could have analyzed frequency of words with bind_tf_idf(). 
", style = "color:#EAFAEF"),
                         h6("[Dataset and running time]", style = "color:#EAFAEF"),
                         h6("The algorithm function I made runs many code chunks that require to pull data and save them as new datasets. And because of this process, rendering the final output from this function can often be lengthy. A similar case holds for lyrical content, as mentioned before -- this would lengthen the running time even more, because it scrapes through each row of the many rows of each of ten songs. So I had to try my best to keep the code as concise as possible. The running time ranges from 4 to 10 seconds per each action button click.
", style = "color:#EAFAEF"),
                         h3("Conclusion", style = "color:#1DB954"),
                         h6("I have learned so much from building this app, especially with the design and feasibility. Focusing on both backend and frontend development, I was a developer and user, trying to find what features could work most efficiently. Alongside with the aforementioned features, I was excited to learn how to create a search bar that renders a list of songs according to whatever artist a user types in, and tables that are only reactive to the songs that the user chooses, because these two are the backbones of the remaining analysis and codes of the project."
                            , style = "color:#EAFAEF"),
                         br(),
                         h6("Although the project does not include everything that I had hoped, it does the job perfectly fine, as it achieves my original objective to recommend users new songs based on their music history, which is reflected in their self-selection. From online research and exploration of Spotify Web API-based web apps and spotifyr-based data science projects, I came to learn that there is no existing web app with spotifyr and/or geniusr prior to my own. With this in mind, I hope this project serves as a starting point for web app development using spotifyr() package, focusing on either data visualization or machine learning, or both.", style = "color:#EAFAEF"),
                         h3("Sources", style = "color:#1DB954"),
                         h6("Some links I used for inspiration are:
https://developer.spotify.com/documentation/web-api/reference/tracks/get-audio-features/", style = "color:#EAFAEF"),
                         h6("https://stackoverflow.com/questions/49968975/shiny-pickerintput-choices-based-on-search-bar", style = "color:#EAFAEF"),
                         h6("https://medium.com/@boplantinga/what-do-spotifys-audio-features-tell-us-about-this-year-s-eurovision-song-contest-66ad188e112a#:~:text=Loudness%20values%20are%20averaged%20across,to%201.0%20the%20attribute%20value
", style = "color:#EAFAEF"),
                         h6("https://mastering-shiny.org/action-tidy.html
", style = "color:#EAFAEF")
                         ))
  ))

server <- function(input, output, session) {
  values3 <- reactiveValues(table = data.frame(artist = NULL, song = NULL, id = NULL))
  values2 <- reactiveValues(table1 = data.frame(danceability = NULL, energy = NULL, loudness = NULL, speechiness = NULL, acousticness = NULL, instrumentalness = NULL, liveness = NULL, valence = NULL, tempo = NULL))
  values4 <- reactiveValues(table2 = data.frame(lyric = NULL, song_name = NULL))
  values8 <- reactiveValues(table = data.frame(genres = NULL, href = NULL, id = NULL, images = NULL, name = NULL, popularity = NULL, type = NULL, uri = NULL, external_urls.spotify = NULL, followers.href = NULL, followers.total = NULL))
  values7 <- reactiveValues(table = data.frame(artist_name <- NULL, danceability = NULL, energy = NULL, loudness = NULL, speechiness = NULL, acousticness = NULL, instrumentalness = NULL, liveness = NULL, valence = NULL, tempo = NULL, track_name = NULL))
  
  colors_border=sample(wes_palette("Zissou1", 8, type = "continuous")) #radar chart colors
  skip_to_next <- FALSE
  af_minmax <- data.frame(danceability=rep(NA), energy =rep(NA), loudness=rep(NA), speechiness=rep(NA), acousticness=rep(NA), instrumentalness=rep(NA), liveness=rep(NA), valence=rep(NA), tempo=rep(NA),  # as many cols as you need
                          stringsAsFactors=FALSE)
  # for radarchart
  af_minmax = rbind(c(0, 0, -60, 0, 0, 0, 0, 0, 60), af_minmax)
  af_minmax = rbind(c(1, 1, 0, 1, 1, 1, 1, 1, 180), af_minmax)
  af_minmax <- af_minmax[-c(3),]
  observeEvent(input$search, {
    possibleArtists <- search_spotify(input$search)[["tracks"]][["items"]][["name"]] %>%
      unique() %>%
      as_tibble()
    updatePickerInput(
      session = session,
      inputId = "picker",
      choices = possibleArtists)
  }, ignoreInit = TRUE)
  
  observe({
    input$reset
    values3$table <- NULL
    values2$table1 <- NULL
    values4$table2 <- NULL
    values7$table <- NULL
    values8$table <- NULL
    user_ra <- list()
    user_audio <- list()
  })
  
  observeEvent(input$save, {
    if(length(as.character(input$picker)) > 0) {
      newLine <- data.frame(artist = input$search, 
                            song = input$picker,
                            id = (search_spotify(input$search)[["tracks"]][["items"]][["id"]][which(search_spotify(input$search)[["tracks"]][["items"]][["name"]] == input$picker)])[1],
                            artist_id = search_spotify(input$search)[["artists"]][["items"]][["id"]][1])
      values3$table <- rbind(values3$table, newLine) %>%
        dplyr::distinct()
    }
    
     for(n in 1:nrow(values3$table)){ # for audio features graphs
       tryCatch(temp <- get_track_audio_features(ids = values3$table$id[n]) %>%
                  dplyr::select(1,2,4, 6:11) %>%
                  data.frame(),
                error = function(e) {skip_to_next <<- TRUE})
       if(skip_to_next){next()}
     }
     values2$table1 <- rbind(values2$table1, temp)
     
     for (n in 1:nrow(values3$table)){ # for bind_tf_idf graph
       skip_to_next <- FALSE
       tryCatch(lyrics <- get_lyrics_search(values3$table$artist[n], values3$table$song[n]) %>%
                  dplyr::select(1,4) %>%
                  data.frame(),
                error = function(e) {skip_to_next <<- TRUE})
       if(skip_to_next){next()}
     } 
     names(lyrics)[1] <- "lyric"
     values4$table2 <- rbind(values4$table2, lyrics)
  }
  )
  
  observeEvent(input$save, {
    output$testtable <- renderTable({
      if(is.null(input$search)){
        return()
      } else{
        values3$table[,c(1,2)]}
    })
    
    output$lyric <- renderTable({
      if(is.null(input$picker) | is.null(input$search)){
        return()
      } else{values4$table2 %>%
          mutate(line = row_number()) %>%
          unnest_tokens(word, lyric) %>%
          anti_join(stop_words) %>%
          count(word, sort = TRUE) %>%
          mutate(total = sum(n)) %>%
          mutate(title = "most popular lyrics") %>%
          bind_tf_idf(word, title, n) %>%
          head(15)}
    })
    output$selected_songs_geom_density_ridges <- renderPlot({
      if(is.null(input$picker) | is.null(input$search)){
        return()
      } else{
        ggplot(values2$table1 %>%
                 mutate(loudness = rescale(values2$table1$loudness),
                        tempo = rescale(values2$table1$tempo)) %>%
                 gather("feature", "values"), # for density ridges
               aes(x = values, y = factor(feature))) +
          geom_density_ridges(scale = 2, size = 0.1, rel_min_height = 0.03, quantile_lines=TRUE,quantiles=2, alpha = 0.5) +
          theme_ridges() +
          ggtitle("Audio Features") +
          xlab("Value") +
          ylab("Audio feature") +
          labs(caption = "*Note: loudness and tempo are rescaled to 0-1 range.") +
          theme(legend.position = "top",
                legend.title = element_text(size = 10, color = "white"),
                legend.text = element_text(size = 10, color = "white"),
                axis.title = element_text(face = 2, color = "white"),
                axis.text = element_text(color = "white"),
                plot.caption = element_text(color = "grey"))}},
      bg = "transparent")
    
    output$selected_songs_audio_radar_chart <- renderPlot({
      if(is.null(input$picker) | is.null(input$search)){
        return()
      } else{radarchart2(rbind(af_minmax, values2$table1), axistype=1 ,
                         pcol= colors_border, plwd=1 , plty=1,
                         cglcol="grey", cglty=1, axislabcol="white", caxislabels=seq(0,100, 25), cglwd=0.8,
                         vlcex=0.8,
                         vlabcol = "white")
        legend(x=1, y=1,  bty = "n", legend = values3$table[,2], pch=20 , col=colors_border, text.col = "#9999a1", cex=0.8, pt.cex=1.5)
      }},
      bg = "transparent")
    
    output$selected_tf_idf <- renderPlot({
      if(is.null(input$picker) | is.null(input$search)){
        return()
      } else{
        ggplot(
          values4$table2 %>%
            mutate(line = row_number()) %>%
            unnest_tokens(word, lyric) %>%
            anti_join(stop_words) %>%
            count(word, sort = TRUE) %>%
            mutate(total = sum(n)) %>%
            mutate(title = "most popular lyrics") %>%
            bind_tf_idf(word, title, n) %>%
            mutate(word = factor(word, levels = rev(unique(word)))) %>%
            head(15),
          aes(word, tf, fill = "5EBF83")) +
          geom_bar(stat = "identity") +
          geom_col(show.legend = FALSE) +
          scale_fill_manual(values = c(rgb(0.2,0.5,0.3,1))) +
          labs(x = NULL, y = "frequency") +
          coord_flip() +
          ggtitle("Word Frequency") +
          theme(legend.position = "none",
                plot.background = element_rect(fill = "transparent"),
                panel.background = element_rect(fill = "transparent"),
                axis.title = element_text(face = 2, color = "white"),
                axis.text = element_text(color = "white"),
                plot.caption = element_text(color = "grey"),
                plot.title = element_text(color = "white"))}},
      bg = "transparent")
  })
  
  observeEvent(input$rec, {
    ## FINAL FINAL VERSION OF FUNCTION
    sugg_songs_final <- function(x){
      user_ra <- list()
      user_audio <- list()
      for (j in 1:nrow(x)){
        # skip_to_next <- FALSE
        tryCatch(user_ra[[j]] <- get_related_artists(x$artist_id[j]) %>%
                   dplyr::filter(name != x$artist[j]) %>%
                   distinct(name, .keep_all = TRUE) %>%
                   data.frame(),
                 error = function(e) {skip_to_next <<- TRUE})
        if(skip_to_next){next()}
      }
      user_ra_adj <- dplyr::bind_rows(user_ra, .id = "column.name")
      user_pop_q1 <- as.numeric(summary(user_ra_adj$popularity)[2]) # as.numeric(summary(user_ra_adj[,c(7)])[2])
      user_pop_q3 <- as.numeric(summary(user_ra_adj$popularity)[5]) # as.numeric(summary(user_ra_adj[,c(7)])[5])
      user_foll_q1 <- as.numeric(summary(user_ra_adj$followers.total)[2]) # as.numeric(summary(user_ra_adj[,c(12)])[2])
      user_foll_q3 <- as.numeric(summary(user_ra_adj$followers.total)[5])# as.numeric(summary(user_ra_adj[,c(12)])[5])
      
      user_ra_adj <- user_ra_adj %>%
        dplyr::filter(popularity > user_pop_q1 & popularity < user_pop_q3) %>%
        dplyr::filter(followers.total > user_foll_q1 & followers.total < user_foll_q3)
      # skip_to_next <- FALSE
      for(j in 1:nrow(user_ra_adj)){
        tryCatch(user_audio[[j]] <- get_artist_audio_features(user_ra_adj$id[j]) %>%
                   dplyr::select(1, 9, 10, 12, 14:19, 30) %>%
                   data.frame(),
                 error = function(e) {skip_to_next <<- TRUE})
        if(skip_to_next){next()} #takes long time
      }
      user_ra_audio  <- dplyr::bind_rows(user_audio, .id = "column.name")
      user_ra_audio <- user_ra_audio %>%
        distinct(track_name, artist_name, .keep_all = TRUE) # duplicate songs with very similar audio features, so they were also distinctly selected
      user_ra_audio <- sample_n(user_ra_audio, 25, replace = TRUE)
      
      tree.preds2 <- ifelse(predict(tree.model2, user_ra_audio)[,2] > 0.5, 1, 0)
      
      predicted_songs <- user_ra_audio[which(tree.preds2 == 1, arr.ind = FALSE, useNames = TRUE),] %>%
        dplyr::select(2, 12)
      colnames(predicted_songs) <- c("artist", "song")
      
      return(predicted_songs)
    }
    n <- reactive({sugg_songs_final(values3$table)})
    output$rec_table <- renderTable({
      if(is.null(input$picker) | is.null(input$search)) {
        return()
      } else{
        n()}
    })
  })
}



# Run the application 
shinyApp(ui = ui, server = server)

