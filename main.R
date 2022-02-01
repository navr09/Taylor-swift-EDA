library(taylor)
library(dplyr)
library(ggplot2)
library(stringr)
library(ggridges)
library(ggthemes)
library(data.table)
library(tidyverse)
library(tidytext)
library(kableExtra)
library(syuzhet)
#install.packages("ggplot2")

head(taylor::taylor_album_songs$lyrics)

album_data <- taylor::taylor_album_songs

album_data$track_release
head(album_data)


pairs(album_data[,c("danceability","energy"
                    ,"loudness","speechiness","acousticness"
                    ,"instrumentalness","liveness","valence")], pch=10
      ,col=factor(album_data$album_name))

# From this we find out that, the columns that influence the valence of albums are 
# danceability, energy, loudness,acousticness and liveness.

pairs(album_data[,c("danceability","energy"
                    ,"loudness","acousticness","liveness","valence")], pch=10
      ,col=factor(album_data$album_name))

library(GGally)
ggpairs(album_data,columns = c("danceability","energy"
                    ,"loudness","acousticness","liveness","tempo","valence"))

# However, we also see that although there is variance, liveness isn't highly correlated with valence,
# hence we can drop this column as well.
# We can conclude that these columns are influential for the valence value

as.numeric(album_data$album_name)
album_data_sortval = arrange(album_data,valence)
album_data_sortval_subset = album_data_sortval[,c("album_name","album_release","track_number"
              ,"track_name","promotional_release","single_release","track_release"
              ,"danceability","energy","loudness","speechiness","acousticness"
              ,"instrumentalness","liveness","valence","tempo","duration_ms")]
album_data_sortval_subset

##### plot of valence over albums

# we consider just the columns which have an impact on valence
analysis_val_albums = album_data_sortval_subset[,c("track_name","album_name","danceability","energy"
                                                   ,"loudness","liveness","valence")]
analysis_val_albums

# Convert album name 1989 to string
analysis_val_albums_conv <- analysis_val_albums %>% 
  mutate("album_name" = str_replace(analysis_val_albums$album_name, "1989", "Nineteen Eighty Nine"))

# Get valence mean across all albums
mean_valenc=tapply( analysis_val_albums_conv$valence, analysis_val_albums_conv$album_name, mean)


analysis_val_albums_conv %>% 
  group_by(analysis_val_albums_conv$album_name) %>% 
  ggplot(aes(x = analysis_val_albums_conv$valence, y = analysis_val_albums_conv$album_name)) + 
  geom_boxplot()

analysis_val_albums_conv %>% 
  group_by(analysis_val_albums_conv$album_name) %>% 
ggplot(aes(x = analysis_val_albums_conv$valence, y = analysis_val_albums_conv$album_name
           , fill = ..x..)) + 
  geom_density_ridges_gradient(scale = 0.9) + 
  scale_fill_gradient(low = "white", high = "maroon3") + 
  theme(panel.background = element_rect(fill = "white")) +
  theme(plot.background = element_rect(fill = "white")) +
  xlim(0,1) +
  theme(legend.position = "none")

# Graph-how many of her album songs have valence above median (Or .5) and how many below.
# reorder graph based on albums release wrt years
# plot valence mean w.r.t years - see if we can use some dynamic graph here


# Lyrics EDA

head(taylor::taylor_album_songs)
lyrics_data <- taylor::taylor_album_songs[,c("album_name","track_name","lyrics")]
lyrics_data <- lyrics_data %>% 
  mutate("album_name" = str_replace(lyrics_data$album_name, "1989", "Nineteen Eighty Nine"))
lyrics_data$album_name
lyrics_data <- lyrics_data %>% 
  mutate("album_name" = str_replace(lyrics_data$album_name,"Fearless (Taylor's Version)", "Fearless"))
lyrics_data$album_name
# Making the lyrics column EDA friendly as it was wrapped in a tibble.
# TODO - Check if the collapse should be a space or a full stop.
lyrics_data <- lyrics_data %>% 
  mutate(lyrics = map_chr(lyrics, ~ .[[2]] %>% str_c(collapse = " ")))

# Getting the total word count of each song
lyrics_data$word_count <- sapply(lyrics_data$lyrics, function(x) length(unlist(strsplit(as.character(x), "\\W+"))))
songwise_wordcount <- lyrics_data
# Getting the total word count across albums
# Plot graph here for lyrics data by joining it with years. Graph: Albums vs word_count 
# in the order of years.
# Plot another graph to show the year wise word count. years vs word_count
# try to get both these graphs in one. Dynamic graphs
lyrics_data_summary <- lyrics_data %>%
  group_by(album_name) %>%
  summarise("Total_Words" = sum(word_count))
# TODO line graph for word count over years along with bar graph for albums(in a year wise manner)


# graphs for the eda so far 
# Stop words parse through
# Sentiment analysis bar plots(explore others)across albums, if that doesn't look good, then keep them at song level.

data(stop_words)

# removing the stop words from the data
tidy_lyrics <- lyrics_data %>%
  unnest_tokens(word, lyrics) %>%
  anti_join(stop_words, by=c("word"="word")) 

# Removing a few more stop-words which aren't there in the library
tidy_lyrics <- tidy_lyrics %>% 
  anti_join(stop_words, by=c("word"="word")) %>% 
  filter(word != "ooh") %>% 
  filter(word != "yeah") %>% 
  filter(word != "ah") %>% 
  filter(word != "uh") %>% 
  filter(word != "ha") %>%
  filter(word != "whoa") %>%
  filter(word != "eh") %>%
  filter(word != "hoo") %>%
  filter(word != "ey") %>%
  filter(word != "mmm") %>% 
  filter(word != "eeh") %>% 
  filter(word != "huh") %>% 
  filter(word != "na")

# Look at the most popular words without removing the stop words
tidy_lyrics %>%
  count(word, sort = TRUE) %>%
  kable(align = "c")%>%
  kable_styling(bootstrap_options = c("striped", "condensed","responsive", "bordered")) %>%
  add_header_above(c("Most popular words in Taylor Swift songs"= 2), bold = TRUE)  %>% 
  scroll_box(width = "500px", height = "400px") 

# TODO plot horizontal bar chart for top 10 words to show how far off it is from the other words.
# In turn suggests the kind of genre she has stuck to across albums. (Love songs majorly)

# Get the words she usually uses to express sentiment 
lyrics_data$lyrics <- as.character(lyrics_data$lyrics)

tidy_lyrics_s2 <- lyrics_data %>%
  unnest_tokens(word,lyrics)

#song_wrd_count <- tidy_lyrics_s2 %>% count(track_title)

#lyric_counts <- tidy_lyrics_s2 %>%
#  left_join(word_count, by = "track_name") %>%
#  rename(total_words=n)

library(textdata)
lyric_sentiment <- tidy_lyrics_s2 %>%
  inner_join(get_sentiments("nrc"),by="word")

lyric_sentiment %>%
  count(word,sentiment,sort=TRUE) %>%
  group_by(sentiment)%>%top_n(n=10) %>%
  ungroup() %>%
  ggplot(aes(x=reorder(word,n),y=n,fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment,scales="free") +
  xlab("Sentiments") + ylab("Scores")+
  ggtitle("Top words used to express emotions and sentiments") +
  coord_flip()

# Here's a list of her go-to words when she wants to show a certain emotion in her songs.

# Next we try to compare albums to see how it looks.
# Deductions on the same
# Finally a correlation between songs and check the words that are highly correlated to tell
# what kind of songs she usually plays(firm conclusion on Love songs is her go-to)

tay_sentiment <- tidy_lyrics_s2%>%
  inner_join(get_sentiments("bing"))%>% 
  count(album_name, track_name, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

tay_sentiment$album_name
tay_sentiment$album_name[tay_sentiment$album_name=="Fearless (Taylor's Version)"]<-"Fearless"
tay_sentiment$album_name[tay_sentiment$album_name=="Nineteen Eighty Nine"]<-"1989"


# Positive sentiment
tay_sentiment_pos <- tay_sentiment %>%                                      # Top N highest values by group
  arrange(desc(sentiment)) %>% 
  group_by(album_name) %>%
  slice(1:3)

# Negative sentiment
tay_sentiment_neg <- tay_sentiment %>%                                      # Top N highest values by group
  arrange((sentiment)) %>% 
  group_by(album_name) %>%
  slice(1:3)

tay_sentiment[tay_sentiment$album_name =='folklore',]
tay_sentiment_neg[tay_sentiment1$album_name =='folklore',]
albumnames <- c("Taylor Swift", "Fearless", "Speak Now", "RED", "1989", "Reputation", "Lover", "Folklore", "Evermore" )



#Plot:
# Top 3 songs of each album with positive sentiment
tay_sentiment_pos%>%
  ggplot(aes(reorder(track_name, sentiment), sentiment, fill = album_name)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~album_name, ncol = 2, scales = "free")+
 # scale_fill_manual(values = c("skyblue1", "lightgoldenrod1", "mediumorchid3", "red2", "plum1", "slategray"))+
  labs(x = NULL,
       y = "Sentiment",
       title = "Top songs ranked by Positive sentiment",
       caption = "                                                                                                                                    Naveen Ramprasad")+
  theme_minimal()+
  theme()+
  coord_flip()

# top 3 songs of each album with negative sentiment
tay_sentiment_neg%>%
  ggplot(aes(reorder(track_name, sentiment), sentiment, fill = album_name)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~album_name, ncol = 2, scales = "free")+
  # scale_fill_manual(values = c("skyblue1", "lightgoldenrod1", "mediumorchid3", "red2", "plum1", "slategray"))+
  labs(x = NULL,
       y = "Sentiment",
       title = "Top songs ranked by Negative sentiment",
       caption = "                                                                                                                                    Naveen Ramrpasad")+
  theme_minimal()+
  theme()+
  coord_flip()
#+  ggsave("sentiment.png", width = 10, height = 5.5)


# -----------------------------------------------------------------------------------
by_word <- lyrics_data %>%
  unnest_tokens(line, lyrics) %>%
  unnest_tokens(word, line)
#get individual word count
count_by_word <- by_word %>% count(word, sort=TRUE) %>%
  arrange(-n)
head(count_by_word)
words <- by_word %>%
  anti_join(stop_words, by = "word")

#count words minus stop words
word_count <- words %>% count(word, sort = TRUE)



#additional stop words to remove
additional_stop_words <- tibble(word = c("ooh", "ah", "yeah", "ya", "la", "na", "da", "uh", "huh", "mm", "mmm", "ha", "em"))
words2 <- words %>% anti_join(additional_stop_words, by = "word")



#word count in descending order
wordcount2 <- words2 %>% count(word, sort =TRUE)
#top 10 most used words
wordcount10 <- wordcount2 %>% top_n(10)
#Bar plot top 10 words
ggplot(wordcount10,mapping=aes(x=reorder(word,-n), y = n)) +
  geom_bar(stat="identity", color = "purple", fill="lightblue") +
  coord_flip() +
  labs(x=NULL, y = "word count") +
  geom_text(aes(label = signif(n, digits = 3)), nudge_y = 8)
  


#word cloud of most common words
library(wordcloud2)
wordcloud <- wordcount2 %>%
  top_n(150) %>%
  wordcloud2(color = "random-light", backgroundColor = "black", )


#---------------------------------------------------------------------------------

wordcount_byalbum <- words2 %>%
  count(album_name, word) %>%
  group_by(album_name)



view(wordcount_byalbum)



#Get wordcount for number of unique words per album
#Album Taylor Swift
ATS <- wordcount_byalbum %>%
  filter(album_name == "Taylor Swift")
ATSC <- nrow(ATS)
#album Fearless
AF <- wordcount_byalbum %>%
  filter(album_name == "Fearless (Taylor's Version)")
AFC<- nrow(AF)
#Album speak now
ASN <- wordcount_byalbum %>%
  filter(album_name == "Speak Now")
ASNC <- nrow(ASN)
#Album red
AR <- wordcount_byalbum %>%
  filter(album_name == "Red")
ARC <- nrow(AR)
#Album 1989
A1989 <- wordcount_byalbum %>%
  filter(album_name == "Nineteen Eighty Nine")
A1989C <- nrow(A1989)
#Album reputation
ARep <- wordcount_byalbum %>%
  filter(album_name == "reputation")
ARepC <- nrow(ARep)
#album lover
AL <- wordcount_byalbum %>%
  filter(album_name == "Lover")
ALC <- nrow(AL)
#Album folklore
AFolk <- wordcount_byalbum %>%
  filter(album_name == "folklore")
AFolkC <- nrow(AFolk)
#Album Evermore
AEv <- wordcount_byalbum %>%
  filter(album_name == "evermore")
AEvC <- nrow(AEv)

#Create data fram of album name and number of unique words in album
albumwords <- c(ATSC, AFC, ASNC, ARC, A1989C, ARepC, ALC, AFolkC, AEvC)
albumnames <- c("Taylor Swift", "Fearless (Taylor's version)", "Speak Now", "RED", "1989", "Reputation", "Lover", "Folklore", "Evermore" )
album_by_words <- cbind(albumnames, albumwords)
album_by_words <- as.data.frame(album_by_words)
album_by_words$albumnames <- factor(album_by_words$albumnames, levels = c("Taylor Swift", "Fearless (Taylor's version)", "Speak Now", "RED", "1989", "Reputation", "Lover", "Folklore", "Evermore" ) )



#Bar plot of number of unique words in each album ordered by album release
ggplot(album_by_words) +
  geom_bar(mapping = aes(x=albumnames, y=albumwords), stat = "identity", color = "Darkblue", fill = "deepskyblue3") +
  plot(x=albumnames, y = albumwords) +
  labs(x="Album Title", y="No. of unique words") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

ggplot(data = album_by_words, aes(x = albumnames, y = albumwords)) + geom_line()

# ---------------------------------------------------------


library(shiny)
library(dplyr)
library(ggplot2)



ui <- fluidPage(
  titlePanel("Taylor album songs"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "Album_name", 
                  label = "Select album name",  # Give the input a label to be displayed in the app
                  choices = (taylor_album_songs$album_name)),
      splitLayout(cellWidths = c("50%", "50%"), plotOutput("distPlot1"), 
                  plotOutput("distPlot2"))
      
    ), 
    mainPanel()
  )
)

server <- function(input, output) {
  data <- reactive({
    req(input$Album_name)
    df <- taylor_album_songs %>% filter(album_name %in% input$Album_name)
  })
  output$distPlot1 <- renderPlot({ggplot(data(), aes(x=album_name, y=valence)) +
      geom_boxplot() + coord_cartesian(ylim = c(0,1)) 
  })
  output$distPlot2 <- renderPlot({ggplot(data(), aes(x=album_name, y=danceability)) + 
      geom_boxplot() + coord_cartesian(ylim = c(0,1))})
  
}  


shinyApp(ui = ui, server = server)

#Plot:
tay_sentiment%>%
  ggplot(aes(reorder(track_name, sentiment), sentiment, fill = album_name)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~album_name, ncol = 3, scales = "free")+
  # scale_fill_manual(values = c("skyblue1", "lightgoldenrod1", "mediumorchid3", "red2", "plum1", "slategray"))+
  labs(x = NULL,
       y = "Sentiment",
       title = "Taylor Swift's songs ranked by sentiment",
       caption = "                                                                                                                                    Ariane Aumaitre - @ariamsita")+
  theme_minimal()+
  theme()+
  coord_flip()
#+  ggsave("sentiment.png", width = 10, height = 5.5)

library(shiny)
library(dplyr)
library(ggplot2)



ui <- fluidPage(
  titlePanel("Taylor album songs"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "Album_name", 
                  label = "Select album name",  # Give the input a label to be displayed in the app
                  choices = (album_name)),
      splitLayout(cellWidths = c("50%", "50%"), plotOutput("distPlot1"), 
                  plotOutput("distPlot2"))
      
    ), 
    mainPanel()
  )
)

server <- function(input, output) {
  data <- reactive({
    req(input$Album_name)
    df <- taylor_album_songs %>% filter(album_name %in% input$Album_name)
  })
  output$distPlot1 <- renderPlot({ggplot(data(), aes(x=sentiment, y=track_name)) +
      geom_boxplot() + coord_cartesian(ylim = c(0,1)) 
  })
  
}  


shinyApp(ui = ui, server = server)