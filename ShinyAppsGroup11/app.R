
library(shiny)
library(bslib)
library(tidyverse)
library(jsonlite)
library(tidytext)
library(ldatuning)
library(wordcloud2)
library(topicmodels)
library(plotly)
library(forcats)

options(scipen = 999) 

#importing the data 
MC3_challenge <- fromJSON("data/MC3.json")

#extracting edges 
MC3_edges <-as_tibble(MC3_challenge$links) %>%
  distinct() %>%
  mutate(source = as.character(source),
         target = as.character(target),
         type = as.character(type)) %>%
  group_by(source,target, type) %>%
  summarise(weights = n()) %>%
  filter (source != target) %>%
  ungroup()

#extrading nodes 
MC3_nodes <-as_tibble(MC3_challenge$nodes) %>%
  mutate(country = as.character(country),
         id = as.character(id),
         product_services = as.character(product_services),
         revenue_omu = as.numeric(as.character(revenue_omu)),
         type = as.character(type)) %>%
  select(id,country,type,revenue_omu,product_services)

#default masterlist 
id1 <- MC3_edges %>%
  select(source) %>%
  rename(id = source)
id2 <- MC3_edges %>%
  select(target) %>%
  rename(id = target)
MC3_nodes_master <- rbind(id1, id2) %>%
  distinct() %>%
  left_join(MC3_nodes,
            unmatched = "drop")

#create new node df to include id number
MC3_nodes_Masterlist <- MC3_nodes_master %>%
  select(id) %>%
  distinct() %>%
  rename(label = id) %>%
  ungroup()

#add ID to nodes dataframe
MC3_masternodes <- MC3_nodes_Masterlist %>%
  mutate(id = as.character(1:nrow(MC3_nodes_Masterlist))) %>%
  relocate(id,label) %>%
  ungroup()

#to append correspoinding id through left_join 
MC3_edges_addID <- MC3_edges %>%
  rename(sourcelabel = source, targetlabel = target) %>%
  left_join(MC3_masternodes, by = c("sourcelabel" = "label")) %>%
  rename(source = id) %>%
  left_join(MC3_masternodes, by = c("targetlabel" = "label")) %>%
  rename(target = id) %>%
  relocate(source,target)

#word related code from this line
#unnest words
token_nodes <- MC3_nodes %>%
  unnest_tokens(word, product_services)

#remove stop_words
stopwords_removed <- token_nodes %>% 
  anti_join(stop_words)

#remove generic words 
remove_characters <- c("character", "0","unknown","products","services",
                       "including", "source", "offers","range", "related")

#create dataframe of each word with frequency 
stopwords_removed_freq <- stopwords_removed %>%
  filter(!word %in% remove_characters) %>%
  group_by(word) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  ungroup()

#generate word cloud 
set.seed(1234) #for reproductibility
wordcloud2(data=stopwords_removed_freq, size=1.6, color='random-dark')

token_nodes$word[token_nodes$word == "character"] <- "NA"
token_nodes$word[token_nodes$word == "0"] <- "NA"

#remove stop_words
stopwords_removed <- token_nodes %>% 
  anti_join(stop_words) %>%
  filter(!word %in% c("NA", "unknown", "products"))

dim(stopwords_removed)

stopwords_removed %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in product_services field")

# using as.matrix()
MC3_text <- stopwords_removed %>%
  count(id, word) %>%  # count each word used in each identified review 
  cast_dtm(id, word, n) %>%  # use the word counts by reviews  to create a DTM
  as.matrix()

# create models with different number of topics
result <- ldatuning::FindTopicsNumber(
  MC3_text,
  topics = seq(from = 2, to = 20, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  verbose = TRUE
)

FindTopicsNumber_plot(result)

# Global variables here
n <- 1


cards3 <- list(

card(full_screen = TRUE, card_header("Word Cloud"),
     card_body(numericInput('size', 'Size of wordcloud', n), wordcloud2Output('wordcloud2')
     )),

card(full_screen = TRUE, card_header("1) Select Number of Topic Group"),
     layout_sidebar(
       fillable = TRUE,
       sidebar = sidebar(
         numericInput("topic_group", "Number of Topic Group", value = 4, min = 2, max = 20, step = 1),
         actionButton("run_button", "Generate Result")
       ), card_body(plotlyOutput("myplot"))))
)


# Define the UI
ui <- navbarPage(
  title = "Group 11 VAA Project",
  theme = bs_theme(version = 5, bootswatch = "minty"),
  
  # First panel tab
  tabPanel(
    "Dashboard",
    fluidPage(
      plotOutput("")
    )
  ),
  # Second panel tab
  tabPanel(
    "Network Analysis",
    fluidPage(
      plotOutput("")
    )
  ),
  
  #Cards #3 begin ----------------------------------------------
  
  # Third panel tab
  tabPanel(
    "Topic Analysis",
    layout_columns(
      fill = FALSE,
      col_width = c(12, 12),
      row_heights = c(1.5, 1.5),
      cards3[[1]],
      cards3[[2]]
    )
  )
)


# Define the server code
server <- function(input, output) {
  set.seed(1234)  # Set the seed to 1234

  output$wordcloud2 <- renderWordcloud2({
    wordcloud2(data=stopwords_removed_freq, input$size)
  })
  
  topicModel <- eventReactive(input$run_button, {
    print("Run button is working!")
    LDA(MC3_text, input$topic_group , method = "Gibbs", control = list(iter = 500, verbose = 25))
  })
  
  lda_topics <- reactive({
    topicModel() %>%
      tidy(matrix = "beta")
  })
  
  word_probs <- reactive({
    lda_topics() %>%
      group_by(topic) %>%
      top_n(15, beta) %>%
      ungroup() %>%
      mutate(term2 = fct_reorder(term, beta))
  })
  
  output$myplot <- renderPlotly({
    ggplot(word_probs(), aes(term2, beta, fill = as.factor(topic))) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~ topic, scales = "free") +
      coord_flip()
  })
}


# Return a Shiny app object

shinyApp(ui = ui, server = server)

