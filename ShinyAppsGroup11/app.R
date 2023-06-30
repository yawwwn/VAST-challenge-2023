
library(shiny)
library(bslib)
library(conflicted)
library(tidyverse)
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::lag)
library(tidytext)
library(ldatuning)
library(wordcloud2)
library(topicmodels)
library(plotly)

options(scipen = 999) 


stopwords_removed <- read.csv("data/stopwords_removed.csv")

MC3_text_pre <- stopwords_removed %>%
  count(id, word) %>%  # count each word used in each identified review 
  cast_dtm(id, word, n) %>%  # use the word counts by reviews  to create a DTM
  as.matrix()


MC3_text <- as.matrix(MC3_text_pre)


# create models with different number of topics
#result <- ldatuning::FindTopicsNumber(
#  MC3_text,
#  topics = seq(from = 2, to = 3, by = 1),
#  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
#  method = "Gibbs",
#  control = list(seed = 77),
#  verbose = TRUE
#)

#FindTopicsNumber_plot(result)

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


# Global variables here

cards3 <- list(

card(full_screen = TRUE, card_header("Select Size of Word Cloud"),
     card_body(numericInput('size', 'Size of wordcloud', value = 1, min = 1, max = 10, step = 1),
               actionButton("run_button1", "Generate"),
               wordcloud2Output('wordcloud2')
     )),

card(full_screen = TRUE, card_header("Select Number of Topic Group"),
     layout_sidebar(
       fillable = TRUE,
       sidebar = sidebar(
         numericInput("topic_group", "Number of Topic Group", value = 6, min = 2, max = 20, step = 1),
         actionButton("run_button", "Generate")
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

  wordcloudinput <- eventReactive(input$run_button1, {
    print("Run button 1 is working!")
    input$size
  })
  
  output$wordcloud2 <- renderWordcloud2({
    wordcloud2(data=stopwords_removed_freq, wordcloudinput())
  })
  
  word_probs <- eventReactive(input$run_button, {
    print("Run button 2 is working!")
    LDA(MC3_text, input$topic_group , method = "Gibbs", control = list(iter = 50, verbose = 25)) %>%
      tidy(matrix = "beta") %>%
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

