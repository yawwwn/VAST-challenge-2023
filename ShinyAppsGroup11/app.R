
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
library(DT)
library(data.table)
library(gt)
library(ggplot2)
library(bulletr)
library(ggthemes)
library(gtExtras)
library(ggiraph)
library(hrbrthemes)
library(svglite)

options(scipen = 999) 


stopwords_removed <- read.csv("data/stopwords_removed.csv")
stopwords_removed_join <- read.csv("data/stopwords_removed_join.csv")
summary_data <- read.csv("data/summary_data.csv")
MC3_nodes_master <- read.csv("data/MC3_nodes_master.csv")
MC3_nodes_master_revenue <- read.csv("data/MC3_nodes_master_revenue.csv")
nodes_df2 <- read_rds("data/nodes_df2.rds")
mc3_edges_country <- read_rds("data/mc3_edges_country.rds")
edges_df2 <- read_rds("data/edges_df2.rds")



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


# Card 1 List here --------------------------------->

cards1 <- list(
  card(
    full_screen = TRUE,
    card_header("Counts by Country"),
    plotOutput("company_plot")
  ),
  card(
    full_screen = FALSE,
    card_header("Company Data Table"),
    DT:: dataTableOutput("company_dt", width = "100%", height = "auto")
  ),
  card(
    full_screen = FALSE,
    card_header("Topics Average Revenue"),
    gt_output("bullet_topics")
  )
)



##Parent Value_box 1 set ----------------------->

value_box_1_1 <- value_box(
  title = "Median of the Number of Companies Owned per Person",
  value = "2 Companies",
  showcase = bsicons::bs_icon("align-bottom")
)

value_box_1_2 <- value_box(
  title = "Median of the Number of Companies Contacts per Person",
  value =  "1 Company",
  showcase = bsicons::bs_icon("align-center"),
  theme_color = "dark")

value_box_1_3 <- value_box(
  title = "Median Revenue per Person",
  value = "$25,000",
  showcase = bsicons::bs_icon("handbag"),
  theme_color = "secondary")

# Card 3 List here ----------------------------------->

cards3 <- list(

card(full_screen = TRUE, card_header("Select Size of Word Cloud"),
     layout_sidebar(
       fillable = TRUE,
       sidebar = sidebar(
            numericInput('size', 'Size of wordcloud', value = 1, min = 1, max = 10, step = 1),
            actionButton("run_button1", "Generate"),
            ), card_body(wordcloud2Output('wordcloud2')
     ))),

card(full_screen = TRUE, card_header("Select Number of Topic Group"),
     layout_sidebar(
       fillable = TRUE,
       sidebar = sidebar(
         numericInput("topic_group", "Number of Topic Group", value = 6, min = 2, max = 20, step = 1),
         actionButton("run_button", "Generate")
       ), card_body(plotlyOutput("myplot"))))
  )



# Define the UI ===================================================>
ui <- navbarPage(
  title = "Group 11 VAA Project",
  theme = bs_theme(version = 5, bootswatch = "minty"),
  
  #UI First Panel #1 begin --------------------------------------------->
  
  # First panel tab
  tabPanel(
    "Dashboard",
    layout_columns(
      fill = FALSE,
      col_widths = c(4, 4, 4, 4, 4),
      value_box_1_1, value_box_1_2, value_box_1_3, cards1[[1]], cards1[[2]], cards1[[3]]),
    ),
  
  #UI Second Panel #2 begin --------------------------------------------->
  
  # Second panel tab
  tabPanel(
    "Network Analysis",
    fluidPage(
      plotOutput("")
    )
  ),
  
  
  #UI Third Panel #3 begin --------------------------------------------->
  
  # Third panel tab
  tabPanel(
    "Topic Analysis",
    navset_tab(
      id = "myNavset",
      nav_panel(title = "Wordcloud",
                layout_columns(
                  cards3[[1]]
        )),
      nav_panel(title = "Select the Number of Topics",
                  layout_columns(
                   cards3[[2]]
               )
              )
           )
    )
  )



# Define the server code =======================================>
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
  

  # Company plot
  output$company_plot <- renderPlot({
    mc3_nodes1_Other <- MC3_nodes_master %>%
      filter(!is.na(country)) %>%
      group_by(country) %>%
      summarise(counts = n()) %>%
      arrange(desc(counts)) %>%
      mutate(country = ifelse(row_number() > 3, "Other", country)) %>%
      group_by(country) %>%
      summarise(counts = sum(counts)) %>%
      mutate(counts = as.numeric(counts)) %>%
      arrange(desc(counts)) 
    
    ggplot(mc3_nodes1_Other, aes(x = reorder(country, -counts), y = counts)) +
      geom_bar(stat = "identity", fill = '#3498db') +
      geom_text(aes(label = format(counts, big.mark = ",")), vjust = -0.5) +
      theme_minimal() +
      labs(x = "Country", y = "Counts",
           title = 'Count of Nodes by Country',
           subtitle = 'ZH, Oceanus, and Marebak were the top 3 countries where most nodes domiciled') +
      theme(axis.title.y = element_text(angle = 0, vjust = 0.5, hjust = 1))
  })
  
  
  #Revenue plot (KIV)
#  output$revenue_plot <- renderPlot({
#    MC3_nodes_master %>%
#      filter(!is.na(country)) %>%
#      group_by(country) %>%
#      summarise(revenue_omu = sum(revenue_omu)) %>%
#      arrange(desc(revenue_omu)) %>%
#      mutate(country = ifelse(row_number() > 3, "Other", country)) %>%
#      group_by(country) %>%
#      summarise(revenue_omu = sum(revenue_omu)) %>%
#      ggplot(aes(x = reorder(country, -revenue_omu), y = revenue_omu)) +
#      geom_bar(stat = "identity", fill = '#3498db') +
#      geom_text(aes(label = format(revenue_omu, big.mark = ",")), vjust = -0.5) +
#      scale_y_continuous(labels = function(x) format(x/1000000, nsmall = 1, big.mark = ".", decimal.mark = ",")) +
#      theme_minimal() +
#      labs(x = "Country", y = "Revenue in mil",
#           title = 'Distribution of Revenue by Country',
#           subtitle = 'ZH, Rio Isla, and Utoporiana were the top 3 countries where most revenue was generated') +
#      theme(axis.title.y = element_text(angle = 90, vjust = 0.5, hjust = 1))
#  })
  
  
  #Company Data table
  output$company_dt <- DT:: renderDataTable({
    # Create a data table from the df data frame
    dt <- data.table(MC3_nodes_master)
    
    
    # Create a new data table with the count of types for each id
    MC3_nodes_master_count <- dt[, .N, by = c("id", "type")]
    
    # Reshape the data table to have type values as columns and group by ID in rows
    wide_dt <- dcast(MC3_nodes_master_count, id ~ type, value.var = "N")
    
    # Replace NA values with 0
    wide_dt[is.na(wide_dt)] <- 0
    
    
    wide_dt[, N := rowSums(.SD), .SDcols = -"id"]
    
    wide_dt <- wide_dt[order(-N)]
    wide_dt[, "NA" := NULL]
    datatable(wide_dt)
  })
  
  #topic Revenue bullet graph 

  output$bullet_topics <- render_gt({
    stopwords_removed_join %>%
      group_by(`topic`) %>%
      summarise('Average Revenue of Each topic vs Global Average' = mean(revenue_omu)) %>%
      mutate(target = mean(stopwords_removed_join$revenue_omu)) %>%
      gt::gt() %>%
      gt_plt_bullet(column = 'Average Revenue of Each topic vs Global Average', 
                    target = target, 
                    width = 60,
                    palette = c("lightblue", 
                                "black")) %>%
      tab_header(title = "Comparison of average revenue of each topic vs. all topics") %>%
      gt_theme_538()
  })
}


# Return a Shiny app object

shinyApp(ui = ui, server = server)

