#-------------------------------------------> Must have packages
library(shiny)
library(bslib)
library(conflicted)
library(tidyverse)
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::lag)
library(tidytext)
library(wordcloud2)
library(topicmodels)
library(plotly)
library(DT)
library(data.table)
library(gt)
library(bulletr)
library(ggthemes)
library(gtExtras)
library(ggiraph)
library(hrbrthemes)
library(svglite)
conflicts_prefer(plotly::layout)
library(visNetwork)
library(heatmaply)
library(igraph)
library(scales)
library(lobstr)
library(ggstatsplot)
library(ggdist)
library(PMCMRplus)
library(shinyWidgets)
library(bsicons)
#-------------------------------------------> Must have packages

print(mem_used())
options(scipen = 999) #disables scientific notation


stopwords_removed <- read.csv("data/stopwords_removed.csv")
stopwords_removed_join <- read.csv("data/stopwords_removed_join.csv") # for K = 7
summary_data <- read.csv("data/summary_data.csv")
MC3_nodes_master <- read.csv("data/MC3_nodes_master.csv")
MC3_nodes_master_revenue <- read.csv("data/MC3_nodes_master_revenue.csv")
nodes_df2 <- read_rds("data/nodes_df2.rds")
mc3_edges_country <- read_rds("data/mc3_edges_country.rds")
edges_df2 <- read_rds("data/edges_df2.rds")
company_bytopic <- read_rds("data/company_bytopic.rds")


MC3_text_pre <- stopwords_removed %>%
  count(id, word) %>%  # count each word used in each identified review 
  cast_dtm(id, word, n) %>%  # use the word counts by reviews  to create a DTM
  as.matrix()


MC3_text <- as.matrix(MC3_text_pre)

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


#-----------Dashboard Data table prep----------------------------->
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

#===========Dashboard Data table prep----------------------------->
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


#===========Dashboard 4 Data prep----------------------------->>

nodes_df2_topic <- nodes_df2 %>%
  left_join(company_bytopic, by = c("name" = "id"), unmatched= "drop")%>%
  mutate(topic_clean = ifelse(is.na(topic),"unknown",topic)) %>%
  select(-type.y)%>%
  rename(type = type.x)

# Function to get connected nodes up to a specified degree
getConnectedNodes <- function(nodeId, degree) {
  connectedNodes <- data.frame(id = nodeId, degree = 0)  # Initialize with the input node and degree 0
  visitedNodes <- data.frame(id = nodeId, degree = 0)  # Keep track of visited nodes
  
  for (i in 1:degree) {
    fromNodes <- unique(edges_df2$from[edges_df2$to %in% connectedNodes$id])
    toNodes <- unique(edges_df2$to[edges_df2$from %in% connectedNodes$id])
    
    newNodes <- unique(c(fromNodes, toNodes))
    newDegrees <- rep(i, length(newNodes))
    
    newNodes_df <- data.frame(id = newNodes, degree = newDegrees)
    newNodes_df <- newNodes_df[!newNodes_df$id %in% visitedNodes$id, ]  # Exclude visited nodes
    
    visitedNodes <- rbind(visitedNodes, newNodes_df)  # Add newly visited nodes
    
    connectedNodes <- rbind(connectedNodes, newNodes_df)  # Add newly connected nodes
  }
  
  # Keep only the nodes with the lowest degree
  connectedNodes <- connectedNodes[!duplicated(connectedNodes$id), ]
  
  return(connectedNodes)
}

# Card 1 List here --------------------------------->

cards1 <- list(
  card(
    full_screen = TRUE,
    card_header("Counts by Country"),
    plotOutput("company_plot", height = "400px", width = "auto")
  ),
  card(
    full_screen = TRUE,
    card_header("Company Data Table"),
    DT:: dataTableOutput("company_dt", width = "auto", height = "auto")
  ),
  
  card(
    full_screen = TRUE,
    card_header("Word Cloud and Bullet Graph of Topic Revenue"),
    card_body(
      fluidRow(
        column(width = 12, wordcloud2Output("wordcloud2", height = "200px", width = "auto")),
        column(width = 12, gt_output("bullet_topics"))
     )
    )
  )
)
  
#  card(full_screen = TRUE, card_header("Select Size of Word Cloud"),
#       layout_sidebar(
#         fillable = TRUE,
#         sidebar = sidebar(
#           width = 100,
#           numericInput('size', 'Size of wordcloud', value = 1, min = 1, max = 10, step = 1),
#           actionButton("run_button1", "Go"),
#         ), card_body(wordcloud2Output('wordcloud2')
#         ))),
#  card(
#    full_screen = TRUE,
#    card_header("Topics Average Revenue"),
#    gt_output("bullet_topics")
  




##Value_box 1 set ----------------------->

value_box_1_1 <- value_box(
  title = "Median of the Number of Companies Owned per Person",
  value = "2 Companies",
  showcase = bsicons::bs_icon("align-bottom"),
  theme_color = "info")

value_box_1_2 <- value_box(
  title = "Median of the Number of Companies Contacts per Person",
  value =  "1 Company",
  showcase = bsicons::bs_icon("align-center"),
  theme_color = "primary")

value_box_1_3 <- value_box(
  title = "Median Revenue per Person",
  value = "$25,000",
  showcase = bsicons::bs_icon("handbag"),
  theme_color = "success")

# Card 2 List here --------------------------------->

cards2 <- list(
  card(
    full_screen = TRUE,
    card_header("Network Graph of Top Company Ownership and Contacts"),
    visNetworkOutput("networkPlot_owner")
  ),
  card(
    full_screen = TRUE,
    card_header("Total Revenue from respective companies owned by Top Owners"),
    plotlyOutput("barPlot_owner_rev")
  ),
  card(
    full_screen = TRUE,
    card_header("Revenue Comparison of Top Owners"),
    plotlyOutput("boxPlot_top_owner")
  ),
  card(
    full_screen = TRUE,
    card_header("Revenue Comparison of ALL Owners"),
    plotlyOutput("boxPlot_all_owner")
  ),
  card(
    full_screen = TRUE,
    card_header("Country Distribution of companies owned"),
    plotlyOutput("heatmap_owner")
  ),
  card(
    full_screen = TRUE,
    card_header("Network Graph of Top Companies and their owners"),
    visNetworkOutput("networkPlot_company")  
  ),  card(
    full_screen = TRUE,
    card_header("Total Revenue from Top Companies"),
    plotlyOutput("barPlot_company")
  ),
  card(
    full_screen = TRUE,
    card_header("Revenue Comparison of Top Companies"),
    plotlyOutput("boxPlot_top_co")
  ),
  card(
    full_screen = TRUE,
    card_header("Revenue Comparison of ALL Companies"),
    plotlyOutput("boxPlot_all_co")
  ),
  card(
    full_screen = TRUE,
    card_header("Country Distribution of Top Companies"),
    plotlyOutput("barPlot_co_country")
  ),
  card(
    full_screen = TRUE,
    card_header("Total of number of companies in the related Communities involved"),
    plotlyOutput("barPlot_community")
  )
)


# Card 3 List here ----------------------------------->

cards3 <- list(
  card(full_screen = TRUE, card_header("Select Number of Topic Group"),
     layout_sidebar(
       width = 215,
       fillable = TRUE,
       sidebar = sidebar(
         numericInput("topic_group", "Number of Topic Group", value = 7, min = 2, max = 20, step = 1),
         actionButton("run_button", "Generate")
       ), card_body(plotlyOutput("myplot"))
     )
    ),
  card(
    full_screen = TRUE,
    card_header("ANOVA Test"),
    card_body(
      plotOutput("statplot1"),
      markdown("Since p < 0.05, reject the null that revenue across Topic Groups are similar"
    ))),
  card(
    full_screen = TRUE,
    card_header("Confidence Interval"),
    plotOutput("statplot2")
  )
)


# Card 4 List here -------------------------------->

cards4 <- list(
  card(
    full_screen = TRUE,
    card_header("Network Graph"),
    visNetworkOutput("networkPlot"#, height = "500px"))
    )
  )
)

# Value Box Set for the 4th panel here -------------------------------->


value_box_4_1 <-
  value_box(
    title = "Total Revenue (OMU)", 
    value = textOutput("total_revenue_omu"),
    showcase = bs_icon("cash-coin"),
    p("Total revenue of companies involved within network")
  )

value_box_4_2 <-  value_box(
    title = "Highest Associated Topic#", 
    value = textOutput("topic"),
    showcase = bs_icon("water"),
    p("Based on number of topics K=7"),
    theme_color = "info"
  )

value_box_4_3 <-  value_box(
    title = "Number of Topics Involved", 
    value = textOutput("no_of_topic"),
    showcase = bs_icon("box-seam-fill"),
    p("Number of topics involved with the network (based on K=7)"),
    theme_color = "info"
  )

value_box_4_4 <-  value_box(
    title = "Top Community Group", 
    value = textOutput("community"),
    showcase = bs_icon("people-fill"),
    p("Community Group with the most participants"),
    theme_color = "success"
  )



# Define the UI ===================================================>
ui <- navbarPage(
  title = "Group 11 VAA Project",
  theme = bs_theme(version = 5, bootswatch = "minty"),
  tags$head(tags$style(".shiny-notification {position: fixed; top: 24% ;left: 20%}")),

  #UI First Panel #1 begin --------------------------------------------->
  
  # First panel tab
  tabPanel(
    "Dashboard",
    layout_columns(
      fill = FALSE,
      col_widths = c(4, 4, 4, 4, 4, 4),
      value_box_1_1, value_box_1_2, value_box_1_3, cards1[[1]], cards1[[2]], cards1[[3]])
    ),
  
  #UI Second Panel #2 begin --------------------------------------------->
  
  # Second panel tab
  tabPanel(
    "Network Analysis",
    layout_sidebar(
      sidebar = sidebar(
       p(style = "font-family: Arial; font-style: italic; font-size: 18px;", "Narrow down nodes by centrality measures:"),
       selectInput("centrality", "Choose Centrality Measure:",
                  choices = c("betweenness centrality", "degree centrality"),
                  selected = "betweenness centrality"),
       sliderInput("topN", "Select Top N Nodes:", min = 1, max = 100, value = 5),
       tags$hr(),  # Add a horizontal line as a divider
       p(style = "font-family: Arial; font-style: italic; font-size: 18px;", "Additional Filters"),
       selectInput("nodeType", "Filter by Type:",
                  choices = c("All", "Beneficial Owner + Company Contact", "Beneficial Owner", "Company Contact", "Company"),
                  selected = "All"),
        radioButtons("colorOption", "Visualise nodes by:",
                   choices = c("View by Community", "View by Type"),
                   selected = "View by Community"),
       checkboxInput("CountryToggle", "Enable Graph Select by Country", value = FALSE)
      ),
    navset_tab(
      id = "myNavset",
      nav_panel(title = "Owners & Contacts",
                layout_column_wrap(
                  width = "250px",
                  fill = FALSE
                ),
                cards2[[1]],
                layout_columns(
                  cards2[[2]], cards2[[5]]
                ),
                layout_columns(
                  cards2[[3]], cards2[[4]], cards2[[11]]
                )
      ),
      nav_panel(title = "Companies",
                layout_column_wrap(
                  width = "250px",
                  fill = FALSE
                ),
                cards2[[6]],
                layout_columns(
                  cards2[[7]], cards2[[10]]
                ),
                layout_columns(
                  cards2[[8]], cards2[[9]]
                )
       )
     )
    )
  ),
  
  
  #UI Third Panel #3 begin --------------------------------------------->
  
  # Third panel tab
  tabPanel(
    "Topic Analysis",
    layout_columns(fill = FALSE,
                   col_widths = c(12, 6, 6),
                   cards3[[1]],cards3[[2]],cards3[[3]])
                  
            ),

#UI 4th Panel #4 begin --------------------------------------------->

  tabPanel(
  "Deep-Dive Investigation",
  layout_sidebar(
    sidebar = sidebar(
      pickerInput(
        inputId = "searchNode",
        label = "Search Names:",
        selected = "John Smith",
        choices = sort(nodes_df2$name),
        options = list(`live-search` = TRUE),
        multiple = FALSE
      ),
      sliderInput("degree", "Nth Degree of relationship:", min = 1, max = 10, value = 2),
      tags$hr(),  # Add a horizontal line as a divider
      p(style = "font-family: Arial; font-style: italic; font-size: 12px;", "Graph Filters"),
      radioButtons("colorOption2", "Visualise nodes by:",
                   choices = c("View by Community", "View by Type", "View by Degree"),
                   selected = "View by Community"),
      p(style = "font-family: Arial; font-size: 12px;", "Layout:"),
      checkboxInput("layoutToggle", "Hierarchical Layout", value = FALSE),
      checkboxInput("physicsToggle", "Enable Physics", value = FALSE)
    ),
    layout_columns(
    fill = FALSE,
    col_widths = c(3,3,3,3,12),
    value_box_4_1, value_box_4_2, value_box_4_3, value_box_4_4, cards4[[1]])
 #   layout_column_wrap(
#      width = "250px",
#     fill = FALSE,
#      !!!vbs4
    ),
 #   !!!cards4
  )
)
  

# Define the server code =======================================>
server <- function(input, output) {
  set.seed(1234)  # Set the seed to 1234
  
  
  output$wordcloud2 <- renderWordcloud2({
    set.seed(1234)  # Set the seed to 1234
    wordcloud2(data=stopwords_removed_freq, 1)
  })
  

  # Company plot
  output$company_plot <- renderPlot({
    
    
    dat <- data.frame(x = numeric(0), y = numeric(0))
    
    withProgress(message = 'Making plot', value = 0, {
      # Number of times we'll go through the loop
      n <- 10
      
      for (i in 1:n) {
        # Each time through the loop, add another row of data. This is
        # a stand-in for a long-running computation.
        dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
        
        # Increment the progress bar, and update the detail text.
        incProgress(1/n, detail = paste("Doing part", i))
        
        # Pause for 0.1 seconds to simulate a long computation.
        Sys.sleep(0.1)
      }
    })
    
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
    # Update the code to display only the top 10 rows
    datatable(wide_dt[1:10,]) 
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
  
  output$networkPlot_owner <- renderVisNetwork({
    
    
    dat2 <- data.frame(x = numeric(0), y = numeric(0))
    
    withProgress(message = 'Making plot', value = 0, {
      # Number of times we'll go through the loop
      n <- 10
      
      for (i in 1:n) {
        # Each time through the loop, add another row of data. This is
        # a stand-in for a long-running computation.
        dat2 <- rbind(dat2, data.frame(x = rnorm(1), y = rnorm(1)))
        
        # Increment the progress bar, and update the detail text.
        incProgress(1/n, detail = paste("Doing part", i))
        
        # Pause for 0.1 seconds to simulate a long computation.
        Sys.sleep(0.6)
      }
    })
    
    
    filteredNodes <- nodes_df2[!is.na(nodes_df2$new_type), ]
    centralityMeasure <- input$centrality
    topN <- input$topN
    nodeType <- input$nodeType
    colorOption <- input$colorOption
    
    # Filter nodes based on centrality measure
    filteredNodes <- filteredNodes[order(-filteredNodes[[centralityMeasure]]), ]
    
    # Filter nodes based on type
    if (nodeType != "All") {
      filteredNodes <- filteredNodes[filteredNodes$new_type == nodeType, ]
    }
    
    # Add color and shape properties based on the selected option
    if (colorOption == "View by Community") {
      nodes_df2$group <- nodes_df2$community  # Assuming you have a 'community' column in 'nodes_df2'
    } else if (colorOption == "View by Type") {
      nodes_df2$shape <- "icon"
      nodes_df2$icon.code <- ifelse(is.na(nodes_df2$new_type), "f1ad", "f007")
      nodes_df2$icon.color <- ifelse(is.na(nodes_df2$new_type),"#116A7B" ,"#FF2171")
    }
    
    # Get the top N nodes and their connected nodes
    topNodes <- filteredNodes[1:min(topN, nrow(filteredNodes)), ]
    connectedNodeIds <- unique(c(
      edges_df2$from[edges_df2$to %in% topNodes$id],
      edges_df2$to[edges_df2$from %in% topNodes$id],
      topNodes$id
    ))
    selectedNodes <- nodes_df2[nodes_df2$id %in% connectedNodeIds, ]
    selectedEdges <- edges_df2[
      edges_df2$from %in% connectedNodeIds & edges_df2$to %in% connectedNodeIds,
    ]
    
    
    
    vis <- visNetwork(
      nodes = selectedNodes,
      edges = selectedEdges
    ) %>%
      visIgraphLayout(layout = "layout_with_fr", type = "full", smooth = TRUE) %>%
      visEdges(color = list(highlight = "#7C3238"), width = 4, arrows = "from") %>%
      visNodes(
        borderWidth = 1,
        shadow = TRUE,
      ) %>%
      visInteraction(dragNodes = TRUE, 
                     dragView = TRUE, 
                     zoomView = TRUE) %>%
      visLayout(randomSeed = 123)%>%
      addFontAwesome()%>%
      visLegend(
        addEdges = data.frame(
          label = c("Beneficial Owner", "Company Contact"),
          color = c("#9BABB8", "#3E7C59"),
          stringsAsFactors = FALSE,
          font.align = "top"
        ),
        ncol = 3,
        width = 0.1
      ) 
    
    if (input$CountryToggle) {
      vis <- vis %>%
        visOptions(
          highlightNearest = TRUE,
          nodesIdSelection = TRUE,
          selectedBy = "country")
    } else {
      vis <- vis %>%
        visOptions(
          highlightNearest = TRUE,
          nodesIdSelection = TRUE,
          selectedBy = "community")
      vis
    }
    
    
    
  })
  
  output$barPlot_owner_rev <- renderPlotly({
    filteredNodes <- nodes_df2[!is.na(nodes_df2$new_type), ]
    centralityMeasure <- input$centrality
    topN <- input$topN
    nodeType <- input$nodeType
    
    # Filter nodes based on centrality measure
    filteredNodes <- filteredNodes[order(-filteredNodes[[centralityMeasure]]), ]
    
    # Filter nodes based on type
    if (nodeType != "All") {
      filteredNodes <- filteredNodes[filteredNodes$new_type == nodeType, ]
    }
    
    # Get the top N nodes
    topNodes <- filteredNodes[1:min(topN, nrow(filteredNodes)), ]
    # Create a new column with reordered names
    #topNodes$Name <- reorder(topNodes$name, -topNodes$total_revenue_omu)
    
    
    # Create bar plot
    plot_ly(topNodes, x = topNodes$name, y = topNodes$total_revenue_omu, type = "bar") %>%
      layout(xaxis = list(title = "Owners & Contacts",
                          tickangle = -45),
             yaxis = list(title = "Total Revenue (OMU)"),
             hoverlabel = list(namelength = -1),
             showlegend = FALSE)
    
#    p <- ggplot(topNodes, aes(x = name,
#                              y = total_revenue_omu)) +
#      geom_col_interactive(aes(tooltip = name)) +
#      geom_bar(stat = "identity", fill = "steelblue") +
#      labs(x = NULL, y = NULL) +
#      theme_minimal() +
#      xlab("Owners & Contacts") +  # Add x-axis title
#      ylab("Total Revenue (OMU)") +  # Add y-axis title
#      theme_minimal() +
#      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5, size = 8),
#            panel.grid.major.x = element_blank(),
#            panel.grid.minor.x = element_blank()) +
 #     scale_y_continuous(labels = comma)  # Add numerical scaling for y-axis 
#    
#    ggplotly(p)
  })
  
  #heatmap
  output$heatmap_owner <- renderPlotly({
    filteredNodes <- nodes_df2[!is.na(nodes_df2$new_type), ]
    centralityMeasure <- input$centrality
    topN <- input$topN
    nodeType <- input$nodeType
    
    # Filter nodes based on centrality measure
    filteredNodes <- filteredNodes[order(-filteredNodes[[centralityMeasure]]), ]
    
    # Filter nodes based on type
    if (nodeType != "All") {
      filteredNodes <- filteredNodes[filteredNodes$new_type == nodeType, ]
    }
    
    # Get the top N nodes
    topNodes <- filteredNodes[1:min(topN, nrow(filteredNodes)), ]
    
    filtered_edges <- mc3_edges_country[mc3_edges_country$to %in% topNodes$name, ]
    
    # Count the number of transactions per month and year
    transactions_count <- filtered_edges %>%
      mutate(country = replace(country, is.na(country), "unknown"))%>%
      group_by(to, country) %>%
      summarize(count = n())
    
    heatmap_data <- transactions_count %>%
      pivot_wider(names_from = to, values_from = count, values_fill = 0) 
    
    row.names(heatmap_data) <- heatmap_data$country
    
    # Convert the data to a matrix using data.matrix
    heatmap_matrix <- data.matrix(heatmap_data)
    
    # Create a separate matrix or data frame with custom tooltip text
    #tooltip_text <- data.matrix(paste("Year:", colnames(heatmap_data)[-1], "\nCountry:", heatmap_data$Country))
    
    # Create the interactive heatmap using heatmaply
    heatmaply(heatmap_matrix[, -c(1)], #remove first column
              dendrogram = "none", #remove cluster
              colors = Blues,
              margins = c(NA,200,60,NA),
              fontsize_row = 6,
              fontsize_col = 6,
              #main="Country Distribution for Top Owners",
              xlab = "Owners & Contacts",
              ylab = "Countries involved",
    )
  }
  )
  
  
  output$boxPlot_top_owner <- renderPlotly({
    filteredNodes <- nodes_df2[!is.na(nodes_df2$new_type), ]
    centralityMeasure <- input$centrality
    topN <- input$topN
    nodeType <- input$nodeType
    
    # Filter nodes based on centrality measure
    filteredNodes <- filteredNodes[order(-filteredNodes[[centralityMeasure]]), ]
    
    # Filter nodes based on type
    if (nodeType != "All") {
      filteredNodes <- filteredNodes[filteredNodes$new_type == nodeType, ]
    }
    
    # Get the top N nodes
    topNodes <- filteredNodes[1:min(topN, nrow(filteredNodes)), ]
    
    # Create a box plot for total_revenue_omu using plotly
    plot_ly(topNodes, x = "", y = ~total_revenue_omu, type = "box") %>%
      layout(xaxis = list(title = ""),
             yaxis = list(title = "Total Revenue (OMU)"),
             hoverlabel = list(namelength = -1),
             showlegend = FALSE)
  })
  
  output$boxPlot_all_owner <- renderPlotly({
    filteredNodes <- nodes_df2[!is.na(nodes_df2$new_type), ]
    centralityMeasure <- input$centrality
    topN <- input$topN
    nodeType <- input$nodeType
    
    # Filter nodes based on centrality measure
    filteredNodes <- filteredNodes[order(-filteredNodes[[centralityMeasure]]), ]
    
    # Filter nodes based on type
    if (nodeType != "All") {
      filteredNodes <- filteredNodes[filteredNodes$new_type == nodeType, ]
    }
    
    # Get the top N nodes
    topNodes <- filteredNodes[1:min(topN, nrow(filteredNodes)), ]
    
    # Create a box plot for total_revenue_omu using plotly
    plot_ly(filteredNodes, x = "", y = ~total_revenue_omu, type = "box") %>%
      layout(xaxis = list(title = ""),
             yaxis = list(title = "Total Revenue (OMU)"),
             hoverlabel = list(namelength = -1),
             showlegend = FALSE)
  })
  
  output$barPlot_community <- renderPlotly({
    filteredNodes <- nodes_df2[!is.na(nodes_df2$new_type), ]
    centralityMeasure <- input$centrality
    topN <- input$topN
    nodeType <- input$nodeType
    
    # Filter nodes based on centrality measure
    filteredNodes <- filteredNodes[order(-filteredNodes[[centralityMeasure]]), ]
    
    # Filter nodes based on type
    if (nodeType != "All") {
      filteredNodes <- filteredNodes[filteredNodes$new_type == nodeType, ]
    }
    
    # Get the top N nodes
    topNodes <- filteredNodes[1:min(topN, nrow(filteredNodes)), ]
    topNodesAll<- nodes_df2[nodes_df2$community %in% topNodes$community,]
    
    topNodes_community<- topNodesAll %>% #new datatable
      group_by(community) %>%
      summarize(count = n()) %>%
      ungroup()
    # Create a new column with reordered names
    topNodes_community$community <- reorder(topNodes_community$community, -topNodes_community$count)
    
    # Create bar plot
    p <- ggplot(topNodes_community, aes(x = community
                                        , y = count)) +
      geom_col_interactive(aes(tooltip = community)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      labs(x = NULL, y = NULL) +
      xlab("Communities") +  # Add x-axis title
      ylab("Total Number of Companies within") +  # Add y-axis title
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5, size = 8),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank()) +
      scale_y_continuous(labels = comma)  # Add numerical scaling for y-axis
    
    ggplotly(p)
  })
  
  
  output$networkPlot_company <- renderVisNetwork({
    
    filteredNodes <- nodes_df2[is.na(nodes_df2$new_type), ]
    centralityMeasure <- input$centrality
    topN <- input$topN
    nodeType <- input$nodeType
    colorOption <- input$colorOption
    
    # Filter nodes based on centrality measure
    filteredNodes <- filteredNodes[order(-filteredNodes[[centralityMeasure]]), ]
    
    # Filter nodes based on type
    if (nodeType != "All") {
      filteredNodes <- filteredNodes[filteredNodes$type == nodeType &
                                       !is.na(filteredNodes$type), ]
    }
    
    # Add color and shape properties based on the selected option
    if (colorOption == "View by Community") {
      nodes_df2$group <- nodes_df2$community  # Assuming you have a 'community' column in 'nodes_df2'
    } else if (colorOption == "View by Type") {
      nodes_df2$shape <- "icon"
      nodes_df2$icon.code <- ifelse(is.na(nodes_df2$new_type), "f1ad", "f007")
      nodes_df2$icon.color <- ifelse(is.na(nodes_df2$new_type),"#116A7B" ,"#FF2171")
    }
    
    # Get the top N nodes and their connected nodes
    topNodes <- filteredNodes[1:min(topN, nrow(filteredNodes)), ]
    connectedNodeIds <- unique(c(
      edges_df2$from[edges_df2$to %in% topNodes$id],
      edges_df2$to[edges_df2$from %in% topNodes$id],
      topNodes$id
    ))
    selectedNodes <- nodes_df2[nodes_df2$id %in% connectedNodeIds, ]
    selectedEdges <- edges_df2[
      edges_df2$from %in% connectedNodeIds & edges_df2$to %in% connectedNodeIds,
    ]
    
    vis <- visNetwork(
      nodes = selectedNodes,
      edges = selectedEdges
    ) %>%
      visIgraphLayout(layout = "layout_with_fr", type = "full", smooth = TRUE) %>%
      visEdges(color = list(highlight = "#7C3238"), width = 4, arrows = "from") %>%
      visNodes(
        borderWidth = 1,
        shadow = TRUE,
      ) %>%
      visInteraction(dragNodes = TRUE, 
                     dragView = TRUE, 
                     zoomView = TRUE) %>%
      visLayout(randomSeed = 123)%>%
      addFontAwesome()%>%
      visLegend(
        addEdges = data.frame(
          label = c("Beneficial Owner", "Company Contact"),
          color = c("#9BABB8", "#3E7C59"),
          stringsAsFactors = FALSE,
          font.align = "top"
        ),
        ncol = 3,
        width = 0.1
      )
    if (input$CountryToggle) {
      vis <- vis %>%
        visOptions(
          highlightNearest = TRUE,
          nodesIdSelection = TRUE,
          selectedBy = "country")
    } else {
      vis <- vis %>%
        visOptions(
          highlightNearest = TRUE,
          nodesIdSelection = TRUE,
          selectedBy = "community")
      vis
    }
  })
  
  output$barPlot_company <- renderPlotly({
    filteredNodes <- nodes_df2[is.na(nodes_df2$new_type), ]
    centralityMeasure <- input$centrality
    topN <- input$topN
    nodeType <- input$nodeType
    
    # Filter nodes based on centrality measure
    filteredNodes <- filteredNodes[order(-filteredNodes[[centralityMeasure]]), ]
    
    # Filter nodes based on type
    if (nodeType != "All") {
      filteredNodes <- filteredNodes[filteredNodes$type == nodeType &
                                       !is.na(filteredNodes$type), ]    }
    
    # Get the top N nodes
    topNodes <- filteredNodes[1:min(topN, nrow(filteredNodes)), ]
    # Create a new column with reordered names
    topNodes$Name <- reorder(topNodes$name, -topNodes$revenue_omu)
    
    
    # Create bar plot
    plot_ly(topNodes, x = topNodes$name, y = topNodes$revenue_omu, type = "bar") %>%
      layout(xaxis = list(title = "Owners & Contacts",
                          tickangle = -45),
             yaxis = list(title = "Total Revenue (OMU)"),
             hoverlabel = list(namelength = -1),
             showlegend = FALSE)
    
    # Create bar plot
   # p <- ggplot(topNodes, aes(x = Name
  #                            , y = revenue_omu)) +
  #    geom_col_interactive(aes(tooltip = name)) +
  #    geom_bar(stat = "identity", fill = "steelblue") +
  #    xlab("Companies") +  # Add x-axis title
  #    ylab("Total Revenue (OMU)") +  # Add y-axis title
  #    theme_minimal() +
  #    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5, size = 8),
  #          panel.grid.major.x = element_blank(),
  #          panel.grid.minor.x = element_blank()) +
  #    scale_y_continuous(labels = comma)  # Add numerical scaling for y-axis
    
  #  ggplotly(p)
  })
  
  output$boxPlot_top_co <- renderPlotly({
    filteredNodes <- nodes_df2[is.na(nodes_df2$new_type), ]
    centralityMeasure <- input$centrality
    topN <- input$topN
    nodeType <- input$nodeType
    
    # Filter nodes based on centrality measure
    filteredNodes <- filteredNodes[order(-filteredNodes[[centralityMeasure]]), ]
    
    # Filter nodes based on type
    if (nodeType != "All") {
      filteredNodes <- filteredNodes[filteredNodes$type == nodeType &
                                       !is.na(filteredNodes$type), ]    }
    
    # Get the top N nodes
    topNodes <- filteredNodes[1:min(topN, nrow(filteredNodes)), ]
    
    # Create a box plot for total_revenue_omu using plotly
    plot_ly(topNodes, x = "", y = ~revenue_omu, type = "box") %>%
      layout(xaxis = list(title = ""),
             yaxis = list(title = "Total Revenue (OMU)"),
             hoverlabel = list(namelength = -1),
             showlegend = FALSE)
  })
  
  output$boxPlot_all_co <- renderPlotly({
    filteredNodes <- nodes_df2[is.na(nodes_df2$new_type), ]
    centralityMeasure <- input$centrality
    topN <- input$topN
    nodeType <- input$nodeType
    
    # Filter nodes based on centrality measure
    filteredNodes <- filteredNodes[order(-filteredNodes[[centralityMeasure]]), ]
    
    # Filter nodes based on type
    if (nodeType != "All") {
      filteredNodes <- filteredNodes[filteredNodes$type == nodeType &
                                       !is.na(filteredNodes$type), ]
    }
    
    # Get the top N nodes
    topNodes <- filteredNodes[1:min(topN, nrow(filteredNodes)), ]
    
    # Create a box plot for total_revenue_omu using plotly
    plot_ly(filteredNodes, x = "", y = ~revenue_omu, type = "box") %>%
      layout(xaxis = list(title = ""),
             yaxis = list(title = "Total Revenue (OMU)"),
             hoverlabel = list(namelength = -1),
             showlegend = FALSE)
  })
  
  output$barPlot_co_country <- renderPlotly({
    filteredNodes <- nodes_df2[is.na(nodes_df2$new_type), ]
    centralityMeasure <- input$centrality
    topN <- input$topN
    nodeType <- input$nodeType
    
    # Filter nodes based on centrality measure
    filteredNodes <- filteredNodes[order(-filteredNodes[[centralityMeasure]]), ]
    
    # Filter nodes based on type
    if (nodeType != "All") {
      filteredNodes <- filteredNodes[filteredNodes$type == nodeType &
                                       !is.na(filteredNodes$type), ]
    }
    
    # Get the top N nodes
    topNodes <- filteredNodes[1:min(topN, nrow(filteredNodes)), ]
    
    topNodes_country <- topNodes %>% #new datatable
      group_by(country) %>%
      summarize(count = n()) %>%
      ungroup()
    
    # Create a new column with reordered names
    topNodes_country$Country <- reorder(topNodes_country$country, -topNodes_country$count)
    
    
    # Create bar plot
    p <- ggplot(topNodes_country, aes(x = Country
                                      , y = count)) +
      geom_col_interactive(aes(tooltip = count)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      xlab("Countries") +  # Add x-axis title
      ylab("Number of Companies") +  # Add y-axis title
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5, size = 8),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank()) + 
      scale_y_continuous(labels = comma)  # Add numerical scaling for y-axis
    
    ggplotly(p)
  })
  
  #-------------------------------------------Topic modeling Section--->
  
  word_probs <- eventReactive(input$run_button, {
    print("Run button 2 is working!")
    LDA(MC3_text, input$topic_group , method = "Gibbs", control = list(iter = 25, verbose = 25)) %>%
      tidy(matrix = "beta") %>%
      group_by(topic) %>%
      top_n(15, beta) %>%
      ungroup() %>%
      mutate(term2 = fct_reorder(term, beta))
  }
  , ignoreNULL = FALSE
  )
  
  output$myplot <- renderPlotly({
    
    dat3 <- data.frame(x = numeric(0), y = numeric(0))
    
    withProgress(message = 'Making plot', value = 0, {
      # Number of times we'll go through the loop
      n <- 10
      
      for (i in 1:n) {
        # Each time through the loop, add another row of data. This is
        # a stand-in for a long-running computation.
        dat3 <- rbind(dat3, data.frame(x = rnorm(1), y = rnorm(1)))
        
        # Increment the progress bar, and update the detail text.
        incProgress(1/n, detail = paste("Doing part", i))
        
        # Pause for 0.1 seconds to simulate a long computation.
        Sys.sleep(0.7)
      }
    })
    
    ggplot(word_probs(), aes(term2, beta, fill = as.factor(topic))) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~ topic, scales = "free") +
      coord_flip()
  })
  
  output$statplot1 <- renderPlot({
    ggbetweenstats(stopwords_removed %>%
                  left_join(word_probs(), by = c("word" = "term2"), unmatched= "drop") %>%
                  na.omit(),
                   x= topic, y= revenue_omu, type ="np",
                   xlab= "Topic Group", ylab = "Revenue($)",
                   title = "Comparison of Revenue across Topic Group",
                   pairwise.comparisons = TRUE, pairwise.display ="ns", conf.level = 0.95
    ) +
      scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M"))
  })
  
  
  output$statplot2 <- renderPlot({
    
    ggplot(data = stopwords_removed %>%
               left_join(word_probs(), by = c("word" = "term2"), unmatched= "drop") %>%
               na.omit(), 
           aes(x = topic, y = revenue_omu)) +
      stat_pointinterval(aes(interval_color = after_stat(level)),
                         point_interval = "median_qi",
                         .width = c(0.95,0.99),
                         point_color = "#C93312") +
      labs(title = "Visualizing Confidence Intervals of Median Revenue", 
           x = "Topic Group", y = "Revenue($)") +
      
      #add colors to graph 
      scale_color_manual(values = c("#446455","#D3DDDC"), 
                         aesthetics = "interval_color") +
      theme(axis.text.x = element_text(vjust = 1, hjust = 1, size = 8)) +
      scale_x_continuous(breaks=seq(1,20,1))+
      scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M"))
})
  
  #-----------------------------------------Dashboard 4 outputs-->
  
  output$networkPlot <- renderVisNetwork({
    selectedNode <- nodes_df2[nodes_df2$name == input$searchNode, ]
    degree <- input$degree
    colorOption <- input$colorOption2
    
    
    if (!is.null(selectedNode)) {
      # Get the node and its connected nodes up to the specified degree
      connectedNodes <- getConnectedNodes(selectedNode$id, degree)
      #selectedNodes <- nodes_df2[nodes_df2$id %in% connectedNodes$id, ]
      selectedNodes <- merge(connectedNodes, nodes_df2, by.x = "id", by.y = "id", all.x = TRUE)
      selectedEdges <- edges_df2[
        edges_df2$from %in% connectedNodes$id & edges_df2$to %in% connectedNodes$id,
      ]
      
      # Add color and shape properties based on the selected option
      if (colorOption == "View by Community") {
        selectedNodes$group <- selectedNodes$community  
      } else if (colorOption == "View by Type") {
        selectedNodes$shape <- "icon"
        selectedNodes$icon.code <- ifelse(is.na(selectedNodes$new_type), "f1ad", "f007")
        selectedNodes$icon.color <- ifelse(is.na(selectedNodes$new_type),"#116A7B" ,"#FF2171")
      } else if (colorOption == "View by Degree") {
        selectedNodes$group <- selectedNodes$degree
      }
      
      
      vis <- visNetwork(
        nodes = selectedNodes,
        edges = selectedEdges
      ) %>%
        visIgraphLayout(layout = "layout_nicely", type = "full", smooth = TRUE) %>%
        visEdges(color = list(highlight = "#7C3238"), width = 4, arrows = "from") %>%
        visNodes(
          borderWidth = 1,
          shadow = TRUE
        ) %>%
        visOptions(
          highlightNearest = TRUE,
          nodesIdSelection = TRUE,
          selectedBy = "community"
        ) %>%
        visInteraction(dragNodes = TRUE, 
                       dragView = TRUE, 
                       zoomView = TRUE) %>%
        addFontAwesome()%>%
        visLegend(
          addEdges = data.frame(
            label = c("Beneficial Owner", "Company Contact"),
            color = c("#9BABB8", "#3E7C59"),
            stringsAsFactors = FALSE,
            font.align = "top"
          ),
          ncol = 3,
          width = 0.1
        )
      
      if (input$layoutToggle) {
        if (input$physicsToggle) {
          vis <- vis %>% visHierarchicalLayout() %>% visPhysics(enabled = TRUE)
        } else {
          vis <- vis %>% visHierarchicalLayout() %>% visPhysics(enabled = FALSE)
        }
      } else {
        if (input$physicsToggle) {
          vis <- vis %>% visIgraphLayout(layout = "layout_with_fr", type = "full", smooth = TRUE, physics = TRUE)
        } else {
          vis <- vis %>% visIgraphLayout(layout = "layout_with_fr", type = "full", smooth = TRUE, physics = FALSE)
        }
      }
      
      
    }
  })
  
  
  {
    output$total_revenue_omu <- renderText({
      selectedNode <- nodes_df2[nodes_df2$name == input$searchNode, ]
      degree <- input$degree
      
      if (!is.null(selectedNode)) {
        connectedNodes <- getConnectedNodes(selectedNode$id, degree)
        selectedNodes <- nodes_df2[nodes_df2$id %in% connectedNodes$id, ]
        totalRevenue <- comma(sum(selectedNodes$revenue_omu, na.rm = TRUE))
        #formattedRevenue <- comma(totalRevenue)
      }
    })
    
  }
  {
    output$topic <- renderText({
      selectedNode <- nodes_df2_topic[nodes_df2_topic$name == input$searchNode, ]
      degree <- input$degree
      
      if (!is.null(selectedNode)) {
        connectedNodes <- getConnectedNodes(selectedNode$id, degree)
        selectedNodes <- nodes_df2_topic[nodes_df2_topic$id %in% connectedNodes$id, ]
        topic_counts <- table(selectedNodes$topic_clean)
        max_count_topic <- names(topic_counts)[which.max(topic_counts)]
        
        if (max_count_topic == "unknown" && length(topic_counts) > 1) {
          second_max_count_topic <- names(topic_counts[order(topic_counts, decreasing = TRUE)])[2]
          max_count_topic <- ifelse(is.na(second_max_count_topic), max_count_topic, second_max_count_topic)
        }
      }
    })
  }
  {
    output$no_of_topic <- renderText({
      selectedNode <- nodes_df2_topic[nodes_df2_topic$name == input$searchNode, ]
      degree <- input$degree
      
      if (!is.null(selectedNode)) {
        connectedNodes <- getConnectedNodes(selectedNode$id, degree)
        selectedNodes <- nodes_df2_topic[nodes_df2_topic$id %in% connectedNodes$id, ]
        distinct_count <- selectedNodes %>% 
          filter(topic != "unknown")%>%
          distinct(topic) %>% 
          n_distinct()
      }
    })
  }
  
  {
    output$community <- renderText({
      selectedNode <- nodes_df2[nodes_df2$name == input$searchNode, ]
      degree <- input$degree
      
      if (!is.null(selectedNode)) {
        connectedNodes <- getConnectedNodes(selectedNode$id, degree)
        selectedNodes <- nodes_df2[nodes_df2$id %in% connectedNodes$id, ]
        community_counts <- table(selectedNodes$community)
        max_community_counts <- names(community_counts)[which.max(community_counts)]
      }
    })
  }
}

# Return a Shiny app object

shinyApp(ui = ui, server = server)

