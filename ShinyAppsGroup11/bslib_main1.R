#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

pacman::p_load(shiny, bslib, scales, plotly, heatmaply, readr, jsonlite, tidygraph, tidyverse, ggraph, ggiraph, visNetwork, graphlayouts, ggforce, tidytext, skimr, treemap, ggdist, wordcloud, RColorBrewer, tm, udpipe, lattice, stringr, wordcloud2)


nodes_df2 <- read_rds("data/nodes_df2.rds")
mc3_edges_country <- read_rds("data/mc3_edges_country.rds")
edges_df2 <- read_rds("data/edges_df2.rds")


options(scipen = 999) #disables scientific notation

cards <- list(
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

# UI
ui <- page_sidebar(
  title = "Owners, Contacts & Companies",
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
              cards[[1]],
              layout_columns(
                cards[[2]], cards[[5]]
              ),
              layout_columns(
                cards[[3]], cards[[4]], cards[[11]]
              )
    ),
    nav_panel(title = "Companies",
              layout_column_wrap(
                width = "250px",
                fill = FALSE
              ),
              cards[[6]],
              layout_columns(
                cards[[7]], cards[[10]]
              ),
              layout_columns(
                cards[[8]], cards[[9]]
              )
    )
  )
)



server <- function(input, output, session) {
  output$networkPlot_owner <- renderVisNetwork({
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
    p <- ggplot(topNodes, aes(x = name
                              , y = total_revenue_omu)) +
      geom_col_interactive(aes(tooltip = name)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      labs(x = NULL, y = NULL) +
      theme_minimal() +
      xlab("Owners & Contacts") +  # Add x-axis title
      ylab("Total Revenue (OMU)") +  # Add y-axis title
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5, size = 8),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank()) +
      scale_y_continuous(labels = comma)  # Add numerical scaling for y-axis
    
    ggplotly(p)
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
    p <- ggplot(topNodes, aes(x = Name
                              , y = revenue_omu)) +
      geom_col_interactive(aes(tooltip = name)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      xlab("Companies") +  # Add x-axis title
      ylab("Total Revenue (OMU)") +  # Add y-axis title
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5, size = 8),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank()) +
      scale_y_continuous(labels = comma)  # Add numerical scaling for y-axis
    
    ggplotly(p)
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

}

# Run the app
shinyApp(ui, server)
