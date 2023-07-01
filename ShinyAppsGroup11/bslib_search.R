#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(bslib)
library(visNetwork)
library(shinyWidgets)
library(scales)
library(plotly)
library(bsicons)
library(readr)


nodes_df2 <- read_rds("data/nodes_df2.rds")
mc3_edges_country <- read_rds("data/mc3_edges_country.rds")
edges_df2 <- read_rds("data/edges_df2.rds")
nodes_df2_topic <- read_rds("data/nodes_df2_topic.rds")

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

#cards
cards <- list(
  card(
    full_screen = TRUE,
    card_header("Network Graph"),
    visNetworkOutput("networkPlot"#, height = "500px"))
    )
  )#,
 # card(
 #   verbatimTextOutput("variableData")
 # )
)

#value_boxes
vbs <- list(
  value_box(
    title = "Total Revenue (OMU)", 
    value = textOutput("total_revenue_omu"),
    showcase = bs_icon("cash-coin"),
    p("Total revenue involved within network")
  ),
  value_box(
    title = "Highest associated Topic", 
    value = textOutput("topic"),
    showcase = bs_icon("water"),
    p("Based on number of topics (K) = 4 "),
    theme_color = "info"
  ),
  value_box(
    title = "Top Community", 
    value = textOutput("community"),
    showcase = bs_icon("people-fill"),
    p("Top community within the network"),
    theme_color = "success"
  )
)

# UI
ui <- page_sidebar(
  sidebar = sidebar(
    pickerInput(
        inputId = "searchNode",
        label = "Search Names:",
        choices = sort(nodes_df2$name),
        options = list(`live-search` = TRUE),
        multiple = FALSE
      ),
    sliderInput("degree", "Nth Degree of relationship:", min = 1, max = 10, value = 2),
    tags$hr(),  # Add a horizontal line as a divider
    p(style = "font-family: Arial; font-style: italic; font-size: 16px;", "Graph Filters"),
    radioButtons("colorOption", "Visualise nodes by:",
                 choices = c("View by Community", "View by Type", "View by Degree"),
                 selected = "View by Community"),
    p(style = "font-family: Arial; font-size: 16px;", "Layout:"),
    checkboxInput("layoutToggle", "Hierarchical Layout", value = FALSE),
    checkboxInput("physicsToggle", "Enable Physics", value = FALSE)
    ),
  layout_column_wrap(
    width = "250px",
    fill = FALSE,
    !!!vbs
    ),
  !!!cards
  )

server <- function(input, output, session) {
  output$networkPlot <- renderVisNetwork({
    selectedNode <- nodes_df2[nodes_df2$name == input$searchNode, ]
    degree <- input$degree
    colorOption <- input$colorOption
    
    
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
      } else if (colorOption == "View by Degree of relationship") {
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
  
  output$variableData <- renderPrint({
    selectedNode <- nodes_df2[nodes_df2$name == input$searchNode, ]
    degree <- input$degree
    connectedNodeIds <- getConnectedNodes(selectedNode$id, degree)
    #selectedNodes <- nodes_df2[nodes_df2$id %in% connectedNodeIds$nodeId, ]
    #selectedNodes <- merge(connectedNodeIds, nodes_df2, by.x = "nodeId", by.y = "id", all.x = TRUE)
    #variable <- your_variable  # Replace 'your_variable' with the actual variable name
    connectedNodeIds  # Return the variable
  })
  
  
  
  
}

# Run the app
shinyApp(ui, server)
