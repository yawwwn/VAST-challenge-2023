

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(thematic)
library(jsonlite)
library(tidyverse)


mc3_data <- fromJSON("data/MC3.json")

mc3_nodes <- as_tibble(mc3_data$nodes) %>%
  distinct() %>% # Remove duplicate rows
  mutate(country = as.character(country),             # Convert to character type
         id = as.character(id),                       # Convert to character type
         product_services = as.character(product_services),   # Convert to character type
         revenue_omu = as.numeric(ifelse(revenue_omu == "Unknown" | revenue_omu == "NULL", 0, revenue_omu)),
         type = as.character(type)                    # Convert to character type
  ) %>%
  select(id,country,type,revenue_omu,product_services)

#| code-fold: false
#| fig-height: 3.5
#| fig-width: 7
#Extracting edges
mc3_edges_raw <- as_tibble(mc3_data$links) %>% 
  distinct() %>% # Remove duplicate rows
  mutate(source = as.character(source), # Convert to character type
         target = as.character(target), # Convert to character type
         type = as.character(type)) %>% # Convert to character type
  rename(from = source,
         to = target) %>% #ensure compatibility with `tidygraph` functions
  group_by(from, to, type) %>%
  summarise(weights = n()) %>%
  filter(from!=to) %>% #to ensure that no record with similar source and target.
  ungroup()

mc3_nodes_group <- mc3_nodes %>% #given some companies/nodes have multiple rows in different countries
  group_by(id) %>%
  summarize(total_revenue_omu = sum(revenue_omu, na.rm = TRUE))

#Extracting edges
mc3_edges <- mc3_edges_raw %>% 
  left_join(mc3_nodes_group, by = c("from" = "id")) %>% 
  select(from, to, type, weights, total_revenue_omu) 


summary_data <- mc3_edges %>%
  group_by(to) %>%
  rename(id = to) %>%
  summarize(total_revenue_omu = sum(total_revenue_omu, na.rm = TRUE),
            companies_owned = sum(type == "Beneficial Owner"),
            companies_contact = sum(type == "Company Contacts"),
            total_relation = companies_owned+companies_contact)%>%
  arrange(desc(total_relation)) #sort total_relation by descending

#| code-fold: false
#| fig-height: 3.5
#| fig-width: 7

id1 <- mc3_edges %>% # extract the source column from the edges dataframe and rename it to id1
  select(from) %>%
  rename(id = from)

id2 <- mc3_edges %>% # extract the target column from the edges dataframe and rename it to id2
  select(to) %>%
  rename(id = to)

mc3_nodes1 <- rbind(id1, id2) %>% # combine the id1 and id2 dataframes 
  distinct() %>% # remove the duplicates
  left_join(mc3_nodes, by = "id",
            unmatched = "drop") %>%
  left_join(summary_data, by = "id") %>%
  replace_na(list(companies_owned = 0, companies_contact = 0, total_relation = 0))%>% #replace NA values with 0
  mutate(new_type = ifelse(companies_owned>0 & companies_contact>0, "Beneficial Owner + Company Contact",ifelse(companies_owned>0 & companies_contact==0, "Beneficial Owner", ifelse(companies_owned==0 & companies_contact>0, "Company Contact",NA_character_)))) %>%
  mutate(revenue_omu = ifelse(is.na(revenue_omu), 0, revenue_omu))
#create new attributes for node

###Cards begin
cards <- list(
  card(
    full_screen = TRUE,
    card_header("Counts by Country"),
    plotOutput("counts_plot")
  ),
  card(
    full_screen = TRUE,
    card_header("Revenue by Country"),
    plotOutput("revenue_plot")
  )
)


ui <- navbarPage(
  title = "Group 11",
  theme = bs_theme(version = 5, bootswatch = "minty"), # Choose a theme
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
  
  # Third panel tab
  tabPanel(
    "Topic Analysis",
    fluidPage(
      plotOutput("")
    )
  ),
  
  #Cards
  !!!cards
)

# Define server logic
server <- function(input, output) {
  
  # Counts plot
  output$counts_plot <- renderPlot({
    mc3_nodes1_Other <- mc3_nodes1 %>%
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
  
  # Revenue plot
  output$revenue_plot <- renderPlot({
    mc3_nodes1 %>%
      filter(!is.na(country)) %>%
      group_by(country) %>%
      summarise(revenue_omu = sum(revenue_omu)) %>%
      arrange(desc(revenue_omu)) %>%
      mutate(country = ifelse(row_number() > 3, "Other", country)) %>%
      group_by(country) %>%
      summarise(revenue_omu = sum(revenue_omu)) %>%
      ggplot(aes(x = reorder(country, -revenue_omu), y = revenue_omu)) +
      geom_histogram(stat = "identity", fill = '#3498db') +
      geom_text(aes(label = format(revenue_omu, big.mark = ",")), vjust = -0.5) +
      scale_y_continuous(labels = function(x) format(x/1000000, nsmall = 1, big.mark = ".", decimal.mark = ",")) +
      theme_minimal() +
      labs(x = "Country", y = "Revenue in mil",
           title = 'Distribution of Revenue by Country',
           subtitle = 'ZH, Rio Isla, and Utoporiana were the top 3 countries where most revenue was generated') +
      theme(axis.title.y = element_text(angle = 90, vjust = 0.5, hjust = 1))
  })
}


shinyApp(ui, server)