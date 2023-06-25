

library(shiny)
library(shiny.semantic)
library(bslib)
library(dplyr)
library(ggplot2)
library(thematic)
library(tidyverse)
library(tidygraph)
library(jsonlite)


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
  ),
  card(
    full_screen = FALSE,
    card_header("Companies by Country"),
    plotOutput("company_plot")
  ),
  card(
    full_screen = FALSE,
    card_header("Ownership by Individual"),
    plotOutput("ownership_plot")
  )
)

##Parent Value_box

value_box_1 <- value_box(
  title = "Median of the Number of Companies Owned per Person",
  value = "2 Companies",
  showcase = bsicons::bs_icon("align-bottom")
)

value_box_2 <- value_box(
  title = "Median of the Number of Companies Contacts per Person",
  value =  "1 Company",
  showcase = bsicons::bs_icon("align-center"),
  theme_color = "dark"
)

value_box_3 <- value_box(
  title = "Median Revenue per Person",
  value = "$25,000",
  showcase = bsicons::bs_icon("handbag"),
  theme_color = "secondary"
)

value_box_parent <- column(
  width = 12, 
  height = 18,
  value_box_1,
  value_box_2,
  value_box_3
)

ui <- navbarPage(
  title = "Group 11",
  theme = bs_theme(version = 5, bootswatch = "minty"), # Choose a theme
  #Cards
  layout_columns(
    fill = FALSE,
    col_widths = c(4, 4, 4, 4, 4),
    row_heights = c(1, 1),
    cards[[1]], cards[[2]], value_box_parent, cards[[3]], cards[[4]]),
  
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
      geom_bar(stat = "identity", fill = '#3498db') +
      geom_text(aes(label = format(revenue_omu, big.mark = ",")), vjust = -0.5) +
      scale_y_continuous(labels = function(x) format(x/1000000, nsmall = 1, big.mark = ".", decimal.mark = ",")) +
      theme_minimal() +
      labs(x = "Country", y = "Revenue in mil",
           title = 'Distribution of Revenue by Country',
           subtitle = 'ZH, Rio Isla, and Utoporiana were the top 3 countries where most revenue was generated') +
      theme(axis.title.y = element_text(angle = 90, vjust = 0.5, hjust = 1))
  })
  
  # Company plot
  output$company_plot <- renderPlot({
    mc3_nodes1 %>%
      group_by(id) %>%
      summarise(total_countries = n_distinct(country)) %>%
      filter(total_countries > 3) %>%
      arrange(desc(total_countries)) %>%
      ggplot(aes(x = id, y = total_countries)) +
      geom_bar(stat = "identity", fill = '#3498db') +
      geom_text(aes(label = format(total_countries, big.mark=",")), vjust = -0.5) +
      theme_minimal() +
      labs(x = "Companies", y = "Number of Countries",
           title = 'Distribution of Company located in multiple countries',
           subtitle = 'Aqua Aura SE Marine is the most diversified in terms of countries locations') +
      scale_y_continuous(breaks = seq(0, 10, by = 2),
                         labels = seq(0, 10, by = 2))+
      theme(axis.title.y = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1, 
                                       size = 10, margin = margin(t = 0.2, r = 0, b = 0, l = 0)))
  })
  
  # Ownership plot
  output$ownership_plot <- renderPlot({
    mc3_nodes1 %>%
      group_by(id) %>%
      filter(total_relation > 8) %>%
      arrange(desc(total_relation)) %>%
      ggplot(aes(reorder(x = id, -total_relation), y = total_relation)) +
      geom_bar(stat = "identity", fill = '#3498db') +
      geom_text(aes(label = format(total_relation, big.mark=",")), vjust = -0.5) +
      theme_minimal() +
      labs(x = "Companies", y = "Number of relationships",
           title = 'Distribution of Individuals who have the most amount of Company relationships') +
      scale_y_continuous(breaks = seq(0, 10, by = 2),
                         labels = seq(0, 10, by = 2))+
      theme(axis.title.y = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1, 
                                       size = 10, margin = margin(t = 0.2, r = 0, b = 0, l = 0)))
  })
}


shinyApp(ui, server)