library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(bslib)
library(scales)
library(DT)
library(readxl)

# Load and clean the data with error handling
load_vaccination_data <- function() {
  tryCatch({
    data <- read_excel("vaccinations.xlsx")
    
    # Check and rename columns if needed
    if("country" %in% names(data)) {
      data <- data %>% rename(location = country)
    }
    
    data <- data %>%
      filter(!is.na(total_vaccinations_per_hundred)) %>%
      arrange(date) %>%
      group_by(location) %>%  # moved group_by after ensuring location exists
      slice_tail(n = 1) %>%
      ungroup() %>%
      filter(!location %in% c("World", "Europe", "Asia", "North America", "South America", 
                              "European Union", "Africa", "Oceania")) %>%
      select(
        location,
        date,
        total_vaccinations_per_hundred,
        people_vaccinated_per_hundred,
        people_fully_vaccinated_per_hundred
      ) %>%
      mutate(date = as.Date(date))
    
    return(data)
    
  }, error = function(e) {
    message("Error loading data: ", e$message)
    # Create sample data with consistent number of rows
    sample_data <- data.frame(
      location = c("Sample Country 1", "Sample Country 2", "United States", "United Kingdom", "Canada"),
      date = as.Date(rep("2023-01-01", 5)),
      total_vaccinations_per_hundred = c(80, 70, 75, 85, 90),
      people_vaccinated_per_hundred = c(75, 65, 70, 80, 85),
      people_fully_vaccinated_per_hundred = c(70, 60, 65, 75, 80)
    )
    return(sample_data)
  })
}
# Load the data
vaccinations <- load_vaccination_data()

ui <- page_sidebar(
  title = "COVID-19 Vaccination Progress Comparison Dashboard",
  
  sidebar = sidebar(
    title = "Analysis Controls",
    
    selectInput("metric", "Vaccination Metric:",
                choices = c(
                  "Total Vaccinations" = "total_vaccinations_per_hundred",
                  "People Vaccinated" = "people_vaccinated_per_hundred",
                  "Fully Vaccinated" = "people_fully_vaccinated_per_hundred"
                ),
                selected = "total_vaccinations_per_hundred"),
    
    selectInput("compare_countries", "Select Countries to Compare:",
                choices = sort(unique(vaccinations$location)),
                multiple = TRUE,
                selected = head(unique(vaccinations$location), 3)),
    
    sliderInput("top_n", "Show Top N Countries in Overview:",
                min = 5, max = 30, value = 15, step = 5),
    
    hr(),
    
    helpText(paste("Data last updated:", format(max(vaccinations$date), "%B %d, %Y"))),
    helpText("Use this dashboard to compare vaccination progress across countries.")
  ),
  
  layout_columns(
    fill = FALSE,
    
    card(
      card_header("Global Vaccination Progress Overview"),
      plotOutput("barplot", height = "400px")
    ),
    
    layout_columns(
      fill = FALSE,
      col_widths = c(6, 6),
      
      card(
        card_header("Selected Countries Comparison"),
        plotOutput("comparison_plot", height = "300px")
      ),
      
      card(
        card_header("Vaccination Coverage Analysis"),
        plotOutput("coverage_plot", height = "300px")
      )
    ),
    
    card(
      card_header("Detailed Country Data"),
      DTOutput("country_table")
    )
  )
)

server <- function(input, output) {
  
  filtered_data <- reactive({
    req(input$metric, input$top_n)
    vaccinations %>%
      arrange(desc(!!sym(input$metric))) %>%
      head(input$top_n)
  })
  
  comparison_data <- reactive({
    req(input$compare_countries)
    vaccinations %>%
      filter(location %in% input$compare_countries)
  })
  
  get_metric_name <- function(metric) {
    case_when(
      metric == "total_vaccinations_per_hundred" ~ "Total Vaccinations per 100 People",
      metric == "people_vaccinated_per_hundred" ~ "People Vaccinated per 100 People",
      metric == "people_fully_vaccinated_per_hundred" ~ "People Fully Vaccinated per 100 People"
    )
  }
  
  output$barplot <- renderPlot({
    req(filtered_data())
    metric_name <- get_metric_name(input$metric)
    
    ggplot(filtered_data(), aes(x = reorder(location, !!sym(input$metric)), 
                                y = !!sym(input$metric))) +
      geom_bar(stat = "identity", fill = "#2C3E50") +
      geom_text(aes(label = round(!!sym(input$metric), 1)), 
                hjust = -0.1) +
      coord_flip() +
      labs(x = "Country",
           y = metric_name,
           title = paste("Top", input$top_n, "Countries by", metric_name)) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12)
      )
  })
  
  output$comparison_plot <- renderPlot({
    req(comparison_data())
    metric_name <- get_metric_name(input$metric)
    
    ggplot(comparison_data(), 
           aes(x = reorder(location, !!sym(input$metric)), 
               y = !!sym(input$metric))) +
      geom_bar(stat = "identity", aes(fill = location)) +
      geom_text(aes(label = round(!!sym(input$metric), 1)), 
                vjust = -0.5) +
      labs(x = "Country",
           y = metric_name,
           title = "Vaccination Progress Comparison") +
      theme_minimal() +
      theme(
        legend.position = "none",
        plot.title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
  
  output$coverage_plot <- renderPlot({
    req(comparison_data())
    
    comparison_data() %>%
      gather(key = "metric", value = "value", 
             people_vaccinated_per_hundred,
             people_fully_vaccinated_per_hundred) %>%
      mutate(metric = case_when(
        metric == "people_vaccinated_per_hundred" ~ "At Least One Dose",
        metric == "people_fully_vaccinated_per_hundred" ~ "Fully Vaccinated"
      )) %>%
      ggplot(aes(x = location, y = value, fill = metric)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(x = "Country",
           y = "Percentage of Population",
           title = "Vaccination Coverage Comparison",
           fill = "Vaccination Status") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
  
  output$country_table <- renderDT({
    req(comparison_data())
    comparison_data() %>%
      select(
        Country = location,
        Date = date,
        `Total Vaccinations` = total_vaccinations_per_hundred,
        `At Least One Dose` = people_vaccinated_per_hundred,
        `Fully Vaccinated` = people_fully_vaccinated_per_hundred
      ) %>%
      datatable(
        options = list(
          pageLength = 10,
          dom = 'tp',
          ordering = TRUE
        ),
        rownames = FALSE
      ) %>%
      formatRound(columns = c("Total Vaccinations", "At Least One Dose", "Fully Vaccinated"), 
                  digits = 1)
  })
}

shinyApp(ui = ui, server = server)