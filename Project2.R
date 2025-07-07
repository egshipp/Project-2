# Package installation

library(shiny)
library(tidyverse)
library(httr)
library(jsonlite)
library(dplyr)


# API function -----------------------------------------------------------------

fruit_data <- function(fruit_name = "all"){
  base_url <- "https://www.fruityvice.com/api/fruit/"
  
  url <- paste0(base_url, fruit_name)
  
  raw_data <- httr::GET(url)
  
  parsed_data <- fromJSON(rawToChar(raw_data$content))
  
  data <- as_tibble(parsed_data)
  
  return(data)
}

data <- fruit_data()

#Adding a color categorical variable

data$color <- c("Orange", "Red", "Brown", "Red", "Green", "Yellow", "Purple", "Red", "Brown", "Brown",
                "Yellow", "Purple", "Green", "Purple", "Purple", "Orange", "Green", "Red", "Green", "Yellow",
                "Green", "Blue", "Red", "Green", "Orange", "Brown", "Orange", "Red", "Green", "Red", "Red", "Purple",
                "Red", "Green", "Green", "Brown", "Red", "Red", "Orange", "Yellow", "Yellow", "Brown", "Yellow",
                "Purple", "Orange", "Orange", "Orange", "Green", "Red")


# Contingency Tables -----------------------------------------------------------
## Contingency table between family and fruits

data |>
  count(family, name = "fruit_count") |>
  arrange(desc(fruit_count))

## Contingency table between order and fruits

data |> 
  group_by(family) |>
  count(genus, name = "fruit_count") |>
  arrange(desc(fruit_count))

# Numerical Summaries-----------------------------------------------------------

## Numerical summaries based on color

data |> 
  group_by(color) |>
  summarize(
    mean_calories = mean(nutritions$calories, na.rm = TRUE),
    sd_calories = sd(nutritions$calories, na.rm = TRUE),
    mean_fat = mean(nutritions$fat, na.rm = TRUE),
    sd_fat = sd(nutritions$fat, na.rm = TRUE),
    mean_sugar = mean(nutritions$sugar, na.rm = TRUE),
    sd_sugar = sd(nutritions$sugar, na.rm = TRUE), 
    mean_carb = mean(nutritions$carbohydrates, na.rm = TRUE),
    sd_carb = sd(nutritions$carbohydrates, na.rm = TRUE), 
    mean_protein = mean(nutritions$protein, na.rm = TRUE),
    sd_protein = sd(nutritions$protein, na.rm = TRUE)
  )

## Avg sugar and carbs with min and max for every level of color
data |> 
  group_by(color) |>
  summarize(
    count = n(),
    avg_sugar = mean(nutritions$sugar, na.rm = TRUE),
    min_sugar = min(nutritions$sugar, na.rm = TRUE),
    max_sugar = max(nutritions$sugar, na.rm = TRUE),
    avg_carb = mean(nutritions$carbohydrates, na.rm = TRUE),
    min_carb = min(nutritions$carbohydrates, na.rm = TRUE),
    max_carb = max(nutritions$carbohydrates, na.rm = TRUE)
  ) |>
  arrange(desc(avg_sugar))

#Plots--------------------------------------------------------------------------
## Boxplots of sugar by Color

ggplot(data = data, aes(x = color, y = nutritions$sugar)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Sugar Distributions by Color",
       x = "Color of Fruit on Outside",
       y = "Sugar Content")

## Scatterplot of Protein vs. Carbohydrates colored by family

ggplot(data = data, aes(x = nutritions$carbohydrates, y = nutritions$protein, color = family)) +
  geom_point() + 
  theme_minimal() +
  labs(title = "Scatterplot of Carbohydrates vs.Protein",
       x = "Carbohydrates",
       y = "Protein")

## Correlation Matrix Plot 
install.packages("ggcorrplot")
library(ggcorrplot)

data_num <- data |>
  mutate(
    calories = as.numeric(nutritions$calories),
    sugar = as.numeric(nutritions$sugar),
    carbohydrates = as.numeric(nutritions$carbohydrates),
    protein = as.numeric(nutritions$protein)
  ) |>
  select(calories, sugar, carbohydrates, protein)

corr <- cor_pmat(data_num)

ggcorrplot(corr,
           method = "circle",
           insig = "blank",
           lab = TRUE,
           title = "Correlation Matrix of Nutrition Values of Fruit",
           type = "lower")

## Barchart for calorie amounts

data <- data |> 
  mutate(calories = as.numeric(nutritions$calories)) |>
  mutate(calorie_bin = cut(
    nutritions$calories,
    breaks = seq(0, 200, by = 20),
    include.lowest = TRUE,
    right = FALSE
  )) 

bin_counts <- data |> 
  count(calorie_bin, name = "fruit_count")

ggplot(bin_counts, aes(x = calorie_bin, y = fruit_count, fill = calorie_bin)) +
  geom_bar(stat = "identity", color = "black") + 
  theme_minimal() +
  labs(title = "Fruits Grouped By Calorie Ranges",
       x = "Calorie Ranges",
       y = "Fruit Counts") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Shiny App---------------------------------------------------------------------

ui <- navbarPage(
  "Fruit Nutrition Explorer",
  
  # About tab
  tabPanel(
    "About",
    fluidPage("About This App"),
    p("Welcome to the Fruit Nutrition Explorer!"),
    p("This app allows you to explore the nutritional data on a plethora of fruits"),
    p("This data comes from the Fruityvice API",
      tags$a(href="https://www.fruityvice.com/", "Fruityvice Website"))
  ),
  
  # Data download tab
  tabPanel(
    "Data Download",
    sidebarLayout(
      sidebarPanel(
        textInput("fruit_name", "Fruit Name (or leave blank for all):", value = ""),
        actionButton("get_data", "Fetch Data"),
        hr(),
        uiOutput("columns_ui"),
        uiOutput("rows_ui"),
        downloadButton("download_data", "Download CSV")
      ),
      mainPanel(
        DT::dataTableOutput("table"),
        p("Please make sure you fetch the data you will be using before proceding the Data Exploration tab")
      )
    )
  ),

  # Data exploration tab
  tabPanel(
    "Data Exploration",
    sidebarLayout(
      sidebarPanel(
        selectInput("xvar", "X variable:", choices=NULL),    # dynamic
        selectInput("yvar", "Y variable:", choices=NULL),    # dynamic
        selectInput("facetvar", "Facet by:", choices=NULL),  # dynamic
        radioButtons("plottype", "Plot Type:",
                     choices=c("Scatterplot", "Boxplot", "Barplot"))
      ),
      mainPanel(
        plotOutput("plot"),
        verbatimTextOutput("summary")
      )
    )
  )
)


server <- function(input, output, session){
  # Data from API
  fetched_data <- eventReactive(input$get_data, {
    fruit_data <- function(fruit_name="all") {
      base_url <- "https://www.fruityvice.com/api/fruit/"
      url <- paste0(base_url, fruit_name)
      raw <- httr::GET(url)
      parsed <- jsonlite::fromJSON(rawToChar(raw$content), flatten = TRUE)
      data <- tibble::as_tibble(parsed)
      return(data)
    }
    
    df <- fruit_data(ifelse(input$fruit_name=="", "all", input$fruit_name))
    df$color <- c("Orange", "Red", "Brown", "Red", "Green", "Yellow", "Purple", "Red", "Brown", "Brown",
                  "Yellow", "Purple", "Green", "Purple", "Purple", "Orange", "Green", "Red", "Green", "Yellow",
                  "Green", "Blue", "Red", "Green", "Orange", "Brown", "Orange", "Red", "Green", "Red", "Red", "Purple",
                  "Red", "Green", "Green", "Brown", "Red", "Red", "Orange", "Yellow", "Yellow", "Brown", "Yellow",
                  "Purple", "Orange", "Orange", "Orange", "Green", "Red")[seq_len(nrow(df))]
    df
  })
  output$table <- DT::renderDataTable({
    req(fetched_data())
    fetched_data()
  })
  
  # Choosing columns and rows
  output$columns_ui <- renderUI({
    req(fetched_data())
    checkboxGroupInput("columns", "Select Columns:", choices=names(fetched_data()), selected=names(fetched_data()))
  })
  
  output$rows_ui <- renderUI({
    req(fetched_data())
    sliderInput("rows", "Rows to Display:", min=1, max=nrow(fetched_data()), value=c(1, nrow(fetched_data())))
  })
  
  # Download the subset data into a csv
  downloadHandler(
    filename=function() { paste0("fruit_data_", Sys.Date(), ".csv") },
    content=function(file) {
      df <- fetched_data()[input$rows[1]:input$rows[2], input$columns, drop=FALSE]
      write.csv(df, file, row.names=FALSE)
    }
  )
  
  # Choosing variables using a dropdown that will be observed only if new is chosen
  observe({
    req(fetched_data())
    num_vars <- names(fetched_data() |> select(where(is.numeric)))
    all_vars <- names(fetched_data())
    updateSelectInput(session, "xvar", choices=all_vars)
    updateSelectInput(session, "yvar", choices=num_vars)
    updateSelectInput(session, "facetvar", choices=all_vars)
  })
  
  # All summaries and outputs
  output$summary <- renderPrint({
    req(fetched_data())
    df <- fetched_data()
    if (!is.null(input$xvar) && input$xvar %in% names(df)) {
      summary(df[[input$xvar]])
    }
  })
  
  output$plot <- renderPlot({
    req(fetched_data(), input$xvar)
    df <- fetched_data()
    
    p <- ggplot(df, aes_string(x=input$xvar))
    
    if (input$plottype == "Scatterplot" && input$yvar!="") {
      p <- p + geom_point(aes_string(y=input$yvar, color=input$facetvar))
    } else if (input$plottype == "Boxplot" && input$yvar!="") {
      p <- p + geom_boxplot(aes_string(y=input$yvar, fill=input$facetvar))
    } else if (input$plottype == "Barplot") {
      p <- p + geom_bar(aes_string(fill=input$facetvar))
    }
    
    p + theme_minimal() + labs(title="Customizable Plot", x=input$xvar, y=input$yvar)
  })
}

shinyApp(ui = ui, server = server)
