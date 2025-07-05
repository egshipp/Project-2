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
           title = "Correlation Matrix of Nutrition Values of Fruit")
