# Package installation

library(shiny)
library(tidyverse)
library(httr)
library(jsonlite)
library(dplyr)


# API function -----------------------------------------------------------------

get_websitecarbon_data <- function(urls, bytes = 2200000, green = 0) {
  
  # Original api_function inside
  api_function <- function(url, bytes = 2200000, green = 0) {
    base_url <- "https://api.websitecarbon.com/data"
    full_url <- paste0(
      base_url,
      "?url=", URLencode(url),
      "&bytes=", bytes,
      "&green=", green
    )
    
    raw_data <- httr::GET(full_url)
    
    parsed_data <- fromJSON(rawToChar(raw_data$content))
    
    flat_data <- tibble(
      url            = parsed_data$url,
      green          = parsed_data$green,
      bytes          = parsed_data$bytes,
      cleanerThan    = parsed_data$cleanerThan,
      rating         = parsed_data$rating,
      timestamp      = parsed_data$timestamp
    )
    
    stats <- parsed_data$statistics
    stats_flat <- tibble(
      adjustedBytes   = stats$adjustedBytes,
      energy              = stats$energy,
      co2_grid_grams      = stats$co2$grid$grams,
      co2_grid_litres     = stats$co2$grid$litres,
      co2_grid_miles      = stats$co2$grid$miles,
      co2_renew_grams     = stats$co2$renewable$grams,
      co2_renew_litres    = stats$co2$renewable$litres,
      co2_renew_miles     = stats$co2$renewable$miles
    )
    
    bind_cols(flat_data, stats_flat)
  }
  
  # Simple loop over URLs
  results_list <- lapply(urls, api_function)
  
  # Coerce green to character in each result
  results_list <- lapply(results_list, function(df) {
    df$green <- as.character(df$green)
    df
  })
  
  # Combine all results into one data frame
  results_df <- bind_rows(results_list)
  
  return(results_df)
}

# Top 10 sites visited

urls <- c(
  "https://www.google.com",
  "https://www.youtube.com",
  "https://www.facebook.com",
  "https://www.instagram.com",
  "https://www.twitter.com",
  "https://www.wikipedia.org",
  "https://www.amazon.com",
  "https://www.yahoo.com",
  "https://web.whatsapp.com",
  "https://www.tiktok.com"
)

test_data <- get_websitecarbon_data(urls)

print(test_data)

# Contingency Tables -----------------------------------------------------------
## Contingency table between whether website is known to be from renewable resources and the rating

table(test_data$rating, test_data$green)

## Contingency table with binned cleaner than percentiles and whether it is considered a green website or not

test_data |>
  mutate(cleaner_bucket = cut(cleanerThan, breaks = seq(0, 1, by=0.25))) |>
  with(table(cleaner_bucket, green))

# Numerical Summaries-----------------------------------------------------------
## Create numerical summaries for quantitative variables that are grouped by whether they are green or not
test_data |> 
  group_by(green) |>
  summarize(
    mean_adj_bytes = mean(adjustedBytes, na.rm = T),
    sd_adj_bytes = sd(adjustedBytes, na.rm = T),
    mean_energy = mean(energy, na.rm = T),
    sd_energy = sd(energy, na.rm = T),
    mean_co2_grid_grams = mean(co2_grid_grams, na.rm = T),
    sd_co2_grid_grams = sd(co2_grid_grams, na.rm = T),
    mean_co2_renew_grams = mean(co2_renew_grams, na.rm = T),
    sd_co2_renew_grams = sd(co2_renew_grams, na.rm = T)
  ) 

## Create numerical summaries grouped by rating
test_data |> 
  group_by(rating) |>
  summarize(
    mean_adj_bytes = mean(adjustedBytes, na.rm = T),
    sd_adj_bytes = sd(adjustedBytes, na.rm = T),
    mean_energy = mean(energy, na.rm = T),
    sd_energy = sd(energy, na.rm = T),
    mean_co2_grid_grams = mean(co2_grid_grams, na.rm = T),
    sd_co2_grid_grams = sd(co2_grid_grams, na.rm = T),
    mean_co2_renew_grams = mean(co2_renew_grams, na.rm = T),
    sd_co2_renew_grams = sd(co2_renew_grams, na.rm = T)
  )

#Plots--------------------------------------------------------------------------
