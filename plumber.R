#
# This is a Plumber API. You can run the API by clicking
# the 'Run API' button above.
#
# Find out more about building APIs with Plumber here:
#
#    https://www.rplumber.io/
#

library(plumber)
library(httr)
library(jsonlite)
library(dplyr)
library(purrr)

#* @apiTitle Countries API 
#* @apiDescription Dataengineering Test Exam

# Function to get data from online API
get_country_data <- function() {
  res <- GET("https://restcountries.com/v2/all")
  fromJSON(rawToChar(res$content), flatten = TRUE)
}

# Save the data so we not call API every time
countries <- get_country_data()

#* Return number of countries that speak Spanish, English and French
#* @get /languages
function() {
  # Check if 'languages' column exists
  if (!"languages" %in% names(countries)) {
    return(list(error = "languages column not found"))
  }
  langs_list <- countries$languages
  # Use purrr to go through the list of languages
  lang_names <- purrr::map(langs_list, ~ {
    if (is.null(.x)) return(NA)
    if (is.data.frame(.x)) return(.x$name)
    unlist(lapply(.x, `[[`, "name"))
  }) |> unlist()
  # Filter the ones we care about
  filtered <- lang_names[lang_names %in% c("Spanish", "English", "French")]
  # Count them
  as.list(table(filtered))
}

#* Return region with most countries and show how many per region
#* @get /region
function() {
  # Remove empty and NA regions
  region_data <- countries |> 
    filter(!is.na(region) & region != "")
  # Count regions
  region_counts <- table(region_data$region)
  # Find region with most countries
  top_region <- names(which.max(region_counts))
  list(
    region_with_most_countries = top_region,
    counts = as.list(region_counts)
  )
}

#* Return top 10 most and least populated countries
#* @get /population
function() {
  df <- countries |> filter(!is.na(population))
  top10 <- df |> 
    arrange(desc(population)) |> 
    slice_head(n = 10) |> 
    select(name, population)
  bottom10 <- df |> 
    arrange(population) |> 
    slice_head(n = 10) |> 
    select(name, population)
  list(
    top10_most_populated = top10,
    bottom10_least_populated = bottom10
  )
}

#* I show some info about Albania
#* Like population, region and what language they speak
#* @get /albania
function() {
  albania <- countries |> 
    filter(name == "Albania") |> 
    select(name, population, area, region, capital, languages)
  list(
    message = "Info about Albania 🇦🇱",
    country = albania
  )
}

#* @serializer png
#* @get /plot/population
function() {
  df <- countries |> 
    filter(!is.na(population)) |> 
    arrange(desc(population)) |> 
    slice_head(n = 10)
  
  barplot(df$population, names.arg = df$name, las = 2,
          col = "steelblue", main = "Top 10 populations", cex.names = 0.7)
}

#* Plot Albania population
#* @serializer png
#* @get /plot/albania
function() {
  
  alb_pop <- countries |> 
    filter(name == "Albania") |> 
    pull(population)
  
  
  barplot(alb_pop,
          names.arg = "Albania",
          col = "red",
          main = "Population of Albania 🇦🇱",
          ylab = "Population")
}
