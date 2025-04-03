##Install the needed packages -----------------
install.packages("RSelenium")
install.packages("wdman")
install.packages("jsonlite")
install.packages("httr")
install.packages("rvest")
install.packages("dplyr")
install.packages("xml2")

##Loading the Necessary packages -----------------
library(RSelenium)
library(wdman)
library(jsonlite)
library(httr)
library(rvest)
library(dplyr)
library(xml2)
library(rvest)
library(rvest)
library(dplyr)

# Create an empty dataframe to store all car details
all_car_data <- data.frame(
  Brand = character(),
  Model = character(),
  Year = character(),
  Price = character(),
  EngineSize = character(),
  Gear = character(),
  Mileage = character(),
  FuelType = character(),
  Doors = character(),
  Description = character(),
  stringsAsFactors = FALSE
)

# Define base URL
base_url <- "https://www.theaa.com"
# Pagination Handling
base_url2 <- "https://www.theaa.com/used-cars/displaycars?fullpostcode=&travel=2000&page="

current_page <- 1 

url <- paste0(base_url2, current_page)
page_content <- read_html(url)

links <- page_content %>%
  html_nodes("#vl-list-container > div.content-column > div.vl-list > div > a") %>%
  html_attr("href")
# Adjust as needed
while (current_page >= 1) {
  
  
  # Function to scrape car details from a given link
  scrape_car_details <- function(url) {
    pagelink <- tryCatch(read_html(url), error = function(e) NULL)
    
    if (is.null(links)) {
      print(paste("Failed to load:", url))
      return(NULL)
    }
    
    car_brand <- pagelink %>% html_node("h1 > span.make") %>% html_text(trim = TRUE) %>% ifelse(is.na(.), "Unknown", .)
    car_model <- pagelink %>% html_node("div.vehicle-name h1 > span.model") %>% html_text(trim = TRUE) %>% ifelse(is.na(.), "", .)
    car_variant <- pagelink %>% html_node("div.vehicle-name h1 > span.variant") %>% html_text(trim = TRUE) %>% ifelse(is.na(.), "", .)
    car_modell <- paste(car_model, car_variant)  
    
    car_year <- pagelink %>% html_node("section.left-col.specs-panel > div > ul > li:nth-child(2) > span.vd-spec-label > span") %>% html_text(trim = TRUE) %>% ifelse(is.na(.), "Unknown", .)
    engine_size <- pagelink %>% html_node("section.left-col.specs-panel > div > ul > li:nth-child(8) > span.vd-spec-label > span") %>% html_text(trim = TRUE) %>% ifelse(is.na(.), "Unknown", .)
    car_gear <- pagelink %>% html_node("section.left-col.specs-panel > div > ul > li:nth-child(4) > span.vd-spec-label > span") %>% html_text(trim = TRUE) %>% ifelse(is.na(.), "Unknown", .)
    car_petrol <- pagelink %>% html_node("section.left-col.specs-panel > div > ul > li:nth-child(3) > span.vd-spec-label > span") %>% html_text(trim = TRUE) %>% ifelse(is.na(.), "Unknown", .)
    car_mileage <- pagelink %>% html_node("section.left-col.specs-panel > div > ul > li:nth-child(1) > span.vd-spec-label > span") %>% html_text(trim = TRUE) %>% ifelse(is.na(.), "Unknown", .)
    car_door <- pagelink %>% html_node("section.left-col.specs-panel > div > ul > li:nth-child(7) > span.vd-spec-label > span") %>% html_text(trim = TRUE) %>% ifelse(is.na(.), "Unknown", .)
    car_price <- pagelink %>% html_node(".price .total-price") %>% html_text(trim = TRUE) %>% ifelse(is.na(.), "Unknown", .)
    car_desc <- paste(car_brand, car_modell, car_year, engine_size, car_petrol, car_gear, car_door)
    
    # Return data as a dataframe
    return(data.frame(
      Brand = car_brand,
      Model = car_modell,
      Year = car_year,
      Price = car_price,
      EngineSize = engine_size,
      Gear = car_gear,
      Mileage = car_mileage,
      FuelType = car_petrol,
      Doors = car_door,
      Description = car_desc,
      stringsAsFactors = FALSE
    ))
  }
  
  cat("Scraping page", current_page, "\n")
  
  links <- tryCatch({
    read_html(url) %>%
      html_nodes("#vl-list-container > div.content-column > div.vl-list > div > a") %>%
      html_attr("href")
  }, error = function(e) {
    message("Error scraping page ", current_page, ": ", e$message)
    
    return(NULL)
  })
  
  ###############################################
  car_data <- tryCatch({
    scrape_car_details(url)
  }, error = function(e) {
    message("Error scraping page ", current_page, ": ", e$message)
    return(NULL)
  })
  ################################################
  
  # If no links found, stop pagination
  if (is.null(links) || length(links) == 0) {
    break
  }
  
  # Scrape each car page from the pagination
  #for (link in links) {
  # car_data <- scrape_car_details(paste0(base_url, link))
  
  #if (!is.null(car_data)) {
  # all_car_data <- bind_rows(all_car_data, car_data)
  #    }
  # }
  
  
  # Scrape data from individual car listing pages
  for (link in links) {
    car_data <- scrape_car_details(paste0(base_url, link))
    
    if (!is.null(car_data)) {
      all_car_data <- bind_rows(all_car_data, car_data)
      print(paste("Scraped:", car_data$Brand, car_data$Model, "-", car_data$Price))
    }
  }
  
  
  current_page <- current_page + 1
  url <- paste0(base_url2, current_page)
  page_content <- read_html(url)
  links <- page_content %>%
    html_nodes("#vl-list-container > div.content-column > div.vl-list > div > a") %>%
    html_attr("href")
  
  
  Sys.sleep(runif(10, 12, 15)) # Avoid overwhelming the server
}

# Print and save the final data
View(all_car_data)
write.csv(all_car_data, "AAcar_data.csv", row.names = FALSE)