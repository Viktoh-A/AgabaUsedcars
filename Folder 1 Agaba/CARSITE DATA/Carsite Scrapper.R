##Install the needed packages -----------------
install.packages("RSelenium")
install.packages("wdman")
install.packages("jsonlite")
install.packages("httr")
install.packages("rvest")
install.packages("dplyr")
install.packages("xml2")

##Laoding the Necessary packages -----------------
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

# Function to scrape car details from a single page
scrape_car_details <- function(url) {
  # Read the webpage
  webpage <- read_html(url)
  
  # Extract car details using CSS selectors
  car_name <- webpage %>%
    html_nodes("h3.avlink a") %>%
    html_text(trim = TRUE)  # Trim whitespace
  car_brand <- gsub("^([[:alnum:]]+).*", "\\1", car_name)
  
   
  car_model <- gsub("^[[:alnum:]]+\\s*", "", car_name)
  
  
  
  car_year <- webpage %>%
    html_nodes("span.tal") %>%
    html_text(trim = TRUE)
  trimcar_year <- gsub("^(\\d{4}).*", "\\1", car_year)  # Match one or more digits
  
  car_year <- trimcar_year
  
  car_price <- webpage %>%
    html_nodes("h4.advertprice.fl.tac") %>%
    html_text(trim = TRUE)
    
  engine_size <- webpage %>%
    html_nodes("div.section28.fl.mr1p:nth-child(5)") %>%
    html_text(trim = TRUE)
  # Use a regular expression to extract numeric values (e.g., "1600" or "2000")
  trimengine_size <- as.integer(gsub(".{2}$", "", engine_size))/1000  # Match one or more digits
  engine_size <- round(trimengine_size, 2)

  
  car_gear <- webpage %>%
    html_nodes("div.section28.fl.mr1p:nth-child(1)") %>%
    html_text(trim = TRUE)
  
  car_mileage <- webpage %>%
    html_nodes("div.section28.fl:nth-child(2)") %>%
    html_text(trim = TRUE)
  
  car_petrol <- webpage %>%
    html_nodes("div.section28.fl:nth-child(4)") %>%
    html_text(trim = TRUE)
  
  car_door <- webpage %>%
    html_nodes("div.section28.fl:nth-child(6)") %>%
    html_text(trim = TRUE)
  
  car_desc <- webpage %>%
    html_nodes("div.section.f-12.pos-rel span.tal") %>%
    html_text(trim = TRUE)
  
  # Combine all extracted data into a data frame
  car_data <- data.frame(
    Brand = car_brand,
    Model = car_model,
    Year = car_year,
    Price = car_price,
    EngineSize = engine_size,
    Gear = car_gear,
    Mileage = car_mileage,
    FuelType = car_petrol,
    Doors = car_door,
    Description = car_desc,
    stringsAsFactors = FALSE
  )
  
  return(car_data)
}

# Initialize an empty data frame to store all car details
all_car_data <- data.frame(
  Brand = character(),
  Model = character(),
  Year = character(),
  Price = character(),
  EngineSize = integer(),
  Gear = character(),
  Mileage = character(),
  FuelType = character(),
  Doors = character(),
  Description = character(),
  stringsAsFactors = FALSE
)

# Base URL and pagination setup
base_url <- "https://www.carsite.co.uk/used-car"
current_page <- 1
#current_page <= 1


# Scrape multiple pages
while (current_page >= 1) {
  # Construct the URL for the current page
  url <- paste0(base_url, "/page/", current_page)
  
  # Print progress
  cat("Scraping page", current_page, "\n")
  
  # Scrape car details from the current page
  car_data <- tryCatch({
    scrape_car_details(url)
  }, error = function(e) {
    message("Error scraping page ", current_page, ": ", e$message)
    return(NULL)
  })
  
  # Append the scraped data to the main data frame if successful
  if (!is.null(car_data)) {
    all_car_data <- bind_rows(all_car_data, car_data)
  }
  
  # Move to the next page
  current_page <- current_page + 1
  
  # Sleep to avoid overwhelming the server
  Sys.sleep(3)
}

# View the final data frame
View(all_car_data)
# Assuming car_data is your data frame
write.csv(all_car_data, "C:/Users/-/OneDrive - University of Bolton/Documents/selenium-r-demo/car_data.csv", row.names = FALSE, quote = TRUE)
