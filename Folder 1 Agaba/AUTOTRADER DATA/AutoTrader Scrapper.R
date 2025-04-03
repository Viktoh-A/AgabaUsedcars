##Install the needed packages -----------------
install.packages("RSelenium")
install.packages("wdman")
install.packages("jsonlite")
install.packages("httr")
install.packages("rvest")
install.packages("dplyr")

##Laoding the Necessary packages -----------------
library(RSelenium)
library(wdman)
library(jsonlite)
library(httr)
library(rvest)
library(dplyr)

packageVersion("RSelenium")

# Define Firefox options (Capabilities)
browser_capabilities <- list(
  browserName = "firefox",
  "moz:firefoxOptions" = list(
    args = c('--headless')  # Run Firefox in headless mode (optional)
  )
)

# Start the Selenium Server
selenium_server <- rsDriver(
  browser = "firefox",
  geckover = "latest",
)


# get the driver client object
driver <- selenium_server$client
#driver$open()

# navigate to the destination page
driver$navigate("https://www.autotrader.co.uk/car-search?postcode=BL36BA&advertising-location=at_cars&page=")

Sys.sleep(5)  # Wait for the page to load

all_product_links <- character(0)  # Initialize an empty vector to store all links
num_pages <- 100  # Define the number of pages to iterate through

for (page in 1:num_pages) {  
  message(paste("Scraping page", page, "of", num_pages))
  
  # Gradually scroll to the bottom of the page
  for (scroll in 1:5) {  
    driver$executeScript("window.scrollBy(0, document.body.scrollHeight / 5);")
    Sys.sleep(5)  # Adjust sleep time if needed
  }
  
  # Ensure the page is fully loaded before extraction
  Sys.sleep(5)
  
  # Find all product links
  product_elements <- tryCatch({
    driver$findElements(using = "css selector", "li.at__sc-mddoqs-1.ieAGGj a[href^='/car-details/'], .at__sc-14bwrgm-3.gULWOb[href]")
  }, error = function(e) NULL)
  
  # Extract URLs from elements safely
  if (!is.null(product_elements) && length(product_elements) > 0) {
    product_links <- unique(unlist(lapply(product_elements, function(elem) elem$getElementAttribute("href"))))
    all_product_links <- unique(c(all_product_links, product_links))  # Append unique links
    message(paste("Found", length(product_links), "new links on page", page))
  } else {
    message(paste("No links found on page", page))
  }
  
  # Scroll back to the top (if necessary before loading the next set)
  driver$executeScript("window.scrollTo(0, 0);")
  Sys.sleep(5)  # Adjust pause time if needed
}
all_product_links <- unique(all_product_links)
print(all_product_links)



# Initialize an empty data frame to store all car details
product_data <- data.frame(
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

for (link in all_product_links) {
  driver$navigate(link)
  Sys.sleep(runif(5, 7, 10))  # Random delay to avoid bot detection
  
  # Extract product details safely
  car_brand <- tryCatch({
    name_element <- driver$findElement(using = "css selector", "h1[data-testid='advert-title']")
    name_element$getElementText()[[1]]
  }, error = function(e) NA)
  car_brand<- gsub("^(\\S+).*", "\\1", car_brand)
  
  car_model <- tryCatch({
    price_element <- driver$findElement(using = "css selector", "h1[data-testid='advert-title']")
    price_element$getElementText()[[1]]
  }, error = function(e) NA)
  car_model<- gsub("^\\S+\\s", "", car_model)
  
  car_year <- tryCatch({
    description_element <- driver$findElement(using = "css selector", "dl[data-gui='description-list-column'] > div.at__sc-6lr8b9-2:nth-child(2) > span.value_details")
    description_element$getElementText()[[1]]
  }, error = function(e) NA)
  
  
  car_price <- tryCatch({
    description_element <- driver$findElement(using = "css selector", "h2[data-testid='advert-price']")
    description_element$getElementText()[[1]]
  }, error = function(e) NA)
  
  engine_size <- tryCatch({
    description_element <- driver$findElement(using = "css selector", "dl.at__sc-6lr8b9-1:nth-child(2) > div:nth-child(1) > span:nth-child(2)")
    description_element$getElementText()[[1]]
  }, error = function(e) NA)
  #engine_size <- engine_size[str_detect(engine_size, "^\\d+\\.\\d+L$")]
  
  car_gear <- tryCatch({
    description_element <- driver$findElement(using = "css selector", "li.at__sc-1n64n0d-9:nth-child(3)")
    description_element$getElementText()[[1]]
  }, error = function(e) NA)
  
  car_mileage <- tryCatch({
    description_element <- driver$findElement(using = "css selector", "dl > div:nth-child(1) > span.value_details > div.at__sc-efqqw2-2")
    description_element$getElementText()[[1]]
  }, error = function(e) NA)
  
  car_petrol <- tryCatch({
    description_element <- driver$findElement(using = "css selector", "li.at__sc-1n64n0d-9:nth-child(4)")
    description_element$getElementText()[[1]]
  }, error = function(e) NA)
  
  car_door <- tryCatch({
    description_element <- driver$findElement(using = "css selector", "dl.at__sc-6lr8b9-1:nth-child(2) > div:nth-child(3) > span:nth-child(2)")
    description_element$getElementText()[[1]]
  }, error = function(e) NA)
  
  car_desc <- tryCatch({
    description_element <- driver$findElement(using = "css selector", "p[data-testid='advert-subtitle']")
    description_element$getElementText()[[1]]
  }, error = function(e) NA)
  
  # Append data
  product_data <- rbind(product_data, data.frame(
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
  ))
}


# Print the final data frame
print(car_data)
View(product_data)
# Assuming car_data is your data frame
write.csv(product_data, "C:/Users/-/OneDrive - University of Bolton/Documents/selenium-r-demo/autoTrader_data.csv", row.names = FALSE, quote = TRUE)
# close the Selenium client and the server
driver$close()
selenium_server$server$stop()
