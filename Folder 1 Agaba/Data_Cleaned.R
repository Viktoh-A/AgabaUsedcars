#Install the needed packages -----------------
# install.packages("RSelenium")
# install.packages("wdman")
# install.packages("jsonlite")
# install.packages("httr")
# install.packages("rvest")
# install.packages("dplyr")
# install.packages("tidyverse")
# install.packages("stringr")
# install.packages("ggplot2")

##Loading the Necessary packages -----------------
library(RSelenium)
library(wdman)
library(jsonlite)
library(httr)
library(rvest)
library(dplyr)
library(tidyverse)
library(readxl)
library(stringr)
library(scales)
library(ggplot2)


### LOADING SCRAPPED DATA ---------------
#Change the "setwd([type the directory for `Folder 1 Agaba` folder here])"
setwd("C:/Users/-/OneDrive - University of Bolton/Documents/selenium-r-demo/Team Alpha/Portfolio Files/Folder 1 Agaba")
getwd()


library(readr)
AAcar_data <- read.csv("AAcar_data.csv")
#View(AAcar_data)

autoTrader <- read.csv("autoTrader_data.csv")
#View(autoTrader)

CarSite_data <- read.csv("CarSite_data.csv")
#View(CarSite_data)

### COMBINING SCRAPPED DATA ----------------
combined_data <- rbind(AAcar_data, autoTrader, CarSite_data)

### DATA CLEANING ----------------

### Removing all columns where the is "-", "unknown", "Null", " " --------
# Define values to remove
remove_values <- c("-", "unknown", "Null", "", " ")

# Filter out rows containing unwanted values in any column
clean_data <- combined_data %>%
  filter(
    if_all(everything(), ~ !is.na(.) & . != "Unknown" & . != "-"))

### Removing all Rows where the EngizeSize is Manual ----------
clean_data <- clean_data %>%
  filter(!str_detect(EngineSize, "Manual"))

## YEAR COLUMN CLEANING ----------
# Find values in Year column with more than 4 characters

clean_data$Year <- gsub("^(\\d{4}).*", "\\1", clean_data$Year)

## PRICE COLUMN CLEANING ----------    
# Remove "Â£", "£", and "," from Price and convert to numeric
clean_data <- clean_data %>%
  mutate(Price = str_replace_all(Price, "[Â£,£,]", ""))

# Clean the Price column to keep everything up to the first space   
clean_data <- clean_data %>%
  mutate(Price = sub(" .*", "", Price))

## ENGINE SIZE COLUMN CLEANING ----------   
# Remove "L" from EngineSize 
clean_data <- clean_data %>%
  mutate(EngineSize = str_replace_all(EngineSize, "L", ""))

### Removing all Rows where the EngizeSize does not follow the format of engine size 
clean_data <- clean_data %>%
  filter(nchar(as.character(EngineSize)) < 5)

## GEAR COLUMN CLEANING ---------- 
# Remove "space" from values in gear column 
clean_data <- clean_data %>%
  mutate(Gear = str_replace_all(Gear, " ", ""))

## MILEAGE COLUMN CLEANING ---------- 
# Clean the Mileage column to keep everything up to the first space
clean_data <- clean_data %>%
  mutate(Mileage = sub(" .*", "", Mileage))

# Remove "comma" from values in Mileage column 
clean_data <- clean_data %>%
  mutate(Mileage = str_replace_all(Mileage, ",", ""))

## FUEL TYPE COLUMN CLEANING ----------  
# Clean the FuelType column to keep everything up to the first space and retain the values Petrol|Diesel|Hybrid|Electric
clean_data <- clean_data %>%
  mutate(FuelType = sub(" .*", "", FuelType)) 

## DOOR COLUMN CLEANING ----------
# Clean the Door column to keep everything up to the first space and retain the values 2|3|4|5|6
clean_data <- clean_data %>%
  mutate(Doors = sub(" .*", "", Doors))

# Removing all Rows where the Door is 0 
clean_data <- clean_data %>%
  filter(!str_detect(Doors, "0"))

## DESCRIPTION COLUMN CLEANING ----------
# Updating the Description Column
clean_data$Description <- paste(clean_data$Brand, clean_data$Model, clean_data$EngineSize, clean_data$Gear, clean_data$FuelType, clean_data$Doors, clean_data$Year )

## Converting the columns to adequate Data types-------------
clean_data$Brand <- as.character(clean_data$Brand)
clean_data$Model <- as.character(clean_data$Model)
clean_data$Year <- as.numeric(clean_data$Year)
clean_data$Price <- as.numeric(clean_data$Price)
clean_data$EngineSize <- as.numeric(clean_data$EngineSize)
clean_data$Gear <- as.character(clean_data$Gear)
clean_data$Mileage <- as.numeric(clean_data$Mileage)
clean_data$FuelType <- as.character(clean_data$FuelType)
clean_data$Doors <- as.numeric(clean_data$Doors)
clean_data$Description <- as.character(clean_data$Description)


## FEATURES ENGINEERING -----------------
# Populating the Car Age Column
clean_data$`Car Age` <- as.numeric(format(Sys.Date(), "%Y")) - clean_data$Year

# Populating the Price per mile Column
clean_data$`Price per Mile` <- round(clean_data$Price/clean_data$Mileage, 2)

# Populating the Engine Efficiency Column 
clean_data$`Engine Efficiency(mile/ltr)` <- round(clean_data$Mileage/clean_data$EngineSize, 2)


## EXPLORATORY DATA ANALYSIS ----------
full_clean_data <- clean_data
lendf <- length(full_clean_data$Brand )

## HANDLING OUTLIERS -----
# Compute Z-scores for Price and Mileage
z_scores_P <- scale(clean_data$Price)
z_scores_M <- scale(clean_data$Mileage)

# Define threshold for outliers
threshold <- 3

# Identify rows with outliers in Price or Mileage
outliers <- clean_data %>%
  filter(abs(z_scores_P) > threshold | abs(z_scores_M) > threshold)

# Create a new dataset with outliers removed
trimmed_data <- clean_data %>%
  filter(!(abs(z_scores_P) > threshold | abs(z_scores_M) > threshold))

## Use our trimmed dataset as the cleaned dataset for EDA
clean_data <- trimmed_data


# Price ---------
# Price Histogram
PriceHist <- function(dat){
  hist(dat, 
       main = "Price Distribution", 
       xlab = "Price", 
       ylab = "Frequency", 
       col = "purple",
       border = "grey",
       breaks = 100
  ) 
}

# PriceHist2 <- function(dat){
#   hist(dat, 
#        main = "Price Distribution", 
#        xlab = "Price", 
#        ylab = "Frequency", 
#        col = "purple",
#        border = "grey",
#        breaks = 50
#   ) 
# }

PriceHist2 <- function(dat) {
  # Set the background to black
  par(bg = "black")  
  
  # Create the histogram
  hist(dat, 
       main = "Price Distribution", 
       xlab = "Price", 
       ylab = "Frequency", 
       col = "#007bff",       # Bars color
       border = "white",     # Bar borders white for visibility
       breaks = 50,          # Number of bins
       col.main = "white",   # Title text color
       col.lab = "white",    # Label text color
       col.axis = "white"    # Axis text color
  ) 
  
  # Reset par settings (optional, to avoid affecting other plots)
  #par(bg = "white")  
}


# Price Summary Statistics
priceMin <- min(clean_data$Price) # Find the Minimum Car Price
priceMax <- max(clean_data$Price) # Find the Maximum Car Price 
priceAve <- round(mean(clean_data$Price),2)# Find the Average Car Price 
median(clean_data$Price) # Find the Middle Car Price
sd(clean_data$Price) # Find the Standard deviation Car Price

# Mileage Distribution ------
MileageHist <- function(dat) {
  hist(dat, 
       main = "Mileage Distribution", 
       xlab = "Miles", 
       ylab = "Frequency", 
       col = "purple",
       border = "grey",
       breaks = 50
  )
}

# Mileage Summary Statistics

 MileageMin <- round(min(clean_data$Mileage),2)  # Find the Minimum Car Mileage
 MileageMax <- round(max(clean_data$Mileage),2) # Find the Maximum Car Mileage
 MileageAve <- round(mean(clean_data$Mileage),2) # Find the Average Car Mileage
median(clean_data$Mileage) # Find the Middle Car Mileage
  sd(clean_data$Mileage) # Find the Standard deviation Car Mileage


# Grouping the Cars into Brands ------
Brand_grouped <- clean_data %>%
  group_by(Brand) %>%
  summarise(count = n())

# Car Brand Pie Chart
  blue_shades <- c(
    "#0000FF", "#0018A8", "#002366", "#0033AA", "#003F87", "#0047AB", "#00539C", "#0066CC", "#007FFF", "#008080",
    "#0095B6", "#00A4CC", "#00BFFF", "#1E90FF", "#2A52BE", "#4682B4", "#4A90E2", "#5B92E5", "#6495ED", "#6CA0DC",
    "#7EC8E3", "#87CEEB", "#89CFF0", "#8DB3E2", "#92A1CF", "#99CCFF", "#ADD8E6", "#B0C4DE", "#B3D9FF", "#BCD4E6",
    "#C1DFF0", "#CCECFF", "#DAF2FF", "#A2D5F2", "#77B5FE", "#4682B4", "#306EFF", "#2E8B57", "#1E90FF", "#00CED1",
    "#0088CE", "#007BA7", "#00509E", "#003366", "#001F54", "#01008A", "#0F52BA", "#1560BD", "#1B4F72", "#2E3192",
    "#4169E1", "#5B92E5", "#7DF9FF", "#87AFC7", "#8A9BCB", "#A9D8F8", "#9BBCC6"
  ) 
  
  
  
  
  
  # data <- data.frame(
  #   Category = c("Category A", "Category B", "Category C", "Category D"),
  #   Values = c(25, 30, 20, 25)
  # )
  
  # Create a pie chart with custom theme
  ggplot(Brand_grouped, aes(x = "", y = Brand_grouped$count, fill = Brand_grouped$Brand)) +
    geom_bar(stat = "identity", width = 1, color = "white") +
    coord_polar(theta = "y") +
    theme_void() +  # Removes unnecessary plot elements
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      legend.position = "right",
      legend.title = element_text(face = "bold"),
      legend.text = element_text(size = 12)
    ) +
    labs(
      title = "Customized Pie Chart",
      fill = "Categories"
    ) 
  
# Car Brand Distribution 
CarBrandPlot <- function(dat) {
  ggplot(dat, aes(x = "", y = count, fill = Brand)) +
    geom_bar(stat = "identity", width = 1, color = "white") +  # Create the pie slices
    coord_polar(theta = "y") +  # Convert to pie chart
    ggtitle("Car Brands Distribution") +
    theme_void() +  # Remove gridlines and axes
    scale_fill_manual(values = colorRampPalette(c("blue","#FF0666", "#007bff"))(length(unique(dat$Brand))))  # Blue to red gradient
}


# Engine Size Distribution ---------
EngineSize_grouped <- clean_data %>%
  group_by(EngineSize) %>%
  summarise(count = n())

# Engine Size Distribution 
EngineSizePlot <- function(dat) {
  ggplot(dat, aes(x = EngineSize, y = count, fill = EngineSize)) +
    geom_bar(stat = "identity", color = "black") +  # `stat = "identity"` because data is precomputed
    ggtitle("Engine Size Distribution") +
    xlab("Engine Size") +
    ylab("Frequency") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}

# Car Age Distribution ---------
CarAgePlot <- function(dat) {
  hist(dat, 
       main = "Car Age Distribution", 
       xlab = "Car Age", 
       ylab = "Frequency", 
       col = "purple",
       border = "grey",
       breaks = 50
  )
}

# Gear Distribution ---------
Gear_grouped <- clean_data %>%
  group_by(Gear) %>%
  summarise(count = n())

# Gear Distribution
GearDist <- function(dat) {
  ggplot(dat, aes(x = Gear, y = count, fill = Gear)) +
    geom_bar(stat = "identity", color = "black") +  # `stat = "identity"` because data is precomputed
    ggtitle("Gear Distribution") +
    xlab("Engine Size") +
    ylab("Frequency") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
}

# Scatter Plot of Mileage vs Price ------
SPlotMvP <- function(dat) {
  ggplot(dat, aes(x = Mileage, y = Price)) +
    geom_point(alpha = 0.6, color = "blue") +
    labs(title = "Price vs Mileage", x = "Mileage", y = "Price") +
    theme_minimal()
}


# Compute Correlation
pearson_corr <- cor(clean_data$Mileage, clean_data$Price, method = "pearson", use = "complete.obs")
spearman_corr <- cor(clean_data$Mileage, clean_data$Price, method = "spearman", use = "complete.obs")


  pearson_corrr <- cat("Pearson Correlation:", round(pearson_corr, 2))
  spearman_corrr <- paste("Spearman Correlation:", round(spearman_corr, 2),'&',"Pearson Correlation:", round(pearson_corr, 2))


##Price vs car age with Regression Line
GPlotPvC <- function(dat) {
  ggplot(dat, aes(x = `Car Age`, y = Price)) +
    geom_point(alpha = 0.6, color = "blue") +
    geom_smooth(method = "lm", color = "red", se = FALSE) +
    labs(title = "Price vs Age with Regression Line", x = "Age", y = "Price") +
    theme_minimal() 
}

##BOX PLOT for  brand and price
GPlotBvP <- function(dat) {
  # Aggregate data to get average price per brand
  dat_summary <- dat %>%
    group_by(Brand) %>%
    summarize(Average_Price = mean(Price, na.rm = TRUE)) 
  # Create bar chart
  ggplot(dat_summary, aes(x = reorder(Brand, Average_Price), y = Average_Price)) +
    geom_col(fill = "lightblue", color = "black") +
    scale_y_continuous(labels = scales::comma) +  
    labs(title = "Average Price by Car Brand", x = "Car Brand", y = "Average Price") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}



#View(clean_data)
# write.csv(clean_data, "C:/Users/HP/OneDrive - University of Bolton/Desktop/agaba/micode Agaba/OurCleaned_data.csv", row.names = FALSE, quote = TRUE)



