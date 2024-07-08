# The required datasets:
corona_prediction_dataset <- read.csv("C:/Users/hp/Desktop/pragathi/corona.csv")

# Function to replace null values in the corona dataset without printing a message
replace_nulls <- function(corona_dataset, replacement_value = 0) {
  # Replace null values with a specified value (default is 0)
  corona_dataset[is.na(corona_dataset)] <- replacement_value
  return(corona_dataset)
}

# Function for histogram using extracted data with total cases and deaths by continent
histogram_plot <- function(corona_prediction_dataset) {
  # Replace null values in the 'Deaths' and 'Cases' columns with 0
  corona_prediction_dataset$Deaths[is.na(corona_prediction_dataset$Deaths)] <- 0
  corona_prediction_dataset$Cases[is.na(corona_prediction_dataset$Cases)] <- 0
  
  # Aggregate total deaths and cases by continent
  continent_totals <- aggregate(cbind(Cases, Deaths) ~ Continent, data = corona_prediction_dataset, sum)
  
  # Set options to display y-axis labels in numeric format
  options(scipen = 10)
  
  # Create a histogram for deaths with respect to cases by continent
  barplot(height = t(as.matrix(continent_totals[, c("Cases", "Deaths")])),
          beside = TRUE,
          names.arg = continent_totals$Continent,
          col = c('blue', 'red'),
          main = 'Histogram of Cases and Deaths by Continent',
          xlab = 'Continent',
          ylab = 'Count',
          legend.text = c('Cases', 'Deaths'),
          args.legend = list(x = 'topright'))
}

# Function for bar plot using extracted data grouped by continent
bar_plot <- function(corona_prediction_dataset) {
  # Replace null values before plotting
  corona_prediction_dataset <- replace_nulls(corona_prediction_dataset)
  
  # Convert Deaths to numeric, handling non-numeric values
  corona_prediction_dataset$Deaths <- as.numeric(as.character(corona_prediction_dataset$Deaths))
  
  # Check if 'Deaths' is a numeric column
  if (any(is.na(corona_prediction_dataset$Deaths))) {
    cat("Error: 'Deaths' column contains non-numeric values.\n")
    return()
  }
  
  # Aggregate total deaths by continent
  continent_totals <- aggregate(Deaths ~ Continent, data = corona_prediction_dataset, sum)
  
  # Manually specify colors for each continent
  colors <- c("red", "yellow", "blue", "pink", "green", "orange", "violet")
  continent_names <- c("Antarctica", "Asia", "Africa", "Europe", "North America", "South America", "Australia")
  
  # Order the continents based on total deaths
  order_indices <- order(continent_totals$Deaths, decreasing = TRUE)
  continent_names <- continent_names[order_indices]
  colors <- colors[order_indices]
  
  # Create a bar plot for total deaths in each continent
  barplot(continent_totals$Deaths[order_indices], 
          names.arg = continent_names,
          main = "Total Deaths by Continent", 
          ylab = "Total Deaths",
          col = colors,
          legend.text = TRUE,
          args.legend = list(x = "topright", bty = "n", inset = c(0, 0.2)))
  # Print total deaths for each continent
  cat("Total Deaths by Continent:\n")
  for (i in seq_along(continent_names)) {
    cat(continent_names[i], ": ", continent_totals$Deaths[order_indices][i], "\n")
  }
}

scatter_plot <- function(corona_prediction_dataset) {
  # Replace null values before plotting
  corona_prediction_dataset <- replace_nulls(corona_prediction_dataset)
  
  # Define colors for each continent
  continent_colors <- c("Asia" = "green", "Africa" = "pink", "Europe" = "red", 
                        "North America" = "blue", "South America" = "yellow",
                        "Australia" = "orange", "Antarctica" = "violet")
  
  # Create a scatter plot with different colors for each continent
  plot(corona_prediction_dataset$Cases, 
       corona_prediction_dataset$Deaths,
       main = "Scatter Plot Of Covid Data", 
       xlab = "Cases", ylab = "Deaths",
       xlim = c(0, 1000), ylim = c(0, 100),
       pch = 19, col = continent_colors[corona_prediction_dataset$Continent])
  options(scipen = 10) #to remove scientific notation
}

# Displaying plot sub-menu
plot_submenu <- function() {
  print("Plot Submenu (Covid Dataset):")
  print("1. Bar Plot")
  print("2. Scatter Plot")
  print("3. Histogram")
  print("4. Exit")
}

while (TRUE) {
  plot_submenu()
  choice <- suppressWarnings(as.numeric(readline("Your choice (1-4): ")))
  
  if (!is.na(choice) && choice %in% 1:4) {
    if (choice == 1) {
      bar_plot(corona_prediction_dataset)
    } else if (choice == 2) {
      scatter_plot(corona_prediction_dataset)
    } else if (choice == 3) {
      histogram_plot(corona_prediction_dataset)
    } else if (choice == 4) {
      cat("Exiting the program...\n")
      break
    }
  } else {
    cat("Invalid choice. Please enter a number between 1 and 4\n")
  }
}
