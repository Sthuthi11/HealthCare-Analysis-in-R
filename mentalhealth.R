# Function to display the main menu
main_menu <- function() {
  cat("Main Menu:\n")
  cat("1. Add a Row\n")
  cat("2. Add a Column\n")
  cat("3. Add Elements\n")
  cat("4. Delete a Row\n")
  cat("5. Delete a Column\n")
  cat("6. Delete Elements\n")
  cat("7. Display Data\n")
  cat("8. Exit\n")
}

# Read the CSV file
data <- read.csv("C:/Users/hp/Desktop/pragathi/MentalHealth.csv")  # Replace with your file path

while (TRUE) {
  main_menu()
  choice <- as.numeric(readline("Your choice (1-8): "))
  
  if (!is.na(choice) && choice %in% 1:8) {
    if (choice == 1) {
      # Add a Row
      new_row <- strsplit(readline("Enter new row (comma-separated values): "), ",")[[1]]
      data <- rbind(data, new_row)
      cat("Row added successfully!\n\n")
    } else if (choice == 2) {
      # Add a Column
      new_column_name <- readline("Enter new column name: ")
      new_column_values <- as.list(strsplit(readline("Enter values for the new column (comma-separated values): "), ",")[[1]])
      data[[new_column_name]] <- new_column_values
      cat("Column added successfully!\n\n")
    } else if (choice == 3) {
      # Add Elements
      row_index <- as.numeric(readline("Enter row index for adding elements: "))
      col_name <- readline("Enter column name for adding elements: ")
      new_elements <- strsplit(readline("Enter elements to add (comma-separated values): "), ",")[[1]]
      data[row_index, col_name] <- new_elements
      cat("Elements added successfully!\n\n")
    } else if (choice == 4) {
      # Delete a Row
      row_index <- as.numeric(readline("Enter row index to delete: "))
      data <- data[-row_index, ]
      cat("Row deleted successfully!\n\n")
    } else if (choice == 5) {
      # Delete a Column
      col_name <- readline("Enter column name to delete: ")
      data[[col_name]] <- NULL
      cat("Column deleted successfully!\n\n")
    } else if (choice == 6) {
      # Delete Elements
      row_index <- as.numeric(readline("Enter row index for deleting elements: "))
      col_name <- readline("Enter column name for deleting elements: ")
      data[row_index, col_name] <- NA
      cat("Elements deleted successfully!\n\n")
    } else if (choice == 7) {
      # Display Data
      print("Current Data:")
      print(data)
      cat("\n")
    } else if (choice == 8) {
      # Exit
      cat("Exiting the program...\n")
      break
    }
  } else {
    cat("Invalid choice. Please enter a number between 1 and 8\n\n")
  }
}

