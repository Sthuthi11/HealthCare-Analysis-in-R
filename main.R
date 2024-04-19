main_menu <- function() {
  while (TRUE) {
    cat("\nMENU:\n")
    cat("1. Visualization of corona dataset\n")
    cat("2. Regression models of cancer dataset\n")
    cat("3. Refit using mental health dataset\n")
    cat("4. Exit\n")
    
    choice <- as.numeric(readline("Enter your choice: "))
    
    if (choice == 1) {
      source("plot.R")
    } else if (choice == 2) {
      source("regression.R")
    } else if (choice == 3) {
      source("mentalhealth.R")
    } else if (choice == 4) {
      cat("Exiting the program. Thank you!\n")
      break
    } else {
      cat("Invalid choice. Please enter a number from 1 to 4.\n")
    }
  }
}

# Run the main menu
main_menu()
