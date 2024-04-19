dataset <- read.csv("C:/Users/hp/Desktop/pragathi/cancer.csv")


# Function for linear regression using extracted data
linear_regression_plot <- function(dataset) {
  plot(dataset$Age, dataset$Genetic.Risk,
       main = "Linear Regression : Age Vs Genetic Risk",
       ylim = c(1, 8),
       ylab = "Genetic Risk", xlab = "Age",
       col = "darkgreen", pch = 19)
  abline(lm(Genetic.Risk ~ Age, data = dataset), col = "black")
}
  while (TRUE) {
    print("Regression Submenu (Cancer dataset):")
    print("1. Linear Regression")
    print("2. Back to Main Menu")
    
        sub_choice <- as.numeric(readline("Enter your regression choice (1-2): "))
        if (sub_choice == 2) {
          print("Returning to main menu")
          break
        } else if (sub_choice == 1) {
          linear_regression_plot(dataset)
          
        } else {
          print("Invalid choice. Please enter a valid regression option.")
        }
  }
  
