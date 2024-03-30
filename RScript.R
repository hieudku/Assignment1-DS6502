
######################################## 
#### Adya Q4- heatmaps ####

# Installing the required package
install.packages("reshape2")

# Loading from the library
library(reshape2)

# Preparing the dataset
dataset.melt <- melt(dataset.df)

# Calculating average time spent by gender
avg_time_spent <- aggregate(time_spent ~ gender, dataset.df, mean)

# Convert the data to long format for the heatmap
avg_time_spent_long <- reshape2::melt(avg_time_spent, id.vars = "gender")

# Creating a heatmap
ggplot(avg_time_spent_long, aes(x = gender, y = variable, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "yellow", high = "blue") +
  labs(title = "Heatmap of Average Time Spent by Gender",
       x = "Gender",
       y = "Time Spent",
       fill = "Average Time Spent")

# Creating a histogram for each numerical column in the dataset
for(col in names(dataset.df)) {
  # Check if the column is numerical
  if(is.numeric(dataset.df[[col]])) {
    # Create a histogram
    p <- ggplot(dataset.df, aes_string(col)) + 
      geom_histogram(binwidth = 10, fill = "green", color = "black") +
      labs(title = paste("Histogram of", col),
           x = col,
           y = "Frequency")
    print(p)
  }
}

######################################