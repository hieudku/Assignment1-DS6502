# Get the current working directory
getwd()

# Install package ggplot2 package for creating graphics and plots
install.packages("ggplot2")
# Install package dplyr that used for the manipulation and display of data within a data frame
install.packages("dplyr") 

# Import package - to be added more if needed
library(ggplot2)
library(dplyr)

# Import dataset
dataset.df <- read.csv("./Excel File/TimeOnSocialMedia.csv")

# Test a few first line of imported file
head(dataset.df)
# Test the last few line of imported file
tail(dataset.df)

########################### Q2 - Basic descriptive Statistics #######################################

#### Time spent column ####

# Calculated the mean of the 'time spent' value
Time_spent_Mean <- mean(dataset.df$time_spent)

#Calculated the median of the 'time spent' value
Time_spent_Median <- median(dataset.df$time_spent)

#Calculated the Standard deviation of the 'time spent' value 
Time_spent_Standard_deviation <- sd(dataset.df$time_spent)

# Calculated the range of the 'time spent' value
Time_spent_range <- range(dataset.df$time_spent)

# Calculates the minimum values of the 'time_spent' value
min_time_spent <- min(dataset.df$time_spent)
# Calculates the maximum values of the 'time_spent' value
max_time_spent <- max(dataset.df$time_spent)
                      
# Calculates the quartiles of the 'time spent' value
Time_spent_quartiles <- quantile(dataset.df$time_spent)


# Use summary() function to double check with the result
summary(dataset.df$time_spent)

############################### Q4 - Plot Graphs ########################################################################

###### Average Time Spent vs Platform ########## 

# Calculate the mean 'time_spent' for each unique value of the 'platform' variable.
platform_time_spent <- aggregate(time_spent ~ platform, data = dataset.df, FUN = mean)

#----------------------- BAR PLOT VS PLATFORM ---------------------#
# Create a bar plot showing the average time spent on social media by platform, with the bars colored according to the platform. 

# Sets 'platform' as the x-axis variable, 'time_spent' as the y-axis variable, and 'platform' as the fill color aesthetic.
ggplot(platform_time_spent, aes(x = platform, y = time_spent, fill = platform)) +
  # Adds a bar layer to the plot, where each bar represents the values of 'time_spent'
  # The stat = "identity" argument indicates that the height of the bars should correspond to the actual values in the data
  geom_bar(stat = "identity") + 
  # Sets the titles for the plot, x-axis, and y-axis
  labs(title = "Average Time Spent on Social Media by Platform",
       x = "Platform",
       y = "Time Spent (hours)") + 
  # Setting the plot title to be centered, changing the color, size, and style of the axis labels, and adjusting the size and style of the axis titles.
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), #center the title, front size to 20 and bold
                                         axis.text = element_text(colour = "brown", size = 10, face = "bold"), # Color the x and y label, front size to 10 and bold 
                                         axis.title.x = element_text(size = 15, face = "bold"), # Front size 15 and bold the x title
                                         axis.title.y = element_text(size = 15, face = "bold")) # Front size 15 and bold the y title

##########################################################

###### Average time spent VS Age ########## 

# Divide age data into 5 groups for cleaner graph
dataset.df$age_group <- cut(dataset.df$age, breaks = c(18, 28, 38, 48, 58, 64), include.lowest = TRUE, right = FALSE,
                            labels = c("18-27", "28-37", "38-47", "48-57", "58-64")) 

#----------------------- BOX PLOT VS AGE ---------------------#
# Create a box plot showing the average time spent on social media by age group, with the bars colored according to the age 

# Sets the x-axis to the age group (converted to a factor) and the y-axis to the time spent.
ggplot(dataset.df, aes(x = factor(dataset.df$age_group), y = time_spent)) +
  geom_boxplot(aes(fill = factor(dataset.df$age_group))) + # Fill color by age
  scale_fill_viridis_d() + # sets the color scale for the age groups.
  theme_minimal() + 
  # Sets the titles for the plot, x-axis, y-axis, and legend.
  labs(
    title = "Boxplot of Time Spent per Day by Age",
    x = "Age",
    y = "Time Spent",
    fill = "Age Group"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5), # Center the plot title
    axis.text.x = element_text(angle = 45, hjust = 1), 
    legend.position = "bottom" # Position the legend at the bottom
  )

##########################################################

###### Average Time Spent VS Gender ########## 

# to analyze if there are significant differences in social media usage patterns between different gender identities
#count the number of each time spent  value
time_spent_count <- table(dataset.df$time_spent)

#basic bar plot
barplot(time_spent_count)

#----------------------- BAR PLOT VS GENDER ---------------------#
# Create a bar plot showing the average time spent on social media by gender, with the bars colored according to the gender 

# Sets the x-axis to 'gender' and the y-axis to 'time_spent'.
average_plot <- ggplot(data= dataset.df, aes( x = gender,y = time_spent)) +
  # Summary statistic layer to the plot.
  # Calculates the mean of 'time_spent' for each level of 'gender' and represents it
  # Fill color of the bar
  stat_summary(fun = "mean", geom = "bar", fill = c("blue","red","orange"), color = "black", alpha = 0.6) +
  labs(title = "Average Time Spent by Gender", x = "Gender", y = "Average Time Spent") # Sets the titles for the plot, x-axis, and y-axis
average_plot

##########################################################

###### Total time spent VS Location and platform ##########

# Calculate total time spent by location and platform 
total_time_spent <- aggregate(time_spent ~ location + platform, data = dataset.df, sum)

#----------------------- STACKED BAR PLOT VS LOCATION AND PLATFORM ---------------------#
# Create a stacked bar plot showing the total time spent on social media by location and platform, with the bars colored according to each platform. 

# Sets the x-axis to 'location', the y-axis to 'time_spent', and uses different fill colors for each 'platform'.
stacked_bar_plot <- ggplot(data = total_time_spent, aes(x = location, y = time_spent, fill = platform)) + 
  # The heights of the bars represent the values in the 'time spent' variable directly, and position = "stack" stacks the bars for each 'location'.
  geom_bar(stat = "identity", position = "stack") +
  # Sets the titles for the plot, x-axis, and y-axis
  labs(title = "Total Time Spent by Location and Platform on Social Media",
       x = "Location", y = "Total Time Spent") +
  # Add text labels for total time spent on each bar
  geom_text(aes(label = paste(time_spent, "hours")), position = position_stack(vjust = 0.5)) # Positions the text labels in the middle of each stack of bars.

stacked_bar_plot

##########################################################

#### Average Time by Gender ####

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

#----------------------- HEATMAP VS GENDER ---------------------#
# Create a heatmap showing the average time spent by gender.The color intensity represents the average time spent, with yellow indicating lower values and blue indicating higher values.

# Sets the x-axis to 'gender', the y-axis to 'variable', and uses different fill colors for the 'value'.
ggplot(avg_time_spent_long, aes(x = gender, y = variable, fill = value)) +
  geom_tile(color = "white") + # Sets the color of the borders of the tiles to white.
  scale_fill_gradient(low = "yellow", high = "blue") + # Sets the gradient color scale for the fill color. Low values will be represented in yellow and high values will be represented in blue.
  # Sets the titles for the plot, x-axis, and y-axis, and sets the label for the fill legend.
  labs(title = "Heatmap of Average Time Spent by Gender",
       x = "Gender",
       y = "Time Spent",
       fill = "Average Time Spent")

##########################################################

#----------------------- HISTOGRAM VS EACH NUMERIC COLUMN  ---------------------#
# Create a histogram for each numeric column in the dataset.df data frame, providing visual representations of the distribution of values for each numerical variable.

# Loop iterates over each column name in the dataset.df data frame.
for(col in names(dataset.df)) {
  # Check if the column is numerical
  if(is.numeric(dataset.df[[col]])) {
    # Create a histogram
    p <- ggplot(dataset.df, aes_string(col)) + 
      # Adds a histogram layer to the plot with a binwidth of 1000, filled with green color and black border.
      geom_histogram(binwidth = 1000, fill = "green", color = "black") +
      scale_x_continuous(breaks = seq(min(dataset.df$income), max(dataset.df$income), by = 1000)) + # Range of the column values.
      labs(title = paste("Histogram of", col), # Sets the titles for the plot, x-axis, and y-axis
           x = col,
           y = "Frequency")
    print(p)
  }
}

######################################## 

#### Average Time Spent in different income groups by profession ####

# Divide income data into 10 groups for cleaner graph
dataset.df$income_group <- cut(dataset.df$income, breaks = c(10000, 11000, 12000, 13000, 14000,15000, 16000, 17000, 18000, 19000, 20000), include.lowest = TRUE, right = FALSE,
                               labels = c("10000-11000", "11000-12000", "12000-13000","13000-14000", "14000-15000","15000-16000", "16000-17000", "17000-18000", "18000-19000", "19000-20000")) 

#group a new dataset, %>% is a chain operator to chain together multiple operations
#The .groups = "drop" argument ensures that the grouping is dropped after summarization to avoid issues with plotting.
dataset1 <- dataset.df %>% group_by(profession, income_group) %>% summarize(avg_time = mean(time_spent), .groups = 'drop')

#----------------------- LINE PLOT VS TIME SPENT BY INCOME GROUP, CATEGORIZED BY PROFESSION   ---------------------#
# Create a line plot with points showing the average time spent on social media by income group, categorized by profession.

# Sets the x-axis to 'income_group', the y-axis to 'avg_time', and uses different colors for each 'profession'. 
ggplot(data = dataset1, mapping = aes(x=income_group, y=avg_time, color=profession, group = profession)) + 
      geom_line() + geom_point() + # Creates a line plot with points representing each data point.
  # Sets the titles for the plot, x-axis, and y-axis    
  labs(title = "Average Time Spent on Social Media by Income Group, by Profession",  x = "Income Group", y = "Average Time Spent") +
  # Rotates the labels by 45 degrees (angle = 45) and shifts them to the right (hjust = 1)
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  

##########################################################

#### Heatmap of timespent by Gender and Age ####

# Creates a subset of the original dataset (dataset.df) including only rows where the 'gender' column matches either "male", "female", or "non-binary".
dataset_subset <- subset(dataset.df, gender %in% c("male", "female", "non-binary"))

#-----------------------HEATMAP VS GENDER AND AGE BETWEEN HOMEOWNERS AND RENTERS   ---------------------#
# Creates a heatmap to shows the time spent by gender and age between homeowners and renters. 
# Single heatmap #
# Sets the x-axis to 'gender', the y-axis to 'age'
ggplot(data = dataset_subset, aes(x = gender , y = age)) +
  geom_tile(aes(fill = time_spent))+ # Creates the heatmap
  scale_fill_gradient(low = "white", high = "darkgreen") + # Set the color gradient from white to dark green
  # Sets the titles for the plot, x-axis, y-axis and legend.
  labs(title = "Heatmap of Time spent by Gender and Age between homeowners (true) and renters (false)",
       x = "Gender",
       y = "Age (years)",
       fill = "Time spent")

# Creates a set of heatmaps to shows the time spent by gender and age between homeowners and renters
# Sets the x-axis to 'gender', the y-axis to 'age'
ggplot(data = dataset_subset, aes(x = gender , y = age)) +
  geom_tile(aes(fill = time_spent))+ # Creates the heatmap
  scale_fill_gradient(low = "white", high = "darkgreen") + # Set the color gradient from white to dark green
  # Sets the titles for the plot, x-axis, y-axis and legend.
  labs(title = "Heatmap of Time spent by Gender and Age between homeowners (true) and renters (false)",
      x = "Gender",
      y = "Age (years)",
      fill = "Time spent") + 
  facet_wrap(~isHomeOwner) # The data will be split into subsets based on the 'isHomeOwner' variable, and each subset will be plotted separately in its own panel.

###############################################

######### Participants chart
# Count the number of participants from each country
participant_count <- table(dataset.df$location)

# Creates a data frame participant_count_df containing participant counts for each country
# Convert the participant count to a data frame
participant_count_df <- data.frame(
  country = names(participant_count),
  count = as.numeric(participant_count)
)

##########################################################

#-----------------------BAR CHART VS COUNTRY  ---------------------#
# Creates a bar plot to shows the number of participants by country.
# Sets the x-axis to 'country', the y-axis to 'count', and fill colors by country.
ggplot(participant_count_df, aes(x = country, y = count, fill = country)) +
  geom_bar(stat = "identity") + # Adds a bar layer to the plot
  theme_minimal() +
  # Sets the titles for the plot, x-axis, and y-axis
  labs(title = "Number of Participants by Country", x = "Country", y = "Number of Participants") +
  theme(legend.position = "none") # Removes the legend from the plot

##########################################################

######## Pie charts
# Calculate percentage of time spent for each location and platform
pie.df <- dataset.df %>%
  group_by(location, platform) %>%
  summarize(total_time = sum(time_spent)) %>%
  mutate(percentage = total_time / sum(total_time) * 100)

#-----------------------PIE CHART---------------------#
# Create a pie chart to shows the percentage of time spent on platforms by location
# sets 'percentage' as the y-axis variable and 'platform' as the fill aesthetic. Setting x = "" ensures that the pie chart is drawn with a single circle.
Time_Platform_Location_piechart <- ggplot(pie.df, aes(x = "", y = percentage, fill = platform)) +
  geom_bar(stat = "identity", color = "white") + # Sets the color of the bar outlines to white.
  coord_polar("y") +
  facet_wrap(~location) + # Facets the plot into multiple panels based on the 'location' variable.
  theme_void() + # Sets the theme of the plot to a completely blank canvas.
  theme(legend.position = "bottom") + # Positions the legend at the bottom of the plot.
  labs(title = "Percentage of Time Spent on Platforms by Location", fill = "Platform") +
  # Adds text labels to each segment of the pie chart, displaying the percentage values
  # Formats the percentage values to two decimal places with a '%' symbol.
  geom_text(aes(label = paste0(round(percentage, 2), "%")), position = position_stack(vjust = 0.5))

Time_Platform_Location_piechart

###############################################

#-----------------------NORMAL DISTRIBUTION PLOT---------------------#
# Generates a normal distribution plot for the 'time_spent' variable in the dataset
ggplot(dataset.df, aes(x = time_spent)) +
  # Adds a line representing the normal distribution to the plot.
  # The dnorm function is used to generate the normal distribution curve, with its mean and standard deviation specified using the mean and sd arguments, respectively. 
  stat_function(fun = dnorm, args = list(mean = Time_spent_Mean, sd = Time_spent_Standard_deviation), 
                color = "blue", size = 1.5) + # The line is colored blue and has a size of 1.5.
  # Adds a histogram layer to the plot, with density rather than count on the y-axis. 
  # The histogram has a binwidth of 1, black borders, light gray fill color, and 50% transparency.
  geom_histogram(aes(y = ..density..), binwidth = 1, color = "black", fill = "lightgray", alpha = 0.5) +
  scale_x_continuous(breaks = seq(min(dataset.df$time_spent), max(dataset.df$time_spent), by = 1)) + # show all time spent values on x (1-9)
  labs(x = "Time Spent", y = "Density") + # Sets the x-axis and y-axis labels.
  ggtitle("Normal Distribution Plot for Time Spent") # Sets the title of the plot.

# No clear sign of normal distribution > lack of trend and evenly distributed in time spent. Maybe include reasons for why we DONT add this to the repot instead of including it?

# Use this instead of Standard Deviation? - Histogram and density curve of Time Spent
# Note:  the density values on the y-axis of a density plot is the probabilty of observing data points at different values along the time spent x-axis

##########################################################

#-----------------------HISTOGRAM BY LOCATION---------------------#
# Generates a plot that combines a histogram with a density curve to visualize the distribution of 'time_spent' variable in the dataset, segmented by gender using faceting.
ggplot(dataset.df, aes(x = time_spent)) +
  geom_histogram(binwidth = 1, color = "black", aes(y = ..density.., fill = ..count..), alpha = 0.5) +
  geom_density(alpha = 1, color = "blue") + # Density curve, look at peak and tails
  labs(x = "Time Spent", y = "Density") + # Sets the x-axis and y-axis labels.
  scale_x_continuous(breaks = seq(min(dataset.df$time_spent), max(dataset.df$time_spent), by = 1)) + # show all time spent values on x (1-9)
  scale_fill_gradient(low = "green", high = "red") +  # Sets the gradient color scale for the fill color of the histogram bars.
  ggtitle("Histogram with Density Curve for Time Spent") + # Sets the title of the plot.
  
  facet_wrap(~ gender) # split into groups of genders

##########################################################

#-----------------------SINGLE HISTOGRAM---------------------#
# Generates a histogram with a density curve for the 'time_spent' variable
ggplot(dataset.df, aes(x = time_spent)) +
  geom_histogram(binwidth = 1, color = "black", aes(y = ..density.., fill = ..count..), alpha = 0.5) +
  geom_density(alpha = 1, color = "blue") + # Density curve, look at peak and tails
  labs(x = "Time Spent", y = "Density") + # Sets the x-axis and y-axis labels.
  scale_x_continuous(breaks = seq(min(dataset.df$time_spent), max(dataset.df$time_spent), by = 1)) + # show all time spent values on x (1-9)
  scale_fill_gradient(low = "green", high = "red") + # Sets the gradient color scale for the fill color of the histogram bars.
  ggtitle("Histogram with Density Curve for Time Spent") + # Sets the title of the plot.
  
  annotate("text", x = Inf, y = Inf, hjust = 1, vjust = 1, # Add descriptive statistics: mean, median etc to the histogram
           label = paste("Mean:", round(Time_spent_Mean, 2), "\n",
                         "Median:", round(Time_spent_Median, 2), "\n",
                         "SD:", round(Time_spent_Standard_deviation, 2), "\n",
                         "Min:", min_time_spent, "\n",
                         "Max:", max_time_spent))

##########################################################

#-----------------------SIMPLE BOX PLOT---------------------#
# Generates a boxplot of the 'time_spent' variable
timeSpent_boxplot <- ggplot(dataset.df, aes(y = time_spent)) +
  geom_boxplot() +  # Adds a boxplot layer to the plot
  geom_point(aes(x = 0),color = "red", size = 3) + # Adds a red point to represent the mean value of 'time_spent'.
  # Adds text annotations to display the actual 'time_spent' values.
  geom_text(aes(x = 0.05),label = dataset.df$time_spent, vjust = -0.5, size = 3) +
  scale_y_continuous(labels = scales::comma) +
  ylab("Time Spent") + #  Sets the y-axis label.
  ggtitle("Boxplot of time spent on social media") + # Sets the title of the plot
  
  # Adds text annotations for descriptive statistics
  annotate("text", x = 0.2, y = Inf, hjust = 0, vjust = 1,
           label = paste("Mean:", round(mean(dataset.df$time_spent), 2), "\n",
                         "Median:", round(median(dataset.df$time_spent), 2), "\n",
                         "SD:", round(sd(dataset.df$time_spent), 2), "\n",
                         "Min:", min(dataset.df$time_spent), "\n",
                         "Max:", max(dataset.df$time_spent)))


print(timeSpent_boxplot)
