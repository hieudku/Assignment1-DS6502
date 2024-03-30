# frances branch new new 1234567

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

# frances branch new
# this is my branch 202403280


################################################################################

# Dont worry if you overwrite these codes by accident when pushing main, 
# I ll keep them on a backup branch as we go (Hieu)


getwd()

# do we even need to set wd lol? maybe not if it works on your R studio? (Hieu)

# Install package - to be added more if needed
install.packages("ggplot2")
install.packages("dplyr") # this will install the package in case you haven't got it thus the error (hieu)

# Import package - to be added more if needed
library(ggplot2)
library(dplyr) #when I run this line, given me error that there is no package called 'dplyr'(mia)

# Import dataset
dataset.df <- read.csv("./Excel File/TimeOnSocialMedia.csv")

# Test a few first line of imported file
head(dataset.df)
tail(dataset.df)

########################### Q2 - Basic descriptive Statistics #######################################

#### Time spent column ####

#Mean for time spent value
Time_spent_Mean <- mean(dataset.df$time_spent)
Time_spent_Mean

#Median for time spent value
Time_spent_Median <- median(dataset.df$time_spent)
Time_spent_Median

#Standard deviation for time spent value 
Time_spent_Standard_deviation <- sd(dataset.df$time_spent)
Time_spent_Standard_deviation

#range for time spent value
Time_spent_range <- range(dataset.df$time_spent)
Time_spent_range

#quartiles for time spent value
Time_spent_quartiles <- quantile(dataset.df$time_spent)
Time_spent_quartiles

#use summary() function to double check the result
summary(dataset.df$time_spent)

#### Age column ####

# Mean for for participant's ages
Age_Mean <- mean(dataset.df$age)
cat("Age mean: ", Age_Mean)

# Median for participant's ages
Age_Median <- median(dataset.df$age)
cat("Age median: ", Age_Median)

# Standard deviation for for participant's ages
Age_Standard_Deviation <- sd(dataset.df$age)
cat("Age standard deviation: ", Age_Standard_Deviation)

# range for for participant's ages
Age_Range <- range(dataset.df$age)
cat("Age range: ", Age_Range)

# quartiles for time spent value
Age_Quartiles <- quantile(dataset.df$age)
cat("Age quartiles: ", Age_Quartiles)

#use summary() function to double check the result
summary(dataset.df$age)

#####################


#### Income column ####

# Mean for for participant's income
Income_Mean <- mean(dataset.df$income)
cat("Income mean: ", Income_Mean)

# Median for participant's income
Income_Median <- median(dataset.df$income)
cat("Income median: ", Income_Median)

# Standard deviation for for participant's income
Income_Standard_Deviation <- sd(dataset.df$income)
cat("Income standard deviation: ", Income_Standard_Deviation)

# range for for participant's income
Income_Range <- range(dataset.df$income)
cat("Income range: ", Income_Range)

# quartiles for income
Income_Quartiles <- quantile(dataset.df$income)
cat("Income quartiles: ", Income_Quartiles)

#use summary() function to double check the result
summary(dataset.df$income)



######################

#Havent found out any missing or inconsistencies values, so skip Q3 (mia)

############################### Q4 - Plot Graphs ########################################################################

###### Time spent count & Total Time Spent vs Platform ########## 

#count the number of each time spent  value
time_spent_count <- table(dataset.df$time_spent)

#basic bar plot
barplot(time_spent_count)

#create a bar plot
barplot(time_spent_count, main = "Time spent on social media platforms", xlab = "Platform", 
        ylab = "Time spent (h)") # Great graphs so far! But I think time spent should be 1 - 9 (x-axis) and participants's count from 0 -120 (y-axis)! 
# I might be wrong haha (hieu)

#bar plot to show the trend of time spent over different platforms
Mybarplot <- ggplot(data = dataset.df, aes(x = as.character(platform), 
                                           fill = as.character(platform))) + 
  geom_bar(stat = "count") + labs(title = "Time spent on social media platforms",
                                  x = "Platform", y = "Time spent (h)") # Nice graph! Do the x and y axis also affect this graph or? (hieu)
Mybarplot  

##########################################################

# Other graphs ideas: 
# - Total time spent vs gender? 
# - Age vs Time Spent as Scatterplot? both are numerical, so we can find the relationship between the 2 
# - Income vs Time Spent? same as above

# Should we plot at least once for each column? One type of graph for each relation? let me know your thoughts
# (hieu)


##### Age vs Time Boxplot #####

# **Note on Boxplot - Why i think it works:
# - Can view median line (middle line), first and third Quartiles (bottom and top edges) and min, max values (end of each whiskers)
# - Can be translated into pretty interesting analysis (median line = 5 for all groups, almost same as mean age (5.029) indicating a symmetrical distribution visually)
# - closely resemble a normal distribution without simulating our dataset multiple times, etc..)
# - maybe more
# (hieu)

# Divide age data into 5 groups for cleaner graph
dataset.df$age_group <- cut(dataset.df$age, breaks = c(18, 28, 38, 48, 58, 64), include.lowest = TRUE, right = FALSE,
                            labels = c("18-27", "28-37", "38-47", "48-57", "58-64")) 

# Boxplot age vs time spent
ggplot(dataset.df, aes(x = factor(dataset.df$age_group), y = time_spent)) +
  geom_boxplot(aes(fill = factor(dataset.df$age_group))) + # Fill color by age
  scale_fill_viridis_d() + 
  theme_minimal() + 
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

####################################
#(Frances) bar plot of average time spent by gender
# to analyze if there are significant differences in social media usage patterns between different gender identities
#count the number of each time spent  value
time_spent_count <- table(dataset.df$time_spent)

#basic bar plot
barplot(time_spent_count)

#bar plot to show the average time spent by different gender identities

average_plot <- ggplot(data= dataset.df, aes( x = gender,y = time_spent)) +
  stat_summary(fun = "mean", geom = "bar", fill = c("blue","red","orange"), color = "black", alpha = 0.6) +
  labs(title = "Average Time Spent by Gender", x = "Gender", y = "Average Time Spent")
average_plot

#(Frances) stacked bar plot of average time spent by location and Platform
# to show the relationship among average time, location (3 countries), and platform.
# got a problem here, the number of y axis seems wrong!!!!!!!!!!! 
Time_spent_Mean <- mean(dataset.df$time_spent)

stacked_bar_plot <- ggplot(data= dataset.df, aes(x = location, y = Time_spent_Mean, fill = platform)) + 
                    geom_bar(stat = "identity", position = "stack") +
                    labs(title = "Average Time Spent by Location and Platform on Social Media",
                        x = "Location", y = "Average Time Spent")

stacked_bar_plot
########################## Time 2024-03-30-0:31 Frances################






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