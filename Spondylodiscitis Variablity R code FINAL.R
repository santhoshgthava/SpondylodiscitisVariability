library(readxl)
library(openxlsx)


# Load the data
Spondylodiscitis_Variability_Excel <- read_excel("OneDrive/Spondylodiscitis Variability/Spondylodiscitis Variability Excel.xlsx")
Spondylodiscitis_Variability_Excel$
library(readxl)
library(openxlsx)

# Load the data
Spondylodiscitis_Variability_Excel <- read_excel("OneDrive/Spondylodiscitis Variability/Spondylodiscitis Variability Excel.xlsx")

# Define the columns to split
columns_to_split <- c("How would you treat case 1?", "How would you treat case 2?", "How would you treat case 3?",
                      "How would you treat case 4?", "How would you treat case 5?", "How would you treat case 6?",
                      "How would you treat case 7?", "Typical indications for surgery in spinal infections", 
                      "How would you monitor treatment response of case 1?  (more than one option possible)", 
                      "How would you monitor treatment response in case 2?  (more than one option possible)")

# Replace ";" with "," in the original columns
Spondylodiscitis_Variability_Excel[columns_to_split] <- lapply(Spondylodiscitis_Variability_Excel[columns_to_split], function(x) gsub(";", ", ", x))

# Function to split data into separate dummy columns
split_data <- function(df, column_name) {
  # Split the data into separate values
  values <- strsplit(as.character(df[[column_name]]), ", ")
  
  # Remove leading/trailing white spaces
  values <- lapply(values, function(x) trimws(x, "both"))
  
  # Create unique dummy column names
  unique_values <- unique(unlist(values))
  unique_values <- paste(column_name, unique_values, sep = ": ")
  
  # Create dummy columns and set initial values to 0
  dummy_columns <- data.frame(matrix(0, nrow = nrow(df), ncol = length(unique_values)))
  colnames(dummy_columns) <- unique_values
  
  # Set values to 1 for corresponding dummy columns
  for (i in seq_along(values)) {
    values_split <- paste(column_name, values[[i]], sep = ": ")
    dummy_columns[i, values_split] <- 1
  }
  
  return(dummy_columns)
}





# Apply the split_data function to the specified columns
new_columns <- lapply(columns_to_split, function(column) split_data(Spondylodiscitis_Variability_Excel, column))

# Append the new columns to the original dataframe
Spondylodiscitis_Variability_Excel_NEW2 <- cbind(Spondylodiscitis_Variability_Excel, do.call(cbind, new_columns))

# Write the updated dataframe to a new Excel file
write.xlsx(Spondylodiscitis_Variability_Excel_NEW2, "Spondylodiscitis_Variability_Excel_NEW2.xlsx")

# View the updated dataframe
print(Spondylodiscitis_Variability_Excel_NEW2)



# Define the additional categorical columns to split
additional_columns_to_split <- c("Country", "Role", "Number of spinal cases as responsible surgeon per year",
                                 "Special interest in spine surgery",	"Typical indications for surgery in spinal infections",
                                 "Proportion of spinal infection cases referred to their service that are treated surgically",
                                 "Routine neurosurgery follow-up for discitis	Follow-up period (months) for discitis",
                                	"How long would you treat with antibiotics, presuming a good response in case 1?",
                                 "How long would you treat with antibiotics, presuming a good response in case 2?", 
                                 "How long would you treat with antibiotics, presuming a good response in case 5?",
                                 "How long would you treat with antibiotics, presuming a good response in case 6?")
                                 

# Add these additional columns to your original list of columns to split
columns_to_split <- c(columns_to_split, additional_columns_to_split)

# Apply the split_data function to the newly added categorical columns as well
new_columns <- lapply(columns_to_split, function(column) split_data(Spondylodiscitis_Variability_Excel, column))

# Append the new columns to the original dataframe
Spondylodiscitis_Variability_Excel_NEW2 <- cbind(Spondylodiscitis_Variability_Excel, do.call(cbind, new_columns))

# Write the updated dataframe to a new Excel file
write.xlsx(Spondylodiscitis_Variability_Excel_NEW2, "Spondylodiscitis_Variability_Excel_NEW2.xlsx")

# View the updated dataframe
print(Spondylodiscitis_Variability_Excel_NEW2)




#above splitting is wrong

# Load necessary libraries
library(readxl)
library(openxlsx)

# Load the data
Spondylodiscitis_Variability_Excel <- read_excel("OneDrive/Spondylodiscitis Variability/Spondylodiscitis Variability Excel.xlsx")

# Define the columns to split
columns_to_split <- c("How would you treat case 1?", "How would you treat case 2?", "How would you treat case 3?",
                      "How would you treat case 4?", "How would you treat case 5?", "How would you treat case 6?",
                      "How would you treat case 7?", "Typical indications for surgery in spinal infections", 
                      "How would you monitor treatment response of case 1?  (more than one option possible)", 
                      "How would you monitor treatment response in case 2?  (more than one option possible)")

# Replace ";" with "," in the original columns
Spondylodiscitis_Variability_Excel[columns_to_split] <- lapply(Spondylodiscitis_Variability_Excel[columns_to_split], function(x) gsub(";", ", ", x))

# Function to split data into separate dummy columns
split_data <- function(df, column_name) {
  # Split the data into separate values
  values <- strsplit(as.character(df[[column_name]]), ", ")
  
  # Remove leading/trailing white spaces
  values <- lapply(values, function(x) trimws(x, "both"))
  
  # Create unique dummy column names
  unique_values <- unique(unlist(values))
  unique_values <- paste(column_name, unique_values, sep = ": ")
  
  # Create dummy columns and set initial values to 0
  dummy_columns <- data.frame(matrix(0, nrow = nrow(df), ncol = length(unique_values)))
  colnames(dummy_columns) <- unique_values
  
  # Set values to 1 for corresponding dummy columns
  for (i in seq_along(values)) {
    for (j in values[[i]]) {
      col_name <- paste(column_name, j, sep = ": ")
      dummy_columns[i, col_name] <- 1
    }
  }
  
  return(dummy_columns)
}

# Define the additional categorical columns to split
additional_columns_to_split <- c("Country", "Role", "Number of spinal cases as responsible surgeon per year",
                                 "Special interest in spine surgery",	"Typical indications for surgery in spinal infections",
                                 "Proportion of spinal infection cases referred to their service that are treated surgically",
                                 "Routine neurosurgery follow-up for discitis", "Follow-up period (months) for discitis",
                                 "How long would you treat with antibiotics, presuming a good response in case 1?",
                                 "How long would you treat with antibiotics, presuming a good response in case 2?", 
                                 "How long would you treat with antibiotics, presuming a good response in case 5?",
                                 "How long would you treat with antibiotics, presuming a good response in case 6?")

# Add these additional columns to your original list of columns to split
columns_to_split <- c(columns_to_split, additional_columns_to_split)

# Apply the split_data function to the specified columns
new_columns <- lapply(columns_to_split, function(column) split_data(Spondylodiscitis_Variability_Excel, column))

# Append the new columns to the original dataframe
Spondylodiscitis_Variability_Excel_NEWTrial <- cbind(Spondylodiscitis_Variability_Excel, do.call(cbind, new_columns))

# Write the updated dataframe to a new Excel file
write.xlsx(Spondylodiscitis_Variability_Excel_NEWTrial, "Spondylodiscitis_Variability_Excel_NEWTrial.xlsx")

# View the updated dataframe
print(Spondylodiscitis_Variability_Excel_NEWTrial)



### now correct


















##################START

library(readxl)
Spondylodiscitis_Variability_Excel_NEW <- read_excel("OneDrive/Spondylodiscitis Variability/Spondylodiscitis_Variability_Excel_NEW.xlsx")




########################################## 1. World Map - FINISHED

# Required libraries
library(rworldmap)
library(ggplot2)
library(countrycode)
library(RColorBrewer)
library(ggplot2)
library(maps)


# Load world map
Spondylodiscitis_Variability_World_Map <- read_excel("OneDrive/Spondylodiscitis Variability/Spondylodiscitis Variability World Map.xlsx")



install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel", 
                   "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata"))

install.packages("rworldmap",dependencies=TRUE)

library("ggplot2")
theme_set(theme_bw())
library("sf")

library(rworldmap)
library(ggplot2)

# Prepare the map data
worldprep <- joinCountryData2Map(Spondylodiscitis_Variability_World_Map, 
                                 joinCode = "ISO3", 
                                 nameJoinColumn = "ISO3V10", 
                                 nameCountryColumn = "Country", 
                                 suggestForFailedCodes = FALSE, 
                                 mapResolution = "coarse", 
                                 projection = NA, 
                                 verbose = FALSE)

# Generate the map using mapCountryData
worldmapspondy <- mapCountryData(mapToPlot = worldprep, 
                                 nameColumnToPlot = "Studies", 
                                 xlim = NA, 
                                 ylim = NA, 
                                 mapRegion = "world", 
                                 catMethod = c(0:22), 
                                 colourPalette = "heat", 
                                 addLegend = TRUE,  # Set addLegend to TRUE
                                 borderCol = "black", 
                                 mapTitle = "Number of responders per country", 
                                 aspect = 1, 
                                 missingCountryCol = NA, 
                                 add = FALSE, 
                                 nameColumnToHatch = TRUE, 
                                 lwd = 0.5, 
                                 oceanCol = NA)









############################## 2. Histogram & Violin plot for "Years in practice" - FINISHED



# Load necessary libraries
library(readxl)
library(dplyr)
library(tidyr)
library(caret)
library(broom)

# Read the Excel file
Spondylodiscitis_Variability_Excel_NEW <- read_excel("OneDrive/Spondylodiscitis Variability/Spondylodiscitis_Variability_Excel_NEW.xlsx")

# Clean the column names
cleaned_column_names <- colnames(Spondylodiscitis_Variability_Excel_NEW) %>%
  gsub("\\W", "_", .) %>%
  gsub("_+", "_", .) %>%
  gsub("^_", "", .) %>%
  gsub("_$", "", .)

# Rename columns in the data frame
colnames(Spondylodiscitis_Variability_Excel_NEW) <- cleaned_column_names



#VIOLINPLOT
# Define the theme
theme_nature <- theme_classic() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 10)
  )

# Create the plot
ggplot(Spondylodiscitis_Variability_Excel_NEW, aes(x = "", y = `Years in practice`)) +
  geom_violin(fill = "lightblue", color = NA, trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white") +
  geom_jitter(shape = 16, size = 2, alpha = 0.5) +
  geom_vline(aes(xintercept = median(`Years in practice`, na.rm = T)),
             color = "red", linetype = "dashed", size = 1) +
  labs(
    title = "Years in Practice Distribution",
    x = NULL,
    y = "Years in Practice"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.01, 0.1))) +
  theme_nature


#SIMPLE HISTOGRAM
# Define the theme
theme_nature <- theme_classic() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 10)
  )

# Create the plot
ggplot(Spondylodiscitis_Variability_Excel_NEW, aes(x = `Years in practice`)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "grey", color = "black") +
  geom_density(alpha = .2, fill = "#FF6666") +
  geom_vline(aes(xintercept = median(`Years in practice`, na.rm = T)), color = "red", linetype = "dashed", size = 1) +
  labs(
    title = "Years in Practice Distribution",
    x = "Years in Practice",
    y = "Density"
  ) +
  theme_nature




# Print the plot
print(years_in_practice_plot)





###numbers

# Load necessary libraries
library(e1071)
library(tidyverse)

# Assume df$`Years in Practice` is your data
data <- Spondylodiscitis_Variability_Excel$`Years in practice`

# Calculate the statistics
summary_stats <- summary(data)
mode <- as.numeric(names(which.max(table(data))))
variance <- var(data, na.rm = TRUE)
std_dev <- sd(data, na.rm = TRUE)
range <- diff(range(data, na.rm = TRUE))
iqr <- IQR(data, na.rm = TRUE)
skewness <- skewness(data, na.rm = TRUE)
kurtosis <- kurtosis(data, na.rm = TRUE)

# Combine into a data frame
summary_table <- data.frame(
  Statistic = c("Minimum", "1st Quartile", "Median", "Mean", "3rd Quartile", "Maximum", 
                "Mode", "Variance", "Standard Deviation", "Range", "Interquartile Range", "Skewness", "Kurtosis"),
  Value = c(summary_stats[1], summary_stats[2], summary_stats[3], summary_stats[4], summary_stats[5], summary_stats[6], 
            mode, variance, std_dev, range, iqr, skewness, kurtosis)
)

# Print the table
print(summary_table)




################### 3. Bar Charts




###### ROLE - FINISHED




----------------------------------------

  library(ggplot2)

# Install the package if you haven't already
install.packages("tidyverse")

# Load the package
library(tidyverse)

# Get all binary column names (assuming they start with "Role_")
role_columns <- grep("^Role_", names(Spondylodiscitis_Variability_Excel_NEW), value = TRUE)

# Reshape the data to long format
long_format <- Spondylodiscitis_Variability_Excel_NEW %>%
  select(all_of(role_columns)) %>%
  pivot_longer(everything(), names_to = "Role", values_to = "Value")

# Compute the counts for each role
long_format_counts <- long_format %>% 
  group_by(Role) %>% 
  summarize(Count = sum(Value))

# Create the bar plot
role_plot <- ggplot(long_format_counts, aes(x = Role, y = Count, fill = Role)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  labs(x = "Role", y = "Count", title = "Distribution of Roles") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Print the plot
print(role_plot)

######OPTIMISED

# Calculate counts and percentages
role_counts <- Spondylodiscitis_Variability_Excel_NEW %>% 
  count(Role) %>% 
  mutate(Percentage = n / sum(n) * 100)

# Load necessary libraries
library(entropy)
install.packages("entropy")
library(tidyverse)

# Assume df$Role is your data
data <- Spondylodiscitis_Variability_Excel_NEW$

# Calculate the statistics
mode <- as.character(names(which.max(table(data))))
entropy <- entropy(table(data)/length(data))

# Calculate counts and percentages
counts_percentages <- Spondylodiscitis_Variability_Excel_NEW %>% 
  count(Role) %>%
  mutate(Percentage = n / sum(n) * 100)

# Add mode and entropy to the data frame
summary_table <- counts_percentages %>%
  add_row(Role = "Mode", n = mode, Percentage = NA) %>%
  add_row(Role = "Entropy", n = entropy, Percentage = NA)

# Print the table
print(summary_table)





###numbers

# Load necessary libraries
library(tidyverse)

# Assume df$Role is your data
data <- Spondylodiscitis_Variability_Excel$Role

# Calculate the statistics
mode <- as.character(names(which.max(table(data))))

# Calculate counts and percentages
counts_percentages <- Spondylodiscitis_Variability_Excel %>% 
  count(Role) %>%
  mutate(Percentage = n / sum(n) * 100)

# Add mode to the data frame
summary_table <- counts_percentages %>%
  add_row(Role = "Mode", n = mode, Percentage = NA)

# Print the table
print(summary_table)





### UNIVERSAL


# Load the necessary packages
library(tidyverse)
library(ggthemes)

# Define the function
create_plot <- function(df, prefix, plot_title, x_label) {
  # Get binary column names
  binary_columns <- grep(paste0("^", prefix), names(df), value = TRUE)
  
  # Reshape the data to long format and compute the counts
  long_format <- df %>%
    select(all_of(binary_columns)) %>%
    pivot_longer(everything(), names_to = "Variable", values_to = "Value") %>%
    group_by(Variable) %>%
    summarize(Count = sum(Value)) 
  
  # Compute the percentage and create plot labels
  total_count <- sum(long_format$Count)
  long_format <- long_format %>%
    mutate(Percentage = Count / total_count * 100,
           PlotLabel = str_remove(Variable, paste0(prefix, "_")) %>%
             str_replace_all("_", " ") %>%
             str_to_title())
  
  # Create the bar plot
  plot <- ggplot(long_format, aes(x = PlotLabel, y = Count, fill = PlotLabel)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    geom_text(aes(label = paste0(round(Percentage, 1), "%")), vjust = -0.5) +
    scale_y_continuous(expand = expansion(mult = c(0, .1))) +
    labs(x = x_label, y = "Count", title = plot_title) +
    theme_minimal() +
    theme(legend.position = "bottom") +
    theme_minimal()
  
  # Print the plot
  print(plot)
}

# Use the function
create_plot(Spondylodiscitis_Variability_Excel_NEW, "Role", "Distribution of Roles", "Role")


create_plot(Spondylodiscitis_Variability_Excel_NEW, "Number_of_spinal_cases", "Number of spinal cases as responsible surgeon per year", "Number of cases")


create_plot(Spondylodiscitis_Variability_Excel_NEW, "How_would_you_treat_case_1", "How would you treat Case 1", "Treatment of Choice")



################################   NUMBER OF SPINAL CASES - BAR PLOT DONE
selected_columns <- c("Number of spinal cases as responsible surgeon per year")
for (col in selected_columns) {
  bar_plot <- ggplot(Spondylodiscitis_Variability_Excel_NEW, aes(x = !!as.name(col))) +
    geom_bar(fill = '#386cb0') +
    labs(x = col, y = "Count", title = paste(col)) +
    theme_minimal()
  print(bar_plot)
}


# Bar plots with horizontal labels and line breaks
# Define the theme with adjusted font size and rotation
theme_custom <- theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0, vjust = 0.5, size = 8)  # Adjust font size and rotation
  )



####OPTIMISED

library(ggplot2)
library(dplyr)
library(scales)

library(dplyr)
library(ggplot2)

selected_columns <- c("Number of spinal cases as responsible surgeon per year")

for (col in selected_columns) {
  
  # Convert the column to a factor with specified levels
  Spondylodiscitis_Variability_Excel_NEW[[col]] <- factor(
    Spondylodiscitis_Variability_Excel_NEW[[col]],
    levels = c('<50', '50-100', '101-200', '201-300', '>300')
  )
  
  # Calculate counts and percentages
  column_counts <- Spondylodiscitis_Variability_Excel_NEW %>% 
    count(!!as.name(col)) %>% 
    mutate(Percentage = n / sum(n) * 100)
  
  # Bar plot for each column
  bar_plot <- ggplot(column_counts, aes(x = !!as.name(col), y = n)) +
    geom_bar(stat = "identity", fill = '#386cb0', color = "black") +
    geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
              vjust = -0.3, size = 3.5) +
    labs(x = col, y = "Count", title = paste(col)) +
    theme_bw() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "none",
      axis.line = element_line(),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 12, face = "bold"),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      plot.caption = element_text(size = 10, hjust = 0),
      strip.text = element_text(size = 10, face = "bold"),
      strip.background = element_rect(fill = "#e6e6e6", color = NA),
      panel.border = element_rect(color = "black", fill = NA),
      plot.margin = margin(5, 10, 5, 10)
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, .1))) # Add some space at the top of the bars for the labels
  
  print(bar_plot)
}




###numbers

# Load necessary library
library(dplyr)

# Convert the column to factor if it's not already
# Calculate the statistics
frequency_table <- Spondylodiscitis_Variability_Excel %>% 
  count(`Number of spinal cases as responsible surgeon per year`) %>%
  rename(Category = `Number of spinal cases as responsible surgeon per year`, Frequency = n) %>%
  mutate(Percentage = Frequency / sum(Frequency) * 100,
         Cumulative_Frequency = cumsum(Frequency),
         Cumulative_Percentage = Cumulative_Frequency / sum(Frequency) * 100)

mode_category <- as.character(frequency_table$Category[which.max(frequency_table$Frequency)])

# For ordinal data, the median category is the category that divides the data into two halves
median_category <- as.character(frequency_table$Category[which.min(abs(frequency_table$Cumulative_Percentage - 50))])

# Print the table
print(frequency_table)
print(paste("Mode: ", mode_category))
print(paste("Median: ", median_category))




######### ALL OTHERS

# Define the custom theme with Nature/NEJM style
theme_custom <- theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    axis.line = element_line(),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    plot.caption = element_text(size = 10, hjust = 0),
    strip.text = element_text(size = 10, face = "bold"),
    strip.background = element_rect(fill = "#e6e6e6", color = NA),
    panel.border = element_rect(color = "black", fill = NA),
    plot.margin = margin(5, 10, 5, 10)
  )


# Bar Plot for "Special interest in spine surgery"
bar_plot_1 <- ggplot(Spondylodiscitis_Variability_Excel_NEW, aes(x = reorder(`Special interest in spine surgery`, -desc(`Special interest in spine surgery`)))) +
  geom_bar(fill = '#386cb0') +
  labs(x = NULL, y = "Count", title = "Special Interest in Spine Surgery") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +  # Wrap labels with line breaks
  theme_custom
print(bar_plot_1)


# Calculate Mode
mode <- as.character(names(which.max(table(data))))

# Calculate frequencies and relative frequencies
summary_table <- Spondylodiscitis_Variability_Excel %>%
  count(`Special interest in spine surgery`) %>%
  mutate(Percentage = n / sum(n) * 100,
         CumulativeFrequency = cumsum(n),
         CumulativePercentage = cumsum(Percentage))

# Add mode to the data frame
summary_table <- summary_table %>%
  add_row(`Special interest in spine surgery` = "Mode", n = mode, Percentage = NA, CumulativeFrequency = NA, CumulativePercentage = NA)

# Print the table
print(summary_table)


##numbers





# Bar Plot for "Proportion of spinal infection cases referred to their service that are treated surgically"
bar_plot_2 <- ggplot(Spondylodiscitis_Variability_Excel, aes(x = reorder(`Proportion of spinal infection cases referred to their service that are treated surgically`, -desc(`Proportion of spinal infection cases referred to their service that are treated surgically`)))) +
  geom_bar(fill = '#386cb0') +
  labs(x = NULL, y = "Count", title = "Treatment Proportion") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +  # Wrap labels with line breaks
  theme_custom
print(bar_plot_2)

mode <- as.character(names(which.max(table(data))))

# Calculate frequencies and relative frequencies
summary_table <- Spondylodiscitis_Variability_Excel %>%
  count(``Proportion of spinal infection cases referred to their service that are treated surgically`, -desc(`Proportion of spinal infection cases referred to their service that are treated surgically``) %>%
  mutate(Percentage = n / sum(n) * 100,
         CumulativeFrequency = cumsum(n),
         CumulativePercentage = cumsum(Percentage))

# Add mode to the data frame
summary_table <- summary_table %>%
  add_row(`Special interest in spine surgery` = "Mode", n = mode, Percentage = NA, CumulativeFrequency = NA, CumulativePercentage = NA)

# Print the table
print(summary_table)








# Bar Plot for "How would you treat case 1?"
bar_plot_3 <- ggplot(Spondylodiscitis_Variability_Excel_NEW, aes(x = reorder(`How would you treat case 1?`, -desc(`How would you treat case 1?`)))) +
  geom_bar(fill = '#386cb0') +
  labs(x = NULL, y = "Count", title = "Treatment for Case 1") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +  # Wrap labels with line breaks
  theme_custom
print(bar_plot_3)


# Repeat the above code for the remaining bar plots, adjusting the variables and titles accordingly

# Bar Plot for "How long would you treat with antibiotics, presuming a good response in case 1?"
bar_plot_4 <- ggplot(Spondylodiscitis_Variability_Excel_NEW, aes(x = reorder(`How long would you treat with antibiotics, presuming a good response in case 1?`, -desc(`How long would you treat with antibiotics, presuming a good response in case 1?`)))) +
  geom_bar(fill = '#386cb0') +
  labs(x = NULL, y = "Count", title = "Treatment Duration for Case 1") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +  # Wrap labels with line breaks
  theme_custom
print(bar_plot_4)


# Bar Plot for "How would you treat case 2?"
bar_plot_5 <- ggplot(Spondylodiscitis_Variability_Excel_NEW, aes(x = reorder(`How would you treat case 2?`, -desc(`How would you treat case 2?`)))) +
  geom_bar(fill = '#386cb0') +
  labs(x = NULL, y = "Count", title = "Treatment for Case 2") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +  # Wrap labels with line breaks
  theme_custom
print(bar_plot_5)


# Bar Plot for "How long would you treat with antibiotics, presuming a good response in case 2?"
bar_plot_6 <- ggplot(Spondylodiscitis_Variability_Excel_NEW, aes(x = reorder(`How long would you treat with antibiotics, presuming a good response in case 2?`, -desc(`How long would you treat with antibiotics, presuming a good response in case 2?`)))) +
  geom_bar(fill = '#386cb0') +
  labs(x = NULL, y = "Count", title = "Treatment Duration for Case 2") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +  # Wrap labels with line breaks
  theme_custom
print(bar_plot_6)


# Bar Plot for "How would you treat case 3?"
bar_plot_7 <- ggplot(Spondylodiscitis_Variability_Excel_NEW, aes(x = reorder(`How would you treat case 3?`, -desc(`How would you treat case 3?`)))) +
  geom_bar(fill = '#386cb0') +
  labs(x = NULL, y = "Count", title = "Treatment for Case 3") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +  # Wrap labels with line breaks
  theme_custom
print(bar_plot_7)


# Bar Plot for "How would you treat case 4?"
bar_plot_8 <- ggplot(Spondylodiscitis_Variability_Excel_NEW, aes(x = reorder(`How would you treat case 4?`, -desc(`How would you treat case 4?`)))) +
  geom_bar(fill = '#386cb0') +
  labs(x = NULL, y = "Count", title = "Treatment for Case 4") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +  # Wrap labels with line breaks
  theme_custom
print(bar_plot_8)


# Bar Plot for "How would you treat case 5?"
bar_plot_9 <- ggplot(Spondylodiscitis_Variability_Excel_NEW, aes(x = reorder(`How would you treat case 5?`, -desc(`How would you treat case 5?`)))) +
  geom_bar(fill = '#386cb0') +
  labs(x = NULL, y = "Count", title = "Treatment for Case 5") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +  # Wrap labels with line breaks
  theme_custom
print(bar_plot_9)


# Bar Plot for "How long would you treat with antibiotics, presuming a good response in case 5?"
bar_plot_10 <- ggplot(Spondylodiscitis_Variability_Excel_NEW, aes(x = reorder(`How long would you treat with antibiotics, presuming a good response in case 5?`, -desc(`How long would you treat with antibiotics, presuming a good response in case 5?`)))) +
  geom_bar(fill = '#386cb0') +
  labs(x = NULL, y = "Count", title = "Treatment Duration for Case 5") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +  # Wrap labels with line breaks
  theme_custom
print(bar_plot_10)


# Bar Plot for "How would you treat case 6?"
bar_plot_11 <- ggplot(Spondylodiscitis_Variability_Excel_NEW, aes(x = reorder(`How would you treat case 6?`, -desc(`How would you treat case 6?`)))) +
  geom_bar(fill = '#386cb0') +
  labs(x = NULL, y = "Count", title = "Treatment for Case 6") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +  # Wrap labels with line breaks
  theme_custom
print(bar_plot_11)


# Bar Plot for "How long would you treat with antibiotics, presuming a good response in case 6?"
bar_plot_12 <- ggplot(Spondylodiscitis_Variability_Excel_NEW, aes(x = reorder(`How long would you treat with antibiotics, presuming a good response in case 6?`, -desc(`How long would you treat with antibiotics, presuming a good response in case 6?`)))) +
  geom_bar(fill = '#386cb0') +
  labs(x = NULL, y = "Count", title = "Treatment Duration for Case 6") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +  # Wrap labels with line breaks
  theme_custom
print(bar_plot_12)


# Bar Plot for "How would you treat case 7?"
bar_plot_13 <- ggplot(Spondylodiscitis_Variability_Excel_NEW, aes(x = reorder(`How would you treat case 7?`, -desc(`How would you treat case 7?`)))) +
  geom_bar(fill = '#386cb0') +
  labs(x = NULL, y = "Count", title = "Treatment for Case 7") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +  # Wrap labels with line breaks
  theme_custom
print(bar_plot_13)



#### OPTIMISED
library(stringr)
library(ggplot2)
library(dplyr)
library(scales)

# List of column names to plot
selected_columns <- c("Special interest in spine surgery",
                      "Proportion of spinal infection cases referred to their service that are treated surgically",
                      "How would you treat case 1?",
                      "How would you treat case 2?",
                      "How would you treat case 3?",
                      "How would you treat case 4?",
                      "How would you treat case 5?",
                      "How would you treat case 6?",
                      "How would you treat case 7?")

# Define the custom theme with NEJM style
theme_custom <- theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    axis.line = element_line(),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    plot.caption = element_text(size = 10, hjust = 0),
    strip.text = element_text(size = 10, face = "bold"),
    strip.background = element_rect(fill = "#e6e6e6", color = NA),
    panel.border = element_rect(color = "black", fill = NA),
    plot.margin = margin(5, 10, 5, 10)
  )

# Loop through each column
for (col in selected_columns) {
  
  # Calculate counts and percentages
  column_counts <- Spondylodiscitis_Variability_Excel_NEW %>% 
    count(!!as.name(col)) %>% 
    mutate(Percentage = n / sum(n) * 100)
  
  # Create bar plot
  bar_plot <- ggplot(column_counts, aes(x = reorder(!!as.name(col), -n), y = n)) +
    geom_bar(stat = "identity", fill = '#386cb0', color = "black") +
    geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
              vjust = -0.3, size = 3.5) +
    labs(x = col, y = "Count", title = str_wrap(col, width = 30)) +
    theme_custom +
    scale_y_continuous(expand = expansion(mult = c(0, .1))) + # Add some space at the top of the bars for the labels
    scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) # Wrap labels with line breaks
  
  print(bar_plot)
}



#Histogram and violin plot for discitis followup in months

# Convert "Follow-up period (months) for discitis" to numeric
Spondylodiscitis_Variability_Excel_NEW$`Follow-up period (months) for discitis` <- as.numeric(Spondylodiscitis_Variability_Excel_NEW$`Follow-up period (months) for discitis`)

# Define the theme
theme_nature <- theme_bw() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

# Histogram for "Follow-up period (months) for discitis"
histogram <- ggplot(Spondylodiscitis_Variability_Excel_NEW, aes(x = `Follow-up period (months) for discitis`)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "grey", color = "black") +
  geom_density(alpha = 0.2, fill = "#FF6666") +
  labs(
    title = "Follow-up Period for Discitis",
    x = "Follow-up Period (months)",
    y = "Density"
  ) +
  theme_nature

print(histogram)

# Violin Plot for "Follow-up period (months) for discitis"
violin_plot <- ggplot(Spondylodiscitis_Variability_Excel_NEW, aes(x = "", y = `Follow-up period (months) for discitis`)) +
  geom_violin(fill = "#386cb0", color = "black", trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white", color = "black") +
  geom_jitter(shape = 16, size = 2, alpha = 0.5) +
  labs(
    title = "Follow-up Period for Discitis",
    x = NULL,
    y = "Follow-up Period (months)"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.01, 0.1))) +
  theme_nature

print(violin_plot)



# Load necessary libraries
library(e1071)
library(tidyverse)

# Assume df$`Years in Practice` is your data
data <- as.numeric(Spondylodiscitis_Variability_Excel$`Follow-up period (months) for discitis`)

# Calculate the statistics
summary_stats <- summary(data)
mode <- as.numeric(names(which.max(table(data))))
variance <- var(data, na.rm = TRUE)
std_dev <- sd(data, na.rm = TRUE)
range <- diff(range(data, na.rm = TRUE))
iqr <- IQR(data, na.rm = TRUE)
skewness <- skewness(data, na.rm = TRUE)
kurtosis <- kurtosis(data, na.rm = TRUE)

# Combine into a data frame
summary_table <- data.frame(
  Statistic = c("Minimum", "1st Quartile", "Median", "Mean", "3rd Quartile", "Maximum", 
                "Mode", "Variance", "Standard Deviation", "Range", "Interquartile Range", "Skewness", "Kurtosis"),
  Value = c(summary_stats[1], summary_stats[2], summary_stats[3], mean(data, na.rm = TRUE), summary_stats[5], summary_stats[6], 
            mode, variance, std_dev, range, iqr, skewness, kurtosis)
)

# Print the table
print(summary_table)



#!!!!!!! still need to create barplots for monitoring variables and Typical indications for surgery in spinal infections

############### CASE 1

# Specify the columns you're interested in
case1_cols <- c("How would you treat case 1?: Conservative treatment",
                "How would you treat case 1?: Antibiotics",
                "How would you treat case 1?: Decompression",
                "How would you treat case 1?: Posterior instrumentation",
                "How would you treat case 1?: Interbody fusion",
                "How would you treat case 1?: Posterior decompression",
                "How would you treat case 1?: Other")

# Subset the data for these columns
case1_data <- Spondylodiscitis_Variability_Excel_NEW[, case1_cols]

# Convert data to numeric
case1_data <- apply(case1_data, 2, as.numeric)

# Calculate the sums for each treatment category
case1_sum <- colSums(case1_data, na.rm = TRUE)

# Create a dataframe for the plot
df_case1 <- data.frame(Treatment = names(case1_sum), Count = case1_sum)

# Create the bar plot
ggplot(df_case1, aes(x = reorder(Treatment, -Count), y = Count)) +
  geom_bar(stat = "identity", fill = '#386cb0') +
  labs(x = NULL, y = "Count", title = "Treatment for Case 1") +
  coord_flip() + # Rotate the plot to horizontal
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels




#LIKERT PLOT


library(ggplot2)
library(forcats)  # for fct_reorder
library(scales)
library(dplyr)
library(ggplot2)
library(scales)
library(RColorBrewer)
library(grid)


ordered_questions <- c(
  "How would you treat case 1?",
  "How would you monitor treatment response of case 1?  (more than one option possible)",
  "How long would you treat with antibiotics, presuming a good response in case 1?",
  "How would you treat case 2?",
  "How would you monitor treatment response in case 2?  (more than one option possible)",
  "How long would you treat with antibiotics, presuming a good response in case 2?",
  "How would you treat case 3?",
  "How would you treat case 4?",
  "How would you treat case 5?",
  "How long would you treat with antibiotics, presuming a good response in case 5?",
  "How would you treat case 6?",
  "How long would you treat with antibiotics, presuming a good response in case 6?",
  "How would you treat case 7?"
)

library(dplyr)
library(ggplot2)
library(scales)
library(RColorBrewer)
library(grid)
library(gridExtra)
library(readxl)
library(tidyr)



Spondylodiscitis_Variability_Excel <- read_excel("OneDrive/Spondylodiscitis Variability/Spondylodiscitis Variability Excel.xlsx")


# Questions order
ordered_questions <- c(
  "How would you treat case 1?",
  "How would you monitor treatment response of case 1?  (more than one option possible)",
  "How long would you treat with antibiotics, presuming a good response in case 1?",
  "How would you treat case 2?",
  "How would you monitor treatment response in case 2?  (more than one option possible)",
  "How long would you treat with antibiotics, presuming a good response in case 2?",
  "How would you treat case 3?",
  "How would you treat case 4?",
  "How would you treat case 5?",
  "How long would you treat with antibiotics, presuming a good response in case 5?",
  "How would you treat case 6?",
  "How long would you treat with antibiotics, presuming a good response in case 6?",
  "How would you treat case 7?"
)

# 1. Convert from wide to long format
df_long <- Spondylodiscitis_Variability_Excel %>%
  pivot_longer(cols = everything(), names_to = "Question", values_to = "Option") %>%
  # Set the order of the questions here
  mutate(Question = factor(Question, levels = ordered_questions))

# 2. Count the occurrences
df_count <- df_long %>%
  count(Question, Option, .drop = FALSE) 

# 3. Calculate the percentages
df_count <- df_count %>%
  group_by(Question) %>%
  mutate(Percentage = n / sum(n) * 100) %>%
  ungroup()


library(dplyr)
library(ggplot2)
library(scales)
library(RColorBrewer)
library(grid)
library(gridExtra)
library(readxl)
library(tidyr)



Spondylodiscitis_Variability_Excel <- read_excel("OneDrive/Spondylodiscitis Variability/Spondylodiscitis Variability Excel.xlsx")


# Questions order
ordered_questions <- c(
  "How would you treat case 1?",
  "How would you monitor treatment response of case 1?  (more than one option possible)",
  "How long would you treat with antibiotics, presuming a good response in case 1?",
  "How would you treat case 2?",
  "How would you monitor treatment response in case 2?  (more than one option possible)",
  "How long would you treat with antibiotics, presuming a good response in case 2?",
  "How would you treat case 3?",
  "How would you treat case 4?",
  "How would you treat case 5?",
  "How long would you treat with antibiotics, presuming a good response in case 5?",
  "How would you treat case 6?",
  "How long would you treat with antibiotics, presuming a good response in case 6?",
  "How would you treat case 7?"
)

# 1. Convert from wide to long format
df_long <- Spondylodiscitis_Variability_Excel %>%
  pivot_longer(cols = everything(), names_to = "Question", values_to = "Option") %>%
  # Set the order of the questions here
  mutate(Question = factor(Question, levels = ordered_questions))

# 2. Count the occurrences
df_count <- df_long %>%
  count(Question, Option, .drop = FALSE) 

# 3. Calculate the percentages
df_count <- df_count %>%
  group_by(Question) %>%
  mutate(Percentage = n / sum(n) * 100) %>%
  ungroup()


# Function to generate individual stacked percentage bar plots for each question

library(RColorBrewer)
library(ggplot2)
library(dplyr)
library(scales)

# Get the colors from Set3 palette
set3_colors <- brewer.pal(12, "Set3")

# Function to generate individual plots
generate_plot <- function(data, question) {
  
  # Sort data by Option
  data <- data %>% arrange(Option)
  
  # Generate labels for Option including percentages
  data$OptionLabel <- paste0(data$Option, " (", round(data$Percentage, 1), "%)")
  
  # Map the colors to the options using modulo arithmetic
  option_colors <- set3_colors[1:length(data$Option) %% length(set3_colors) + 1]
  names(option_colors) <- data$OptionLabel
  
  p <- ggplot(data, aes(x = "", y = Percentage, fill = OptionLabel)) +  
    geom_bar(stat = "identity", position = "fill", width = 0.3) +
    
    labs(title = question, x = NULL, y = "") +
    scale_y_continuous(labels = percent_format(scale = 100), breaks = seq(0, 100, by = 0.2)) +
    coord_flip() +
    theme_minimal() +  
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
      axis.text.x = element_text(size = 8),
      axis.text.y = element_text(size = 10),
      axis.title.y = element_blank(),
      legend.text = element_text(size = 8),
      plot.margin = margin(1, 1, 1, 1, "cm"),
      legend.key.size = unit(0.7, "cm")
    ) +
    scale_fill_manual(values = option_colors)
  
  print(p)
}

# Generate and store the plots
for (q in ordered_questions) {
  subset_data <- df_count %>% filter(Question == q)
  generate_plot(subset_data, q)
}



#IQV for each case

calculate_true_iqv <- function(data) {
  k <- nrow(data)  # number of categories
  n <- sum(data$n)  # total number of cases
  
  sum_ni_square <- sum(data$n^2)
  
  IQV <- (k*(n - 1) - sum_ni_square) / (k*(n - 1))
  
  return(IQV)
}

df_iqv <- df_count %>% 
  group_by(Question) %>% 
  summarise(IQV = calculate_true_iqv(cur_data()))

print(df_iqv)




#make graphs

# Create custom color palette
color_palette <- colorRampPalette(brewer.pal(9, "YlOrRd"))(100)

ggplot(df_iqv, aes(x = reorder(Question, -IQV), y = IQV)) +
  
  # Bar plot with custom gradient fill based on IQV
  geom_bar(stat = "identity", aes(fill = IQV), width = 0.7) +
  
  # Annotations for high IQV
  geom_text(aes(label = round(IQV, 2), y = IQV + 0.02), hjust = -0.00001, size = 3.5) +
  
  # Theme and labels
  coord_flip() +
  labs(title = "Index of Qualitative Variation by Question",
      
       caption = "Source: Your Data Source",
       x = NULL, y = "IQV") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
    plot.subtitle = element_text(size = 16, hjust = 0.5),
    axis.title.y = element_text(face = "bold", size = 14, margin = margin(t = 0, r = 20, b = 0, l = 0)),
    axis.text.y = element_text(size = 12, margin = margin(t = 0, r = 10, b = 0, l = 0)),
    axis.title.x = element_blank(),
    legend.position = "none"
  ) +
  scale_fill_gradientn(colors = color_palette, guide = "colourbar")







#### PER COUNTRY

# Function to calculate IQV
calculate_true_iqv <- function(data) {
  k <- nrow(data)  # number of categories
  n <- sum(data$n)  # total number of cases
  
  sum_ni_square <- sum(data$n^2)
  
  IQV <- (k*(n - 1) - sum_ni_square) / (k*(n - 1))
  
  return(IQV)
}

library(dplyr)


#get rid of NAs

df_count <- df_count %>%
  filter(!is.na(Country), !is.na(Question), !is.na(Option), !is.na(n), !is.na(Percentage))


# Calculate the IQV per question per country
df_iqv <- df_count %>% 
  group_by(Country, Question) %>% 
  summarise(IQV = calculate_true_iqv(cur_data())) %>% 
  ungroup()

# Calculate the mean IQV per country
df_mean_country <- df_iqv %>% 
  group_by(Country) %>%
  summarise(Mean_IQV = mean(IQV)) %>%
  ungroup()



# Generate the color palette
color_palette <- colorRampPalette(brewer.pal(12, "Set3"))(num_rows)

# Plotting the mean IQV per country:
ggplot(df_mean_country_filtered, aes(x = reorder(Country, Mean_IQV), y = Mean_IQV, fill = Country)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = color_palette) +
  coord_flip() +
  labs(title = "Overall Mean IQV per Country",
       x = NULL, y = "Mean IQV") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.text.x = element_text(size = 12, color = "black"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 12)
  ) +
  guides(fill=FALSE)  # This will remove the legend since the colors match the y-axis labels.




####### more robust version

library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(doParallel)
# Provided IQV calculation function
calculate_true_iqv <- function(data) {
  k <- nrow(data)  # number of categories
  n <- sum(data$n)  # total number of cases
  
  sum_ni_square <- sum(data$n^2)
  
  IQV <- (k*(n - 1) - sum_ni_square) / (k*(n - 1))
  
  return(IQV)
}

df_iqv <- df_count %>% 
  group_by(Question) %>% 
  summarise(IQV = calculate_true_iqv(cur_data()))

print(df_iqv)

# Bootstrap IQV Function
bootstrap_iqv <- function(data, B = 1000) {
  # Initialize an empty vector to store the bootstrapped IQVs
  bootstrap_results <- numeric(B)
  
  for(i in seq_len(B)) {
    # Resample data with replacement
    sample_data <- sample_n(data, nrow(data), replace = TRUE)
    
    # Calculate IQV for the resampled data
    bootstrap_results[i] <- calculate_iqv(sample_data)
  }
  
  # Return the mean and the 2.5th and 97.5th percentiles
  list(mean = mean(bootstrap_results), 
       lower = quantile(bootstrap_results, 0.025), 
       upper = quantile(bootstrap_results, 0.975))
}

# Register parallel backend
registerDoParallel(makeCluster(detectCores() - 1))  # Leave one core free

# Apply bootstrap using parallel processing
bootstrap_results <- df_count %>%
  group_by(Country, Question) %>%
  do(data.frame(bootstrap_iqv(.))) %>%
  ungroup()

# Stop the cluster after bootstrapping
stopImplicitCluster()

# Apply bootstrap on the data
bootstrap_results <- df_count %>% 
  group_by(Country, Question) %>% 
  do(data.frame(bootstrap_iqv(.)))

# Add a weight column based on total number of responses per country
df_count <- df_count %>% 
  group_by(Country) %>%
  mutate(weight = sum(n)) %>%
  ungroup()

# Now, when plotting, adjust by this weight
ggplot(df_mean_country_filtered, aes(x = reorder(Country, Mean_IQV), y = Mean_IQV, fill = Country)) +
  geom_bar(stat = "identity", aes(weight = weight)) +  # Adjust the bars based on weight
  geom_errorbar(aes(ymin = bootstrap_results$lower, ymax = bootstrap_results$upper), width = 0.2) +  # Display confidence intervals
  scale_fill_manual(values = color_palette) +
  coord_flip() +
  labs(title = "Overall Mean IQV per Country",
       x = NULL, y = "Mean IQV") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.text.x = element_text(size = 12, color = "black"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 12)
  ) +
  guides(fill=FALSE) +  geom_text(aes(label = round(Mean_IQV, 2), y = Mean_IQV + 0.02), hjust = 0.8, size = 3.5)

















#per question per country

# Calculate the IQV for each country-question combination
# Define color palette based on the unique countries in the filtered data
unique_countries <- unique(df_mean_country_filtered$Country)
color_palette <- colorRampPalette(brewer.pal(9, "YlOrRd"))(length(unique_countries))
named_color_palette <- setNames(color_palette, unique_countries)

# Filter out rows where IQV or Country is NA
df_iqv_country_question <- df_iqv_country_question %>% filter(!is.na(IQV) & !is.na(Country))

# List of unique questions
questions <- unique(df_iqv_country_question$Question)

# Generate the plots for each question
plots <- lapply(questions, function(q) {
  
  # Filter data for the specific question
  df_filtered <- df_iqv_country_question %>% filter(Question == q)
  
  # Plot
  p <- ggplot(df_filtered, aes(x = reorder(Country, -IQV), y = IQV, fill = Country)) +
    geom_bar(stat = "identity", width = 0.7) +
    labs(title = paste("IQV for", q),
         x = NULL, y = "Mean IQV") +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
      axis.text.y = element_text(size = 12, color = "black"),
      axis.text.x = element_text(size = 12, color = "black", angle = 90, vjust = 0.5, hjust = 1),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.border = element_blank(),
      axis.ticks.y = element_blank(),
      legend.position = "bottom",
      legend.text = element_text(size = 12)
    ) +
    scale_fill_manual(values = named_color_palette) +
    guides(fill = FALSE) +
    geom_text(aes(label = round(IQV, 2), y = IQV + 0.02), hjust = 0.8, size = 3.5)
  
  return(p)
})

# Display the plots
plots










######### CASE 3

# Specify the columns you're interested in for Case 3
case3_cols <- c("How would you treat case 3?: Posterior decompression",
                "How would you treat case 3?: Antibiotics",
                "How would you treat case 3?: Conservative treatment",
                "How would you treat case 3?: Decompression",
                "How would you treat case 3?: Posterior instrumentation",
                "How would you treat case 3?: Interbody fusion",
                "How would you treat case 3?: Not sure",
                "How would you treat case 3?: NA",
                "How would you treat case 3?: Other")

# Subset the data for these columns (Case 3)
case3_data <- Spondylodiscitis_Variability_Excel_NEW[, case3_cols]

# Convert data to numeric
case3_data <- apply(case3_data, 2, as.numeric)

# Calculate the sums for each treatment category (Case 3)
case3_sum <- colSums(case3_data, na.rm = TRUE)

# Create a dataframe for the plot (Case 3)
df_case3 <- data.frame(Treatment = names(case3_sum), Count = case3_sum)

# Create the bar plot for Case 3
ggplot(df_case3, aes(x = reorder(Treatment, -Count), y = Count)) +
  geom_bar(stat = "identity", fill = '#386cb0') +
  labs(x = NULL, y = "Count", title = "Treatment for Case 3") +
  coord_flip() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

######### CASE 4

# Specify the columns you're interested in for Case 4
case4_cols <- c("How would you treat case 4?: Conservative treatment",
                "How would you treat case 4?: Antibiotics",
                "How would you treat case 4?: Decompression",
                "How would you treat case 4?: Posterior instrumentation",
                "How would you treat case 4?: Interbody fusion",
                "How would you treat case 4?: Posterior decompression",
                "How would you treat case 4?: Not sure",
                "How would you treat case 4?: Other",
                "How would you treat case 4?: NA")

# Subset the data for these columns (Case 4)
case4_data <- Spondylodiscitis_Variability_Excel_NEW[, case4_cols]

# Convert data to numeric
case4_data <- apply(case4_data, 2, as.numeric)

# Calculate the sums for each treatment category (Case 4)
case4_sum <- colSums(case4_data, na.rm = TRUE)

# Create a dataframe for the plot (Case 4)
df_case4 <- data.frame(Treatment = names(case4_sum), Count = case4_sum)

# Create the bar plot for Case 4
ggplot(df_case4, aes(x = reorder(Treatment, -Count), y = Count)) +
  geom_bar(stat = "identity", fill = '#386cb0') +
  labs(x = NULL, y = "Count", title = "Treatment for Case 4") +
  coord_flip() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



######### CASE 5


# Specify the columns you're interested in for Case 5
case5_cols <- c("How would you treat case 5?: Conservative treatment",
                "How would you treat case 5?: Antibiotics",
                "How would you treat case 5?: Conservative antibiotic first for several days",
                "How would you treat case 5?: Surgery after stabilization of pt general status",
                "How would you treat case 5?: Not sure",
                "How would you treat case 5?: Early decompression",
                "How would you treat case 5?: Posterior instrumentation",
                "How would you treat case 5?: Interbody fusion",
                "How would you treat case 5?: Early Posterior decompression",
                "How would you treat case 5?: No instrumentation")

# Subset the data for these columns (Case 5)
case5_data <- Spondylodiscitis_Variability_Excel_NEW[, case5_cols]

# Convert data to numeric
case5_data <- apply(case5_data, 2, as.numeric)

# Calculate the sums for each treatment category (Case 5)
case5_sum <- colSums(case5_data, na.rm = TRUE)

# Create a dataframe for the plot (Case 5)
df_case5 <- data.frame(Treatment = names(case5_sum), Count = case5_sum)

# Create the bar plot for Case 5
ggplot(df_case5, aes(x = reorder(Treatment, -Count), y = Count)) +
  geom_bar(stat = "identity", fill = '#386cb0') +
  labs(x = NULL, y = "Count", title = "Treatment for Case 5") +
  coord_flip() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


######### CASE 6


# Specify the columns you're interested in for Case 6
case6_cols <- c("How would you treat case 6?: Not sure",
                "How would you treat case 6?: Decompression",
                "How would you treat case 6?: Posterior instrumentation",
                "How would you treat case 6?: Interbody fusion",
                "How would you treat case 6?: Antibiotics",
                "How would you treat case 6?: Conservative treatment",
                "How would you treat case 6?: Other",
                "How would you treat case 6?: Anterior reconstruction",
                "How would you treat case 6?: Posterior decompression",
                "How would you treat case 6?: NA")

# Subset the data for these columns (Case 6)
case6_data <- Spondylodiscitis_Variability_Excel_NEW[, case6_cols]

# Convert data to numeric
case6_data <- apply(case6_data, 2, as.numeric)

# Calculate the sums for each treatment category (Case 6)
case6_sum <- colSums(case6_data, na.rm = TRUE)

# Create a dataframe for the plot (Case 6)
df_case6 <- data.frame(Treatment = names(case6_sum), Count = case6_sum)

# Create the bar plot for Case 6
ggplot(df_case6, aes(x = reorder(Treatment, -Count), y = Count)) +
  geom_bar(stat = "identity", fill = '#386cb0') +
  labs(x = NULL, y = "Count", title = "Treatment for Case 6") +
  coord_flip() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



######### CASE 7


# Specify the columns you're interested in for Case 7
case7_cols <- c("How would you treat case 7?: Not sure",
                "How would you treat case 7?: Conservative treatment",
                "How would you treat case 7?: Antibiotics",
                "How would you treat case 7?: Decompression",
                "How would you treat case 7?: Posterior instrumentation",
                "How would you treat case 7?: Anterior reconstruction",
                "How would you treat case 7?: Posterior decompression",
                "How would you treat case 7?: Interbody fusion",
                "How would you treat case 7?: Other",
                "How would you treat case 7?: NA")

# Subset the data for these columns (Case 7)
case7_data <- Spondylodiscitis_Variability_Excel_NEW[, case7_cols]

# Convert data to numeric
case7_data <- apply(case7_data, 2, as.numeric)

# Calculate the sums for each treatment category (Case 7)
case7_sum <- colSums(case7_data, na.rm = TRUE)

# Create a dataframe for the plot (Case 7)
df_case7 <- data.frame(Treatment = names(case7_sum), Count = case7_sum)

# Create the bar plot for Case 7
ggplot(df_case7, aes(x = reorder(Treatment, -Count), y = Count)) +
  geom_bar(stat = "identity", fill = '#386cb0') +
  labs(x = NULL, y = "Count", title = "Treatment for Case 7") +
  coord_flip() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



######### CASE ABX duration: Case 1 Duration of ABx

# Install and load the required packages
install.packages(c("ggplot2", "dplyr", "tidyr", "tibble"))
library(ggplot2)
library(dplyr)
library(tidyr)
library(tibble)

# NEJM color palette (blue)
nejm_colors <- c("#386cb0")

# Specify the columns you're interested in for Cases
case8_cols <- c("How long would you treat with Antibiotics, presuming a good response in case 1?")

# Subset the data for these columns (Case 8)
case8_data <- Spondylodiscitis_Variability_Excel_NEW[, case8_cols]

# Melt the data into a long format
df_case8 <- case8_data %>% pivot_longer(everything(), names_to = "Case", values_to = "Duration")

# Remove NA's and calculate the counts for each treatment duration category
df_case8 <- df_case8 %>% na.omit() %>% 
  group_by(Case, Duration) %>% summarise(Count = n(), .groups = "drop")

# Add a column for the percentage of each count
df_case8 <- df_case8 %>% group_by(Case) %>% mutate(Percentage = round(Count / sum(Count) * 100, 1))

# Create the bar plot for Case 8
ggplot(df_case8, aes(x = reorder(Duration, -Count), y = Count, fill = Case)) +
  geom_bar(stat = "identity", fill = nejm_colors[1]) +
  geom_text(aes(label = paste0(Percentage, "%")), vjust = -0., hjust = -0.2, color = "black", size = 3) +
  labs(x = NULL, y = "Count", title = "Duration of Antibiotics in Cases 1") +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.1))) +  # Adding some margin
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")




######### CASE ABX duration: Case 2 Duration of ABx

# Install and load the required packages
install.packages(c("ggplot2", "dplyr", "tidyr", "tibble"))
library(ggplot2)
library(dplyr)
library(tidyr)
library(tibble)

# NEJM color palette (blue)
nejm_colors <- c("#386cb0")

# Specify the columns you're interested in for Cases
case9_cols <- c("How long would you treat with Antibiotics, presuming a good response in case 2?")

# Subset the data for these columns (Case 8)
case9_data <- Spondylodiscitis_Variability_Excel_NEW[, case9_cols]

# Melt the data into a long format
df_case9 <- case9_data %>% pivot_longer(everything(), names_to = "Case", values_to = "Duration")

# Remove NA's and calculate the counts for each treatment duration category
df_case9 <- df_case9 %>% na.omit() %>% 
  group_by(Case, Duration) %>% summarise(Count = n(), .groups = "drop")

# Add a column for the percentage of each count
df_case9<- df_case9 %>% group_by(Case) %>% mutate(Percentage = round(Count / sum(Count) * 100, 1))

# Create the bar plot for Case 8
ggplot(df_case9, aes(x = reorder(Duration, -Count), y = Count, fill = Case)) +
  geom_bar(stat = "identity", fill = nejm_colors[1]) +
  geom_text(aes(label = paste0(Percentage, "%")), vjust = -0., hjust = -0.2, color = "black", size = 3) +
  labs(x = NULL, y = "Count", title = "Duration of Antibiotics in Cases 2") +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.1))) +  # Adding some margin
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")




######### CASE ABX duration: Case 5 Duration of ABx

# Install and load the required packages
install.packages(c("ggplot2", "dplyr", "tidyr", "tibble"))
library(ggplot2)
library(dplyr)
library(tidyr)
library(tibble)

# NEJM color palette (blue)
nejm_colors <- c("#386cb0")

# Specify the columns you're interested in for Cases
case10_cols <- c("How long would you treat with Antibiotics, presuming a good response in case 5?")

# Subset the data for these columns (Case)
case10_data <- Spondylodiscitis_Variability_Excel_NEW[, case10_cols]

# Melt the data into a long format
df_case10 <- case10_data %>% pivot_longer(everything(), names_to = "Case", values_to = "Duration")

# Remove NA's and calculate the counts for each treatment duration category
df_case10 <- df_case10 %>% na.omit() %>% 
  group_by(Case, Duration) %>% summarise(Count = n(), .groups = "drop")

# Add a column for the percentage of each count
df_case10<- df_case10 %>% group_by(Case) %>% mutate(Percentage = round(Count / sum(Count) * 100, 1))

# Create the bar plot for Case 8
ggplot(df_case10, aes(x = reorder(Duration, -Count), y = Count, fill = Case)) +
  geom_bar(stat = "identity", fill = nejm_colors[1]) +
  geom_text(aes(label = paste0(Percentage, "%")), vjust = -0., hjust = -0.2, color = "black", size = 3) +
  labs(x = NULL, y = "Count", title = "Duration of Antibiotics in Cases 5") +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.1))) +  # Adding some margin
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")









######### CASE ABx: Case 6 Duration of ABx

# Install and load the required packages
install.packages(c("ggplot2", "dplyr", "tidyr", "tibble"))
library(ggplot2)
library(dplyr)
library(tidyr)
library(tibble)

# NEJM color palette (blue)
nejm_colors <- c("#386cb0")

# Specify the columns you're interested in for Cases
case11_cols <- c("How long would you treat with Antibiotics, presuming a good response in case 6?")

# Subset the data for these columns (Case 8)
case11_data <- Spondylodiscitis_Variability_Excel_NEW[, case11_cols]

# Melt the data into a long format
df_case11 <- case11_data %>% pivot_longer(everything(), names_to = "Case", values_to = "Duration")

# Remove NA's and calculate the counts for each treatment duration category
df_case11 <- df_case11 %>% na.omit() %>% 
  group_by(Case, Duration) %>% summarise(Count = n(), .groups = "drop")

# Add a column for the percentage of each count
df_case11 <- df_case11 %>% group_by(Case) %>% mutate(Percentage = round(Count / sum(Count) * 100, 1))

# Create the bar plot for Case
ggplot(df_case11, aes(x = reorder(Duration, -Count), y = Count, fill = Case)) +
  geom_bar(stat = "identity", fill = nejm_colors[1]) +
  geom_text(aes(label = paste0(Percentage, "%")), hjust = -0.1, color = "white", size = 3) +
  labs(x = NULL, y = "Count", title = "Duration of Antibiotics in Cases 6") +
  coord_flip() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")



######## TYPICAL INDICATIONS FOR SURGERY

# Specify the columns you're interested in
indications_cols <- c("Typical indications for surgery in spinal infections: Epidural empyema with functionally relevant deficit (<=3/5 paresis)",
                      "Typical indications for surgery in spinal infections: Vertebral body destruction",
                      "Typical indications for surgery in spinal infections: To obtain microbiology specimen",
                      "Typical indications for surgery in spinal infections: where other means (blood culture, needle biopsy) have failed",
                      "Typical indications for surgery in spinal infections: Epidural empyema with mild deficit (4/5 paresis)",
                      "Typical indications for surgery in spinal infections: Relevant deformity",
                      "Typical indications for surgery in spinal infections: Septic inflammatory response syndrome (fever, tachycardia, tachypnoea)",
                      "Typical indications for surgery in spinal infections: Epidural empyema without neurodeficit",
                      "Typical indications for surgery in spinal infections: CRP above 200mg/L",
                      "Typical indications for surgery in spinal infections: Psoas muscle abscesses",
                      "Typical indications for surgery in spinal infections: Discitis without endplate destruction",
                      "Typical indications for surgery in spinal infections: Discitis with endplate destruction",
                      "Typical indications for surgery in spinal infections: Advanced age >75years",
                      "Typical indications for surgery in spinal infections: Relevant comorbidities")

# Subset the data for these columns
indications_data <- Spondylodiscitis_Variability_Excel_NEW[, indications_cols]

# Convert data to numeric
indications_data <- apply(indications_data, 2, as.numeric)

# Calculate the sums for each indication
indications_sum <- colSums(indications_data, na.rm = TRUE)

# Create a dataframe for the plot
df_indications <- data.frame(Indication = names(indications_sum), Count = indications_sum)

# Sort the dataframe by count in descending order
df_indications <- df_indications[order(df_indications$Count, decreasing = TRUE), ]

# Create the bar plot
ggplot(df_indications, aes(x = reorder(Indication, -Count), y = Count)) +
  geom_bar(stat = "identity", fill = '#386cb0') +
  labs(x = NULL, y = "Count", title = "Typical Indications for Surgery in Spinal Infections") +
  coord_flip() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




##################### OPTIMISED BAR PLOTS -> need to use these

# NEJM color palette (blue and gray)
nejm_colors <- c("#386cb0")

# Function to calculate percentages and plot bar graph
create_bar_plot <- function(data, title){
  percentage <- round(data / sum(data) * 100, 1)
  df <- data.frame(Treatment = names(data), Count = data, Percentage = percentage)
  df <- df[order(df$Count, decreasing = TRUE), ]
  ggplot(df, aes(x = reorder(Treatment, -Count), y = Count, fill = Treatment)) +
    geom_bar(stat = "identity", fill = nejm_colors[1]) +
    geom_text(aes(label = paste0(Percentage, "%")), hjust = -0.2, color = "black", size = 3, fontface = "bold") +
    labs(x = NULL, y = "Count", title = title) +
    coord_flip() +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
}

# Creating list of column names for each case
list_of_cases <- list(case1_cols, case2_cols, case3_cols, case4_cols, case5_cols, case6_cols, case7_cols)

# Looping over each case
for (i in 1:length(list_of_cases)) {
  case_data <- Spondylodiscitis_Variability_Excel_NEW[, list_of_cases[[i]]]
  case_data <- apply(case_data, 2, as.numeric)
  case_sum <- colSums(case_data, na.rm = TRUE)
  print(create_bar_plot(case_sum, paste("Treatment for Case", i)))
}

# Plotting typical indications for surgery
indications_data <- Spondylodiscitis_Variability_Excel_NEW[, indications_cols]
indications_data <- apply(indications_data, 2, as.numeric)
indications_sum <- colSums(indications_data, na.rm = TRUE)
print(create_bar_plot(indications_sum, "Typical Indications for Surgery in Spinal Infections"))



###################### Descriptive data analysis done ###########################

##################### Inferential data analysis started ###################



############################################ multivariate analysis  



####CASE 1



# Load necessary libraries
library(readxl)
library(dplyr)
library(tidyr)
library(caret)
library(broom)

# Read the Excel file
Spondylodiscitis_Variability_Excel_NEW <- read_excel("OneDrive/Spondylodiscitis Variability/Spondylodiscitis_Variability_Excel_NEW.xlsx")

# Clean the column names
cleaned_column_names <- colnames(Spondylodiscitis_Variability_Excel_NEW) %>%
  gsub("\\W", "_", .) %>%
  gsub("_+", "_", .) %>%
  gsub("^_", "", .) %>%
  gsub("_$", "", .)

# Rename columns in the data frame
colnames(Spondylodiscitis_Variability_Excel_NEW) <- cleaned_column_names

# Define the names of the outcome variables
outcomenames <- c(
  "How_would_you_treat_case_1_Conservative_treatment",
  "How_would_you_treat_case_1_antibiotics",
  "How_would_you_treat_case_1_Decompression_and_posterior_instrumentation",
  "How_would_you_treat_case_1_interbody_fusion",
  "How_would_you_treat_case_1_Posterior_decompression",
  "How_would_you_treat_case_1_Other"
)





# Prepare predictors
predictor_names <- setdiff(cleaned_column_names, outcomenames)
# Exclude columns starting with 'How_'
predictor_names <- predictor_names[!grepl("^How_", predictor_names)]

# Include only predictors where all unique values are 0 or 1
predictor_names <- predictor_names[sapply(predictor_names, function(x) {
  unique_vals <- unique(Spondylodiscitis_Variability_Excel_NEW[[x]])
  all(unique_vals %in% c(0, 1)) & !any(is.na(unique_vals))
})]

predictors <- Spondylodiscitis_Variability_Excel_NEW[, predictor_names]



# Initialize an empty list to store the results
results_list <- list()

# For each outcome...
for (outcome_name in outcomenames) {
  
  # For each predictor...
  for (predictor_name in predictor_names) {
    
    # Subset the relevant columns
    analysis_df <- Spondylodiscitis_Variability_Excel_NEW[, c(outcome_name, predictor_name)]
    
    # Drop any rows where both columns are NA
    analysis_df <- analysis_df[!is.na(analysis_df[outcome_name]) & !is.na(analysis_df[predictor_name]), ]
    
    # Ensure both variables are factors
    analysis_df <- lapply(analysis_df, as.factor)
    
    # Only proceed if both variables have at least two unique levels
    if (length(unique(analysis_df[[outcome_name]])) >= 2 & length(unique(analysis_df[[predictor_name]])) >= 2) {
      
      # Calculate the contingency table
      cont_table <- table(analysis_df[[outcome_name]], analysis_df[[predictor_name]])
      
      # Check the expected cell counts to decide which test to use
      # The expected frequencies can be calculated using the row and column totals
      expected <- outer(margin.table(cont_table, 1), margin.table(cont_table, 2), "*") / sum(cont_table)
      if (any(expected < 5)) {
        # If any cell in the expected contingency table has a count less than 5, use Fisher's exact test
        test_result <- fisher.test(cont_table)
        test_type <- "Fisher's exact"
      } else {
        # Otherwise, use Chi-squared test
        test_result <- chisq.test(cont_table)
        test_type <- "Chi-squared"
      }
      
      # Store the results in the results list
      results_list[[paste(outcome_name, predictor_name, sep = ":")]] <- data.frame(
        test_type = test_type,
        statistic = ifelse(!is.null(test_result$statistic), as.numeric(test_result$statistic), NA),
        df = ifelse(!is.null(test_result$parameter), as.numeric(test_result$parameter), NA),
        p_value = test_result$p.value
      )
    }
  }
}

# Convert results_list to a data frame
results_df <- do.call(rbind, results_list)
results_df$Outcome_Predictor <- rownames(results_df)
rownames(results_df) <- NULL

# Adjust p-values for multiple comparisons using the Bonferroni method
results_df <- results_df %>% 
  mutate(p_value_adj = p.adjust(p_value, method = "bonferroni"))

# Filter for significant results
significant_results <- results_df %>% 
  filter(p_value_adj < 0.05)


# Convert to a table suitable for publication using `kable` and `kableExtra`
significant_results_table <- significant_results %>%
  kable("html", caption = "Significant Results of Statistical Tests") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F)

# Print the table
print(significant_results_table)


library(writexl)

# Write the data frame to an Excel file
write_xlsx(significant_results, path = "significant_results1NEWNEW.xlsx")

















############### Case 1 ABx duration




##### this we will use
# Load necessary libraries
library(readxl)
library(dplyr)
library(tidyr)
library(caret)
library(broom)


# Define the names of the outcome variables
outcomenames <- c(
  "How_long_would_you_treat_with_antibiotics_presuming_a_good_response_in_case_1_Not_sure",
    "How_long_would_you_treat_with_antibiotics_presuming_a_good_response_in_case_1_Two_weeks_i_v_followed_by_10_weeks_p_o",
    "How_long_would_you_treat_with_antibiotics_presuming_a_good_response_in_case_1_Two_weeks_i_v_followed_by_4_weeks_p_o",
    "How_long_would_you_treat_with_antibiotics_presuming_a_good_response_in_case_1_One_week_i_v_followed_by_5_weeks_p_o",
    "How_long_would_you_treat_with_antibiotics_presuming_a_good_response_in_case_1_Other",
    "How_long_would_you_treat_with_antibiotics_presuming_a_good_response_in_case_1_One_week_i_v_followed_by_11_weeks_p_o"
)


# Prepare predictors
predictor_names <- setdiff(cleaned_column_names, outcomenames)
# Exclude columns starting with 'How_'
predictor_names <- predictor_names[!grepl("^How_", predictor_names)]

# Include only predictors where all unique values are 0 or 1
predictor_names <- predictor_names[sapply(predictor_names, function(x) {
  unique_vals <- unique(Spondylodiscitis_Variability_Excel_NEW[[x]])
  all(unique_vals %in% c(0, 1)) & !any(is.na(unique_vals))
})]

predictors <- Spondylodiscitis_Variability_Excel_NEW[, predictor_names]



# Initialize an empty list to store the results
results_list <- list()

# For each outcome...
for (outcome_name in outcomenames) {
  
  # For each predictor...
  for (predictor_name in predictor_names) {
    
    # Subset the relevant columns
    analysis_df <- Spondylodiscitis_Variability_Excel_NEW[, c(outcome_name, predictor_name)]
    
    # Drop any rows where both columns are NA
    analysis_df <- analysis_df[!is.na(analysis_df[outcome_name]) & !is.na(analysis_df[predictor_name]), ]
    
    # Ensure both variables are factors
    analysis_df <- lapply(analysis_df, as.factor)
    
    # Only proceed if both variables have at least two unique levels
    if (length(unique(analysis_df[[outcome_name]])) >= 2 & length(unique(analysis_df[[predictor_name]])) >= 2) {
      
      # Calculate the contingency table
      cont_table <- table(analysis_df[[outcome_name]], analysis_df[[predictor_name]])
      
      # Check the expected cell counts to decide which test to use
      # The expected frequencies can be calculated using the row and column totals
      expected <- outer(margin.table(cont_table, 1), margin.table(cont_table, 2), "*") / sum(cont_table)
      if (any(expected < 5)) {
        # If any cell in the expected contingency table has a count less than 5, use Fisher's exact test
        test_result <- fisher.test(cont_table)
        test_type <- "Fisher's exact"
      } else {
        # Otherwise, use Chi-squared test
        test_result <- chisq.test(cont_table)
        test_type <- "Chi-squared"
      }
      
      # Store the results in the results list
      results_list[[paste(outcome_name, predictor_name, sep = ":")]] <- data.frame(
        test_type = test_type,
        statistic = ifelse(!is.null(test_result$statistic), as.numeric(test_result$statistic), NA),
        df = ifelse(!is.null(test_result$parameter), as.numeric(test_result$parameter), NA),
        p_value = test_result$p.value
      )
    }
  }
}

# Convert results_list to a data frame
results_df <- do.call(rbind, results_list)
results_df$Outcome_Predictor <- rownames(results_df)
rownames(results_df) <- NULL

# Adjust p-values for multiple comparisons using the Bonferroni method
results_df <- results_df %>% 
  mutate(p_value_adj = p.adjust(p_value, method = "bonferroni"))

# Filter for significant results
significant_results <- results_df %>% 
  filter(p_value_adj < 0.05)


# Convert to a table suitable for publication using `kable` and `kableExtra`
significant_results_table <- significant_results %>%
  kable("html", caption = "Significant Results of Statistical Tests") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F)

# Print the table
print(significant_results_table)


library(writexl)

# Write the data frame to an Excel file
write_xlsx(significant_results, path = "significant_results1NEWNEW.xlsx")















############### Case 1  Monitor




##### this we will use
# Load necessary libraries
library(readxl)
library(dplyr)
library(tidyr)
library(caret)
library(broom)


# Clean the column names
cleaned_column_names <- colnames(Spondylodiscitis_Variability_Excel_NEW1Monitor) %>%
  gsub("\\W", "_", .) %>%
  gsub("_+", "_", .) %>%
  gsub("^_", "", .) %>%
  gsub("_$", "", .)


# Define the names of the outcome variables
outcomenames <- c(
  "How_would_you_monitor_treatment_response_of_case_1_more_than_one_option_possible_Repeated_CRP_measurement",
  "How_would_you_monitor_treatment_response_of_case_1_more_than_one_option_possible_MRI_follow_up_during_treatment",
  "How_would_you_monitor_treatment_response_of_case_1_more_than_one_option_possible_MRI_follow_up_at_end_of_treatment",
  "How_would_you_monitor_treatment_response_of_case_1_more_than_one_option_possible_Repeated_WBCC_measurement",
  "How_would_you_monitor_treatment_response_of_case_1_more_than_one_option_possible_Repeated_erythrocyte_sedimentation_rate_assessment",
  "How_would_you_monitor_treatment_response_of_case_1_more_than_one_option_possible_Other",
  "How_would_you_monitor_treatment_response_of_case_1_more_than_one_option_possible_NA"
)



# Prepare predictors
predictor_names <- setdiff(cleaned_column_names, outcomenames)
# Exclude columns starting with 'How_'
predictor_names <- predictor_names[!grepl("^How_", predictor_names)]

# Include only predictors where all unique values are 0 or 1
predictor_names <- predictor_names[sapply(predictor_names, function(x) {
  unique_vals <- unique(Spondylodiscitis_Variability_Excel_NEW[[x]])
  all(unique_vals %in% c(0, 1)) & !any(is.na(unique_vals))
})]

predictors <- Spondylodiscitis_Variability_Excel_NEW[, predictor_names]



# Initialize an empty list to store the results
results_list <- list()

# For each outcome...
for (outcome_name in outcomenames) {
  
  # For each predictor...
  for (predictor_name in predictor_names) {
    
    # Subset the relevant columns
    analysis_df <- Spondylodiscitis_Variability_Excel_NEW[, c(outcome_name, predictor_name)]
    
    # Drop any rows where both columns are NA
    analysis_df <- analysis_df[!is.na(analysis_df[outcome_name]) & !is.na(analysis_df[predictor_name]), ]
    
    # Ensure both variables are factors
    analysis_df <- lapply(analysis_df, as.factor)
    
    # Only proceed if both variables have at least two unique levels
    if (length(unique(analysis_df[[outcome_name]])) >= 2 & length(unique(analysis_df[[predictor_name]])) >= 2) {
      
      # Calculate the contingency table
      cont_table <- table(analysis_df[[outcome_name]], analysis_df[[predictor_name]])
      
      # Check the expected cell counts to decide which test to use
      # The expected frequencies can be calculated using the row and column totals
      expected <- outer(margin.table(cont_table, 1), margin.table(cont_table, 2), "*") / sum(cont_table)
      if (any(expected < 5)) {
        # If any cell in the expected contingency table has a count less than 5, use Fisher's exact test
        test_result <- fisher.test(cont_table)
        test_type <- "Fisher's exact"
      } else {
        # Otherwise, use Chi-squared test
        test_result <- chisq.test(cont_table)
        test_type <- "Chi-squared"
      }
      
      # Store the results in the results list
      results_list[[paste(outcome_name, predictor_name, sep = ":")]] <- data.frame(
        test_type = test_type,
        statistic = ifelse(!is.null(test_result$statistic), as.numeric(test_result$statistic), NA),
        df = ifelse(!is.null(test_result$parameter), as.numeric(test_result$parameter), NA),
        p_value = test_result$p.value
      )
    }
  }
}

# Convert results_list to a data frame
results_df <- do.call(rbind, results_list)
results_df$Outcome_Predictor <- rownames(results_df)
rownames(results_df) <- NULL

# Adjust p-values for multiple comparisons using the Bonferroni method
results_df <- results_df %>% 
  mutate(p_value_adj = p.adjust(p_value, method = "bonferroni"))

# Filter for significant results
significant_results <- results_df %>% 
  filter(p_value_adj < 0.05)


# Convert to a table suitable for publication using `kable` and `kableExtra`
significant_results_table <- significant_results %>%
  kable("html", caption = "Significant Results of Statistical Tests") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F)

# Print the table
print(significant_results_table)


library(writexl)

# Write the data frame to an Excel file
write_xlsx(significant_results, path = "significant_results3NEWNEW.xlsx")








#############'Case 2




# Load necessary libraries
library(readxl)
library(dplyr)
library(tidyr)
library(caret)
library(broom)

# Read the Excel file
Spondylodiscitis_Variability_Excel_NEW <- read_excel("OneDrive/Spondylodiscitis Variability/Spondylodiscitis_Variability_Excel_NEW.xlsx")




# Clean the column names
cleaned_column_names <- colnames(Spondylodiscitis_Variability_Excel_NEW) %>%
  gsub("\\W", "_", .) %>%
  gsub("_+", "_", .) %>%
  gsub("^_", "", .) 

# Rename columns in the data frame
colnames(Spondylodiscitis_Variability_Excel_NEW) <- cleaned_column_names


# Define the names of the outcome variables
outcomenames <- c(
  "How_would_you_treat_case_2_Not_sure",
  "How_would_you_treat_case_2_Conservative_treatment",
  "How_would_you_treat_case_2_antibiotics",
  "How_would_you_treat_case_2_Decompression_and_posterior_instrumentation",
  "How_would_you_treat_case_2_anterior_reconstruction",
  "How_would_you_treat_case_2_Posterior_decompression",
  "How_would_you_treat_case_2_Other",
  "How_would_you_treat_case_2_interbody_fusion"
)



# Prepare predictors
predictor_names <- setdiff(cleaned_column_names, outcomenames)
# Exclude columns starting with 'How_'
predictor_names <- predictor_names[!grepl("^How_", predictor_names)]

# Include only predictors where all unique values are 0 or 1
predictor_names <- predictor_names[sapply(predictor_names, function(x) {
  unique_vals <- unique(Spondylodiscitis_Variability_Excel_NEW[[x]])
  all(unique_vals %in% c(0, 1)) & !any(is.na(unique_vals))
})]

predictors <- Spondylodiscitis_Variability_Excel_NEW[, predictor_names]



# Initialize an empty list to store the results
results_list <- list()

# For each outcome...
for (outcome_name in outcomenames) {
  
  # For each predictor...
  for (predictor_name in predictor_names) {
    
    # Subset the relevant columns
    analysis_df <- Spondylodiscitis_Variability_Excel_NEW[, c(outcome_name, predictor_name)]
    
    # Drop any rows where both columns are NA
    analysis_df <- analysis_df[!is.na(analysis_df[outcome_name]) & !is.na(analysis_df[predictor_name]), ]
    
    # Ensure both variables are factors
    analysis_df <- lapply(analysis_df, as.factor)
    
    # Only proceed if both variables have at least two unique levels
    if (length(unique(analysis_df[[outcome_name]])) >= 2 & length(unique(analysis_df[[predictor_name]])) >= 2) {
      
      # Calculate the contingency table
      cont_table <- table(analysis_df[[outcome_name]], analysis_df[[predictor_name]])
      
      # Check the expected cell counts to decide which test to use
      # The expected frequencies can be calculated using the row and column totals
      expected <- outer(margin.table(cont_table, 1), margin.table(cont_table, 2), "*") / sum(cont_table)
      if (any(expected < 5)) {
        # If any cell in the expected contingency table has a count less than 5, use Fisher's exact test
        test_result <- fisher.test(cont_table)
        test_type <- "Fisher's exact"
      } else {
        # Otherwise, use Chi-squared test
        test_result <- chisq.test(cont_table)
        test_type <- "Chi-squared"
      }
      
      # Store the results in the results list
      results_list[[paste(outcome_name, predictor_name, sep = ":")]] <- data.frame(
        test_type = test_type,
        statistic = ifelse(!is.null(test_result$statistic), as.numeric(test_result$statistic), NA),
        df = ifelse(!is.null(test_result$parameter), as.numeric(test_result$parameter), NA),
        p_value = test_result$p.value
      )
    }
  }
}

# Convert results_list to a data frame
results_df <- do.call(rbind, results_list)
results_df$Outcome_Predictor <- rownames(results_df)
rownames(results_df) <- NULL

# Adjust p-values for multiple comparisons using the Bonferroni method
results_df <- results_df %>% 
  mutate(p_value_adj = p.adjust(p_value, method = "bonferroni"))

# Filter for significant results
significant_results <- results_df %>% 
  filter(p_value_adj < 0.05)


# Convert to a table suitable for publication using `kable` and `kableExtra`
significant_results_table <- significant_results %>%
  kable("html", caption = "Significant Results of Statistical Tests") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F)

# Print the table
print(significant_results_table)


library(writexl)

# Write the data frame to an Excel file
write_xlsx(significant_results, path = "significant_results4NEWNEW.xlsx")






#Case 2 ABx



# Load necessary libraries
library(readxl)
library(dplyr)
library(tidyr)
library(caret)
library(broom)

# Read the Excel file
Spondylodiscitis_Variability_Excel_NEW2ABx <- read_excel("OneDrive/Spondylodiscitis Variability/Spondylodiscitis_Variability_Excel_NEW2ABx.xlsx")

# Clean the column names
cleaned_column_names <- colnames(Spondylodiscitis_Variability_Excel_NEW2ABx) %>%
  gsub("\\W", "_", .) %>%
  gsub("_+", "_", .) %>%
  gsub("^_", "", .) %>%
  gsub("_$", "", .)

# Rename columns in the data frame
colnames(Spondylodiscitis_Variability_Excel_NEW) <- cleaned_column_names

# Define the names of the outcome variables
outcomenames <- c(
  "How_long_would_you_treat_with_antibiotics_presuming_a_good_response_in_case_2_Not_sure",
  "How_long_would_you_treat_with_antibiotics_presuming_a_good_response_in_case_2_Two_weeks_i_v_followed_by_4_weeks_p_o_",
  "How_long_would_you_treat_with_antibiotics_presuming_a_good_response_in_case_2_Two_weeks_i_v_followed_by_10_weeks_p_o_",
  "How_long_would_you_treat_with_antibiotics_presuming_a_good_response_in_case_2_One_week_i_v_followed_by_5_weeks_p_o_",
  "How_long_would_you_treat_with_antibiotics_presuming_a_good_response_in_case_2_Other",
  "How_long_would_you_treat_with_antibiotics_presuming_a_good_response_in_case_2_One_week_i_v_followed_by_11_weeks_p_o_"
)




# Prepare predictors
predictor_names <- setdiff(cleaned_column_names, outcomenames)
# Exclude columns starting with 'How_'
predictor_names <- predictor_names[!grepl("^How_", predictor_names)]

# Include only predictors where all unique values are 0 or 1
predictor_names <- predictor_names[sapply(predictor_names, function(x) {
  unique_vals <- unique(Spondylodiscitis_Variability_Excel_NEW[[x]])
  all(unique_vals %in% c(0, 1)) & !any(is.na(unique_vals))
})]

predictors <- Spondylodiscitis_Variability_Excel_NEW[, predictor_names]



# Initialize an empty list to store the results
results_list <- list()

# For each outcome...
for (outcome_name in outcomenames) {
  
  # For each predictor...
  for (predictor_name in predictor_names) {
    
    # Subset the relevant columns
    analysis_df <- Spondylodiscitis_Variability_Excel_NEW[, c(outcome_name, predictor_name)]
    
    # Drop any rows where both columns are NA
    analysis_df <- analysis_df[!is.na(analysis_df[outcome_name]) & !is.na(analysis_df[predictor_name]), ]
    
    # Ensure both variables are factors
    analysis_df <- lapply(analysis_df, as.factor)
    
    # Only proceed if both variables have at least two unique levels
    if (length(unique(analysis_df[[outcome_name]])) >= 2 & length(unique(analysis_df[[predictor_name]])) >= 2) {
      
      # Calculate the contingency table
      cont_table <- table(analysis_df[[outcome_name]], analysis_df[[predictor_name]])
      
      # Check the expected cell counts to decide which test to use
      # The expected frequencies can be calculated using the row and column totals
      expected <- outer(margin.table(cont_table, 1), margin.table(cont_table, 2), "*") / sum(cont_table)
      if (any(expected < 5)) {
        # If any cell in the expected contingency table has a count less than 5, use Fisher's exact test
        test_result <- fisher.test(cont_table)
        test_type <- "Fisher's exact"
      } else {
        # Otherwise, use Chi-squared test
        test_result <- chisq.test(cont_table)
        test_type <- "Chi-squared"
      }
      
      # Store the results in the results list
      results_list[[paste(outcome_name, predictor_name, sep = ":")]] <- data.frame(
        test_type = test_type,
        statistic = ifelse(!is.null(test_result$statistic), as.numeric(test_result$statistic), NA),
        df = ifelse(!is.null(test_result$parameter), as.numeric(test_result$parameter), NA),
        p_value = test_result$p.value
      )
    }
  }
}

# Convert results_list to a data frame
results_df <- do.call(rbind, results_list)
results_df$Outcome_Predictor <- rownames(results_df)
rownames(results_df) <- NULL

# Adjust p-values for multiple comparisons using the Bonferroni method
results_df <- results_df %>% 
  mutate(p_value_adj = p.adjust(p_value, method = "bonferroni"))

# Filter for significant results
significant_results <- results_df %>% 
  filter(p_value_adj < 0.05)


# Convert to a table suitable for publication using `kable` and `kableExtra`
significant_results_table <- significant_results %>%
  kable("html", caption = "Significant Results of Statistical Tests") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F)

# Print the table
print(significant_results_table)


library(writexl)

# Write the data frame to an Excel file
write_xlsx(significant_results, path = "significant_results5NEWNEW.xlsx")






#Case 2 Monitor



# Load necessary libraries
library(readxl)
library(dplyr)
library(tidyr)
library(caret)
library(broom)



# Define the names of the outcome variables
outcomenames <- c(
  "How_would_you_monitor_treatment_response_in_case_2_more_than_one_option_possible_Not_sure",
  "How_would_you_monitor_treatment_response_in_case_2_more_than_one_option_possible_Repeated_CRP_measurement",
  "How_would_you_monitor_treatment_response_in_case_2_more_than_one_option_possible_Repeated_WBCC_measurement",
  "How_would_you_monitor_treatment_response_in_case_2_more_than_one_option_possible_Repeated_erythrocyte_sedimentation_rate_assessment",
  "How_would_you_monitor_treatment_response_in_case_2_more_than_one_option_possible_MRI_follow_up_during_treatment",
  "How_would_you_monitor_treatment_response_in_case_2_more_than_one_option_possible_MRI_follow_up_at_end_of_treatment",
  "How_would_you_monitor_treatment_response_in_case_2_more_than_one_option_possible_Other"
)


# Prepare predictors
predictor_names <- setdiff(cleaned_column_names, outcomenames)
# Exclude columns starting with 'How_'
predictor_names <- predictor_names[!grepl("^How_", predictor_names)]

# Include only predictors where all unique values are 0 or 1
predictor_names <- predictor_names[sapply(predictor_names, function(x) {
  unique_vals <- unique(Spondylodiscitis_Variability_Excel_NEW[[x]])
  all(unique_vals %in% c(0, 1)) & !any(is.na(unique_vals))
})]

predictors <- Spondylodiscitis_Variability_Excel_NEW[, predictor_names]



# Initialize an empty list to store the results
results_list <- list()

# For each outcome...
for (outcome_name in outcomenames) {
  
  # For each predictor...
  for (predictor_name in predictor_names) {
    
    # Subset the relevant columns
    analysis_df <- Spondylodiscitis_Variability_Excel_NEW[, c(outcome_name, predictor_name)]
    
    # Drop any rows where both columns are NA
    analysis_df <- analysis_df[!is.na(analysis_df[outcome_name]) & !is.na(analysis_df[predictor_name]), ]
    
    # Ensure both variables are factors
    analysis_df <- lapply(analysis_df, as.factor)
    
    # Only proceed if both variables have at least two unique levels
    if (length(unique(analysis_df[[outcome_name]])) >= 2 & length(unique(analysis_df[[predictor_name]])) >= 2) {
      
      # Calculate the contingency table
      cont_table <- table(analysis_df[[outcome_name]], analysis_df[[predictor_name]])
      
      # Check the expected cell counts to decide which test to use
      # The expected frequencies can be calculated using the row and column totals
      expected <- outer(margin.table(cont_table, 1), margin.table(cont_table, 2), "*") / sum(cont_table)
      if (any(expected < 5)) {
        # If any cell in the expected contingency table has a count less than 5, use Fisher's exact test
        test_result <- fisher.test(cont_table)
        test_type <- "Fisher's exact"
      } else {
        # Otherwise, use Chi-squared test
        test_result <- chisq.test(cont_table)
        test_type <- "Chi-squared"
      }
      
      # Store the results in the results list
      results_list[[paste(outcome_name, predictor_name, sep = ":")]] <- data.frame(
        test_type = test_type,
        statistic = ifelse(!is.null(test_result$statistic), as.numeric(test_result$statistic), NA),
        df = ifelse(!is.null(test_result$parameter), as.numeric(test_result$parameter), NA),
        p_value = test_result$p.value
      )
    }
  }
}

# Convert results_list to a data frame
results_df <- do.call(rbind, results_list)
results_df$Outcome_Predictor <- rownames(results_df)
rownames(results_df) <- NULL

# Adjust p-values for multiple comparisons using the Bonferroni method
results_df <- results_df %>% 
  mutate(p_value_adj = p.adjust(p_value, method = "bonferroni"))

# Filter for significant results
significant_results <- results_df %>% 
  filter(p_value_adj < 0.05)


# Convert to a table suitable for publication using `kable` and `kableExtra`
significant_results_table <- significant_results %>%
  kable("html", caption = "Significant Results of Statistical Tests") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F)

# Print the table
print(significant_results_table)


library(writexl)

# Write the data frame to an Excel file
write_xlsx(significant_results, path = "significant_results5NEWNEW.xlsx")













#Case 3



# Load necessary libraries
library(readxl)
library(dplyr)
library(tidyr)
library(caret)
library(broom)

# Read the Excel file
Spondylodiscitis_Variability_Excel_NEW3 <- read_excel("OneDrive/Spondylodiscitis Variability/Spondylodiscitis_Variability_Excel_NEW3.xlsx")

# Clean the column names
cleaned_column_names <- colnames(Spondylodiscitis_Variability_Excel_NEW3) %>%
  gsub("\\W", "_", .) %>%
  gsub("_+", "_", .) %>%
  gsub("^_", "", .) %>%
  gsub("_$", "", .)

# Rename columns in the data frame
colnames(Spondylodiscitis_Variability_Excel_NEW3) <- cleaned_column_names

# Define the names of the outcome variables
outcomenames <- c(
  "How_would_you_treat_case_3_antibiotics",
  "How_would_you_treat_case_3_Conservative_treatment",
  "How_would_you_treat_case_3_Decompression_and_posterior_instrumentation",
  "How_would_you_treat_case_3_interbody_fusion",
  "How_would_you_treat_case_3_Not_sure",
  "How_would_you_treat_case_3_NA",
  "How_would_you_treat_case_3_Other",
  "How_would_you_treat_case_3_Posterior_decompression"
)

# Prepare predictors
predictor_names <- setdiff(cleaned_column_names, outcomenames)
# Exclude columns starting with 'How_'
predictor_names <- predictor_names[!grepl("^How_", predictor_names)]

# Include only predictors where all unique values are 0 or 1
predictor_names <- predictor_names[sapply(predictor_names, function(x) {
  unique_vals <- unique(Spondylodiscitis_Variability_Excel_NEW[[x]])
  all(unique_vals %in% c(0, 1)) & !any(is.na(unique_vals))
})]

predictors <- Spondylodiscitis_Variability_Excel_NEW[, predictor_names]



# Initialize an empty list to store the results
results_list <- list()

# For each outcome...
for (outcome_name in outcomenames) {
  
  # For each predictor...
  for (predictor_name in predictor_names) {
    
    # Subset the relevant columns
    analysis_df <- Spondylodiscitis_Variability_Excel_NEW[, c(outcome_name, predictor_name)]
    
    # Drop any rows where both columns are NA
    analysis_df <- analysis_df[!is.na(analysis_df[outcome_name]) & !is.na(analysis_df[predictor_name]), ]
    
    # Ensure both variables are factors
    analysis_df <- lapply(analysis_df, as.factor)
    
    # Only proceed if both variables have at least two unique levels
    if (length(unique(analysis_df[[outcome_name]])) >= 2 & length(unique(analysis_df[[predictor_name]])) >= 2) {
      
      # Calculate the contingency table
      cont_table <- table(analysis_df[[outcome_name]], analysis_df[[predictor_name]])
      
      # Check the expected cell counts to decide which test to use
      # The expected frequencies can be calculated using the row and column totals
      expected <- outer(margin.table(cont_table, 1), margin.table(cont_table, 2), "*") / sum(cont_table)
      if (any(expected < 5)) {
        # If any cell in the expected contingency table has a count less than 5, use Fisher's exact test
        test_result <- fisher.test(cont_table)
        test_type <- "Fisher's exact"
      } else {
        # Otherwise, use Chi-squared test
        test_result <- chisq.test(cont_table)
        test_type <- "Chi-squared"
      }
      
      # Store the results in the results list
      results_list[[paste(outcome_name, predictor_name, sep = ":")]] <- data.frame(
        test_type = test_type,
        statistic = ifelse(!is.null(test_result$statistic), as.numeric(test_result$statistic), NA),
        df = ifelse(!is.null(test_result$parameter), as.numeric(test_result$parameter), NA),
        p_value = test_result$p.value
      )
    }
  }
}

# Convert results_list to a data frame
results_df <- do.call(rbind, results_list)
results_df$Outcome_Predictor <- rownames(results_df)
rownames(results_df) <- NULL

# Adjust p-values for multiple comparisons using the Bonferroni method
results_df <- results_df %>% 
  mutate(p_value_adj = p.adjust(p_value, method = "bonferroni"))

# Filter for significant results
significant_results <- results_df %>% 
  filter(p_value_adj < 0.05)


# Convert to a table suitable for publication using `kable` and `kableExtra`
significant_results_table <- significant_results %>%
  kable("html", caption = "Significant Results of Statistical Tests") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F)

# Print the table
print(significant_results_table)


library(writexl)

# Write the data frame to an Excel file
write_xlsx(significant_results, path = "significant_results3NEWNEW.xlsx")





#Case 4



# Load necessary libraries
library(readxl)
library(dplyr)
library(tidyr)
library(caret)
library(broom)

# Read the Excel file
Spondylodiscitis_Variability_Excel_NEW4 <- read_excel("OneDrive/Spondylodiscitis Variability/Spondylodiscitis_Variability_Excel_NEW4.xlsx")

# Clean the column names
cleaned_column_names <- colnames(Spondylodiscitis_Variability_Excel_NEW4) %>%
  gsub("\\W", "_", .) %>%
  gsub("_+", "_", .) %>%
  gsub("^_", "", .) %>%
  gsub("_$", "", .)

# Rename columns in the data frame
colnames(Spondylodiscitis_Variability_Excel_NEW4) <- cleaned_column_names

# Define the names of the outcome variables
outcomenames <- c(
  "How_would_you_treat_case_4_Conservative_treatment",
  "How_would_you_treat_case_4_antibiotics",
  "How_would_you_treat_case_4_Decompression_and_posterior_instrumentation",
  "How_would_you_treat_case_4_interbody_fusion",
  "How_would_you_treat_case_4_Posterior_decompression",
  "How_would_you_treat_case_4_Not_sure",
  "How_would_you_treat_case_4_Other",
  "How_would_you_treat_case_4_NA"
)


# Prepare predictors
predictor_names <- setdiff(cleaned_column_names, outcomenames)
# Exclude columns starting with 'How_'
predictor_names <- predictor_names[!grepl("^How_", predictor_names)]

# Include only predictors where all unique values are 0 or 1
predictor_names <- predictor_names[sapply(predictor_names, function(x) {
  unique_vals <- unique(Spondylodiscitis_Variability_Excel_NEW[[x]])
  all(unique_vals %in% c(0, 1)) & !any(is.na(unique_vals))
})]

predictors <- Spondylodiscitis_Variability_Excel_NEW[, predictor_names]



# Initialize an empty list to store the results
results_list <- list()

# For each outcome...
for (outcome_name in outcomenames) {
  
  # For each predictor...
  for (predictor_name in predictor_names) {
    
    # Subset the relevant columns
    analysis_df <- Spondylodiscitis_Variability_Excel_NEW[, c(outcome_name, predictor_name)]
    
    # Drop any rows where both columns are NA
    analysis_df <- analysis_df[!is.na(analysis_df[outcome_name]) & !is.na(analysis_df[predictor_name]), ]
    
    # Ensure both variables are factors
    analysis_df <- lapply(analysis_df, as.factor)
    
    # Only proceed if both variables have at least two unique levels
    if (length(unique(analysis_df[[outcome_name]])) >= 2 & length(unique(analysis_df[[predictor_name]])) >= 2) {
      
      # Calculate the contingency table
      cont_table <- table(analysis_df[[outcome_name]], analysis_df[[predictor_name]])
      
      # Check the expected cell counts to decide which test to use
      # The expected frequencies can be calculated using the row and column totals
      expected <- outer(margin.table(cont_table, 1), margin.table(cont_table, 2), "*") / sum(cont_table)
      if (any(expected < 5)) {
        # If any cell in the expected contingency table has a count less than 5, use Fisher's exact test
        test_result <- fisher.test(cont_table)
        test_type <- "Fisher's exact"
      } else {
        # Otherwise, use Chi-squared test
        test_result <- chisq.test(cont_table)
        test_type <- "Chi-squared"
      }
      
      # Store the results in the results list
      results_list[[paste(outcome_name, predictor_name, sep = ":")]] <- data.frame(
        test_type = test_type,
        statistic = ifelse(!is.null(test_result$statistic), as.numeric(test_result$statistic), NA),
        df = ifelse(!is.null(test_result$parameter), as.numeric(test_result$parameter), NA),
        p_value = test_result$p.value
      )
    }
  }
}

# Convert results_list to a data frame
results_df <- do.call(rbind, results_list)
results_df$Outcome_Predictor <- rownames(results_df)
rownames(results_df) <- NULL

# Adjust p-values for multiple comparisons using the Bonferroni method
results_df <- results_df %>% 
  mutate(p_value_adj = p.adjust(p_value, method = "bonferroni"))

# Filter for significant results
significant_results <- results_df %>% 
  filter(p_value_adj < 0.05)


# Convert to a table suitable for publication using `kable` and `kableExtra`
significant_results_table <- significant_results %>%
  kable("html", caption = "Significant Results of Statistical Tests") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F)

# Print the table
print(significant_results_table)


library(writexl)

# Write the data frame to an Excel file
write_xlsx(significant_results, path = "significant_results4NEWNEW.xlsx")









#Case 5 



# Load necessary libraries
library(readxl)
library(dplyr)
library(tidyr)
library(caret)
library(broom)

# Read the Excel file
Spondylodiscitis_Variability_Excel_NEW5 <- read_excel("OneDrive/Spondylodiscitis Variability/Spondylodiscitis_Variability_Excel_NEW5.xlsx")

# Clean the column names
cleaned_column_names <- colnames(Spondylodiscitis_Variability_Excel_NEW5) %>%
  gsub("\\W", "_", .) %>%
  gsub("_+", "_", .) %>%
  gsub("^_", "", .) %>%
  gsub("_$", "", .)

# Rename columns in the data frame
colnames(Spondylodiscitis_Variability_Excel_NEW5) <- cleaned_column_names

# Define the names of the outcome variables
outcomenames <- c(
  "How_would_you_treat_case_5_Conservative_treatment",
  "How_would_you_treat_case_5_antibiotics",
  "How_would_you_treat_case_5_Conservative_antibiotic_first_for_several_days",
  "How_would_you_treat_case_5_surgery_after_stabilization_of_pt_general_status",
  "How_would_you_treat_case_5_Not_sure",
  "How_would_you_treat_case_5_Early_decompression_and_posterior_instrumentation",
  "How_would_you_treat_case_5_inter_body_fusion",
  "How_would_you_treat_case_5_Early_posterior_decompression",
  "How_would_you_treat_case_5_no_instrumentation"
)


# Prepare predictors
predictor_names <- setdiff(cleaned_column_names, outcomenames)
# Exclude columns starting with 'How_'
predictor_names <- predictor_names[!grepl("^How_", predictor_names)]

# Include only predictors where all unique values are 0 or 1
predictor_names <- predictor_names[sapply(predictor_names, function(x) {
  unique_vals <- unique(Spondylodiscitis_Variability_Excel_NEW[[x]])
  all(unique_vals %in% c(0, 1)) & !any(is.na(unique_vals))
})]

predictors <- Spondylodiscitis_Variability_Excel_NEW[, predictor_names]



# Initialize an empty list to store the results
results_list <- list()

# For each outcome...
for (outcome_name in outcomenames) {
  
  # For each predictor...
  for (predictor_name in predictor_names) {
    
    # Subset the relevant columns
    analysis_df <- Spondylodiscitis_Variability_Excel_NEW[, c(outcome_name, predictor_name)]
    
    # Drop any rows where both columns are NA
    analysis_df <- analysis_df[!is.na(analysis_df[outcome_name]) & !is.na(analysis_df[predictor_name]), ]
    
    # Ensure both variables are factors
    analysis_df <- lapply(analysis_df, as.factor)
    
    # Only proceed if both variables have at least two unique levels
    if (length(unique(analysis_df[[outcome_name]])) >= 2 & length(unique(analysis_df[[predictor_name]])) >= 2) {
      
      # Calculate the contingency table
      cont_table <- table(analysis_df[[outcome_name]], analysis_df[[predictor_name]])
      
      # Check the expected cell counts to decide which test to use
      # The expected frequencies can be calculated using the row and column totals
      expected <- outer(margin.table(cont_table, 1), margin.table(cont_table, 2), "*") / sum(cont_table)
      if (any(expected < 5)) {
        # If any cell in the expected contingency table has a count less than 5, use Fisher's exact test
        test_result <- fisher.test(cont_table)
        test_type <- "Fisher's exact"
      } else {
        # Otherwise, use Chi-squared test
        test_result <- chisq.test(cont_table)
        test_type <- "Chi-squared"
      }
      
      # Store the results in the results list
      results_list[[paste(outcome_name, predictor_name, sep = ":")]] <- data.frame(
        test_type = test_type,
        statistic = ifelse(!is.null(test_result$statistic), as.numeric(test_result$statistic), NA),
        df = ifelse(!is.null(test_result$parameter), as.numeric(test_result$parameter), NA),
        p_value = test_result$p.value
      )
    }
  }
}

# Convert results_list to a data frame
results_df <- do.call(rbind, results_list)
results_df$Outcome_Predictor <- rownames(results_df)
rownames(results_df) <- NULL

# Adjust p-values for multiple comparisons using the Bonferroni method
results_df <- results_df %>% 
  mutate(p_value_adj = p.adjust(p_value, method = "bonferroni"))

# Filter for significant results
significant_results <- results_df %>% 
  filter(p_value_adj < 0.05)


# Convert to a table suitable for publication using `kable` and `kableExtra`
significant_results_table <- significant_results %>%
  kable("html", caption = "Significant Results of Statistical Tests") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F)

# Print the table
print(significant_results_table)


library(writexl)

# Write the data frame to an Excel file
write_xlsx(significant_results, path = "significant_results5NEWNEW.xlsx")










#Case 5 ABx



# Load necessary libraries
library(readxl)
library(dplyr)
library(tidyr)
library(caret)
library(broom)

# Read the Excel file
Spondylodiscitis_Variability_Excel_NEW5ABx <- read_excel("OneDrive/Spondylodiscitis Variability/Spondylodiscitis_Variability_Excel_NEW5ABx.xlsx")

# Clean the column names
cleaned_column_names <- colnames(Spondylodiscitis_Variability_Excel_NEW5ABx) %>%
  gsub("\\W", "_", .) %>%
  gsub("_+", "_", .) %>%
  gsub("^_", "", .) %>%
  gsub("_$", "", .)

# Rename columns in the data frame
colnames(Spondylodiscitis_Variability_Excel_NEW5ABx) <- cleaned_column_names

# Define the names of the outcome variables
outcomenames <- c(
  "How_long_would_you_treat_with_antibiotics_presuming_a_good_response_in_case_5_Not_sure",
  "How_long_would_you_treat_with_antibiotics_presuming_a_good_response_in_case_5_Two_weeks_i_v_followed_by_10_weeks_p_o",
  "How_long_would_you_treat_with_antibiotics_presuming_a_good_response_in_case_5_Two_weeks_i_v_followed_by_4_weeks_p_o",
  "How_long_would_you_treat_with_antibiotics_presuming_a_good_response_in_case_5_Other",
  "How_long_would_you_treat_with_antibiotics_presuming_a_good_response_in_case_5_One_week_i_v_followed_by_11_weeks_p_o",
  "How_long_would_you_treat_with_antibiotics_presuming_a_good_response_in_case_5_One_week_i_v_followed_by_5_weeks_p_o",
  "How_long_would_you_treat_with_antibiotics_presuming_a_good_response_in_case_5_NA"
)


# Prepare predictors
predictor_names <- setdiff(cleaned_column_names, outcomenames)
# Exclude columns starting with 'How_'
predictor_names <- predictor_names[!grepl("^How_", predictor_names)]

# Include only predictors where all unique values are 0 or 1
predictor_names <- predictor_names[sapply(predictor_names, function(x) {
  unique_vals <- unique(Spondylodiscitis_Variability_Excel_NEW[[x]])
  all(unique_vals %in% c(0, 1)) & !any(is.na(unique_vals))
})]

predictors <- Spondylodiscitis_Variability_Excel_NEW[, predictor_names]



# Initialize an empty list to store the results
results_list <- list()

# For each outcome...
for (outcome_name in outcomenames) {
  
  # For each predictor...
  for (predictor_name in predictor_names) {
    
    # Subset the relevant columns
    analysis_df <- Spondylodiscitis_Variability_Excel_NEW[, c(outcome_name, predictor_name)]
    
    # Drop any rows where both columns are NA
    analysis_df <- analysis_df[!is.na(analysis_df[outcome_name]) & !is.na(analysis_df[predictor_name]), ]
    
    # Ensure both variables are factors
    analysis_df <- lapply(analysis_df, as.factor)
    
    # Only proceed if both variables have at least two unique levels
    if (length(unique(analysis_df[[outcome_name]])) >= 2 & length(unique(analysis_df[[predictor_name]])) >= 2) {
      
      # Calculate the contingency table
      cont_table <- table(analysis_df[[outcome_name]], analysis_df[[predictor_name]])
      
      # Check the expected cell counts to decide which test to use
      # The expected frequencies can be calculated using the row and column totals
      expected <- outer(margin.table(cont_table, 1), margin.table(cont_table, 2), "*") / sum(cont_table)
      if (any(expected < 5)) {
        # If any cell in the expected contingency table has a count less than 5, use Fisher's exact test
        test_result <- fisher.test(cont_table)
        test_type <- "Fisher's exact"
      } else {
        # Otherwise, use Chi-squared test
        test_result <- chisq.test(cont_table)
        test_type <- "Chi-squared"
      }
      
      # Store the results in the results list
      results_list[[paste(outcome_name, predictor_name, sep = ":")]] <- data.frame(
        test_type = test_type,
        statistic = ifelse(!is.null(test_result$statistic), as.numeric(test_result$statistic), NA),
        df = ifelse(!is.null(test_result$parameter), as.numeric(test_result$parameter), NA),
        p_value = test_result$p.value
      )
    }
  }
}

# Convert results_list to a data frame
results_df <- do.call(rbind, results_list)
results_df$Outcome_Predictor <- rownames(results_df)
rownames(results_df) <- NULL

# Adjust p-values for multiple comparisons using the Bonferroni method
results_df <- results_df %>% 
  mutate(p_value_adj = p.adjust(p_value, method = "bonferroni"))

# Filter for significant results
significant_results <- results_df %>% 
  filter(p_value_adj < 0.05)


# Convert to a table suitable for publication using `kable` and `kableExtra`
significant_results_table <- significant_results %>%
  kable("html", caption = "Significant Results of Statistical Tests") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F)

# Print the table
print(significant_results_table)


library(writexl)

# Write the data frame to an Excel file
write_xlsx(significant_results, path = "significant_results5ABXNEWNEW.xlsx")









#Case 6



# Load necessary libraries
library(readxl)
library(dplyr)
library(tidyr)
library(caret)
library(broom)

# Read the Excel file
Spondylodiscitis_Variability_Excel_NEW6 <- read_excel("OneDrive/Spondylodiscitis Variability/Spondylodiscitis_Variability_Excel_NEW6.xlsx")

# Clean the column names
cleaned_column_names <- colnames(Spondylodiscitis_Variability_Excel_NEW6) %>%
  gsub("\\W", "_", .) %>%
  gsub("_+", "_", .) %>%
  gsub("^_", "", .) %>%
  gsub("_$", "", .)

# Rename columns in the data frame
colnames(Spondylodiscitis_Variability_Excel_NEW6) <- cleaned_column_names

# Define the names of the outcome variables
outcomenames <- c(
  "How_would_you_treat_case_6_Not_sure",
  "How_would_you_treat_case_6_Decompression_and_posterior_instrumentation",
  "How_would_you_treat_case_6_interbody_fusion",
  "How_would_you_treat_case_6_antibiotics",
  "How_would_you_treat_case_6_Conservative_treatment",
  "How_would_you_treat_case_6_Other",
  "How_would_you_treat_case_6_anterior_reconstruction",
  "How_would_you_treat_case_6_Posterior_decompression",
  "How_would_you_treat_case_6_NA"
)


# Prepare predictors
predictor_names <- setdiff(cleaned_column_names, outcomenames)
# Exclude columns starting with 'How_'
predictor_names <- predictor_names[!grepl("^How_", predictor_names)]

# Include only predictors where all unique values are 0 or 1
predictor_names <- predictor_names[sapply(predictor_names, function(x) {
  unique_vals <- unique(Spondylodiscitis_Variability_Excel_NEW[[x]])
  all(unique_vals %in% c(0, 1)) & !any(is.na(unique_vals))
})]

predictors <- Spondylodiscitis_Variability_Excel_NEW[, predictor_names]



# Initialize an empty list to store the results
results_list <- list()

# For each outcome...
for (outcome_name in outcomenames) {
  
  # For each predictor...
  for (predictor_name in predictor_names) {
    
    # Subset the relevant columns
    analysis_df <- Spondylodiscitis_Variability_Excel_NEW[, c(outcome_name, predictor_name)]
    
    # Drop any rows where both columns are NA
    analysis_df <- analysis_df[!is.na(analysis_df[outcome_name]) & !is.na(analysis_df[predictor_name]), ]
    
    # Ensure both variables are factors
    analysis_df <- lapply(analysis_df, as.factor)
    
    # Only proceed if both variables have at least two unique levels
    if (length(unique(analysis_df[[outcome_name]])) >= 2 & length(unique(analysis_df[[predictor_name]])) >= 2) {
      
      # Calculate the contingency table
      cont_table <- table(analysis_df[[outcome_name]], analysis_df[[predictor_name]])
      
      # Check the expected cell counts to decide which test to use
      # The expected frequencies can be calculated using the row and column totals
      expected <- outer(margin.table(cont_table, 1), margin.table(cont_table, 2), "*") / sum(cont_table)
      if (any(expected < 5)) {
        # If any cell in the expected contingency table has a count less than 5, use Fisher's exact test
        test_result <- fisher.test(cont_table)
        test_type <- "Fisher's exact"
      } else {
        # Otherwise, use Chi-squared test
        test_result <- chisq.test(cont_table)
        test_type <- "Chi-squared"
      }
      
      # Store the results in the results list
      results_list[[paste(outcome_name, predictor_name, sep = ":")]] <- data.frame(
        test_type = test_type,
        statistic = ifelse(!is.null(test_result$statistic), as.numeric(test_result$statistic), NA),
        df = ifelse(!is.null(test_result$parameter), as.numeric(test_result$parameter), NA),
        p_value = test_result$p.value
      )
    }
  }
}

# Convert results_list to a data frame
results_df <- do.call(rbind, results_list)
results_df$Outcome_Predictor <- rownames(results_df)
rownames(results_df) <- NULL

# Adjust p-values for multiple comparisons using the Bonferroni method
results_df <- results_df %>% 
  mutate(p_value_adj = p.adjust(p_value, method = "bonferroni"))

# Filter for significant results
significant_results <- results_df %>% 
  filter(p_value_adj < 0.05)


# Convert to a table suitable for publication using `kable` and `kableExtra`
significant_results_table <- significant_results %>%
  kable("html", caption = "Significant Results of Statistical Tests") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F)

# Print the table
print(significant_results_table)


library(writexl)

# Write the data frame to an Excel file
write_xlsx(significant_results, path = "significant_results6ABXNEWNEW.xlsx")








#Case 6ABx



# Load necessary libraries
library(readxl)
library(dplyr)
library(tidyr)
library(caret)
library(broom)

# Read the Excel file
Spondylodiscitis_Variability_Excel_NEW6ABx <- read_excel("OneDrive/Spondylodiscitis Variability/Spondylodiscitis_Variability_Excel_NEW6ABx.xlsx")

# Clean the column names
cleaned_column_names <- colnames(Spondylodiscitis_Variability_Excel_NEW6ABx) %>%
  gsub("\\W", "_", .) %>%
  gsub("_+", "_", .) %>%
  gsub("^_", "", .) %>%
  gsub("_$", "", .)

# Rename columns in the data frame
colnames(Spondylodiscitis_Variability_Excel_NEW6ABx) <- cleaned_column_names

# Define the names of the outcome variables
outcomenames <- c(
  "How_long_would_you_treat_with_antibiotics_presuming_a_good_response_in_case_6_Not_sure",
  "How_long_would_you_treat_with_antibiotics_presuming_a_good_response_in_case_6_Two_weeks_i_v_followed_by_10_weeks_p_o",
  "How_long_would_you_treat_with_antibiotics_presuming_a_good_response_in_case_6_Two_weeks_i_v_followed_by_4_weeks_p_o",
  "How_long_would_you_treat_with_antibiotics_presuming_a_good_response_in_case_6_Other",
  "How_long_would_you_treat_with_antibiotics_presuming_a_good_response_in_case_6_One_week_i_v_followed_by_11_weeks_p_o",
  "How_long_would_you_treat_with_antibiotics_presuming_a_good_response_in_case_6_One_week_i_v_followed_by_5_weeks_p_o",
  "How_long_would_you_treat_with_antibiotics_presuming_a_good_response_in_case_6_NA"
)

# Prepare predictors
predictor_names <- setdiff(cleaned_column_names, outcomenames)
# Exclude columns starting with 'How_'
predictor_names <- predictor_names[!grepl("^How_", predictor_names)]

# Include only predictors where all unique values are 0 or 1
predictor_names <- predictor_names[sapply(predictor_names, function(x) {
  unique_vals <- unique(Spondylodiscitis_Variability_Excel_NEW[[x]])
  all(unique_vals %in% c(0, 1)) & !any(is.na(unique_vals))
})]

predictors <- Spondylodiscitis_Variability_Excel_NEW[, predictor_names]



# Initialize an empty list to store the results
results_list <- list()

# For each outcome...
for (outcome_name in outcomenames) {
  
  # For each predictor...
  for (predictor_name in predictor_names) {
    
    # Subset the relevant columns
    analysis_df <- Spondylodiscitis_Variability_Excel_NEW[, c(outcome_name, predictor_name)]
    
    # Drop any rows where both columns are NA
    analysis_df <- analysis_df[!is.na(analysis_df[outcome_name]) & !is.na(analysis_df[predictor_name]), ]
    
    # Ensure both variables are factors
    analysis_df <- lapply(analysis_df, as.factor)
    
    # Only proceed if both variables have at least two unique levels
    if (length(unique(analysis_df[[outcome_name]])) >= 2 & length(unique(analysis_df[[predictor_name]])) >= 2) {
      
      # Calculate the contingency table
      cont_table <- table(analysis_df[[outcome_name]], analysis_df[[predictor_name]])
      
      # Check the expected cell counts to decide which test to use
      # The expected frequencies can be calculated using the row and column totals
      expected <- outer(margin.table(cont_table, 1), margin.table(cont_table, 2), "*") / sum(cont_table)
      if (any(expected < 5)) {
        # If any cell in the expected contingency table has a count less than 5, use Fisher's exact test
        test_result <- fisher.test(cont_table)
        test_type <- "Fisher's exact"
      } else {
        # Otherwise, use Chi-squared test
        test_result <- chisq.test(cont_table)
        test_type <- "Chi-squared"
      }
      
      # Store the results in the results list
      results_list[[paste(outcome_name, predictor_name, sep = ":")]] <- data.frame(
        test_type = test_type,
        statistic = ifelse(!is.null(test_result$statistic), as.numeric(test_result$statistic), NA),
        df = ifelse(!is.null(test_result$parameter), as.numeric(test_result$parameter), NA),
        p_value = test_result$p.value
      )
    }
  }
}

# Convert results_list to a data frame
results_df <- do.call(rbind, results_list)
results_df$Outcome_Predictor <- rownames(results_df)
rownames(results_df) <- NULL

# Adjust p-values for multiple comparisons using the Bonferroni method
results_df <- results_df %>% 
  mutate(p_value_adj = p.adjust(p_value, method = "bonferroni"))

# Filter for significant results
significant_results <- results_df %>% 
  filter(p_value_adj < 0.05)


# Convert to a table suitable for publication using `kable` and `kableExtra`
significant_results_table <- significant_results %>%
  kable("html", caption = "Significant Results of Statistical Tests") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F)

# Print the table
print(significant_results_table)


library(writexl)

# Write the data frame to an Excel file
write_xlsx(significant_results, path = "significant_results6ABXXXNEWNEW.xlsx")


























#Case 7



# Load necessary libraries
library(readxl)
library(dplyr)
library(tidyr)
library(caret)
library(broom)

# Read the Excel file
Spondylodiscitis_Variability_Excel_NEW7 <- read_excel("OneDrive/Spondylodiscitis Variability/Spondylodiscitis_Variability_Excel_NEW7.xlsx")

# Clean the column names
cleaned_column_names <- colnames(Spondylodiscitis_Variability_Excel_NEW7) %>%
  gsub("\\W", "_", .) %>%
  gsub("_+", "_", .) %>%
  gsub("^_", "", .) %>%
  gsub("_$", "", .)

# Rename columns in the data frame
colnames(Spondylodiscitis_Variability_Excel_NEW7) <- cleaned_column_names

# Define the names of the outcome variables
outcomenames <- c(
  "How_would_you_treat_case_7_Not_sure",
  "How_would_you_treat_case_7_Conservative_treatment",
  "How_would_you_treat_case_7_antibiotics",
  "How_would_you_treat_case_7_Decompression_and_posterior_instrumentation",
  "How_would_you_treat_case_7_anterior_reconstruction",
  "How_would_you_treat_case_7_Posterior_decompression",
  "How_would_you_treat_case_7_interbody_fusion",
  "How_would_you_treat_case_7_Other",
  "How_would_you_treat_case_7_NA"
)


# Prepare predictors
predictor_names <- setdiff(cleaned_column_names, outcomenames)
# Exclude columns starting with 'How_'
predictor_names <- predictor_names[!grepl("^How_", predictor_names)]

# Include only predictors where all unique values are 0 or 1
predictor_names <- predictor_names[sapply(predictor_names, function(x) {
  unique_vals <- unique(Spondylodiscitis_Variability_Excel_NEW[[x]])
  all(unique_vals %in% c(0, 1)) & !any(is.na(unique_vals))
})]

predictors <- Spondylodiscitis_Variability_Excel_NEW[, predictor_names]



# Initialize an empty list to store the results
results_list <- list()

# For each outcome...
for (outcome_name in outcomenames) {
  
  # For each predictor...
  for (predictor_name in predictor_names) {
    
    # Subset the relevant columns
    analysis_df <- Spondylodiscitis_Variability_Excel_NEW[, c(outcome_name, predictor_name)]
    
    # Drop any rows where both columns are NA
    analysis_df <- analysis_df[!is.na(analysis_df[outcome_name]) & !is.na(analysis_df[predictor_name]), ]
    
    # Ensure both variables are factors
    analysis_df <- lapply(analysis_df, as.factor)
    
    # Only proceed if both variables have at least two unique levels
    if (length(unique(analysis_df[[outcome_name]])) >= 2 & length(unique(analysis_df[[predictor_name]])) >= 2) {
      
      # Calculate the contingency table
      cont_table <- table(analysis_df[[outcome_name]], analysis_df[[predictor_name]])
      
      # Check the expected cell counts to decide which test to use
      # The expected frequencies can be calculated using the row and column totals
      expected <- outer(margin.table(cont_table, 1), margin.table(cont_table, 2), "*") / sum(cont_table)
      if (any(expected < 5)) {
        # If any cell in the expected contingency table has a count less than 5, use Fisher's exact test
        test_result <- fisher.test(cont_table)
        test_type <- "Fisher's exact"
      } else {
        # Otherwise, use Chi-squared test
        test_result <- chisq.test(cont_table)
        test_type <- "Chi-squared"
      }
      
      # Store the results in the results list
      results_list[[paste(outcome_name, predictor_name, sep = ":")]] <- data.frame(
        test_type = test_type,
        statistic = ifelse(!is.null(test_result$statistic), as.numeric(test_result$statistic), NA),
        df = ifelse(!is.null(test_result$parameter), as.numeric(test_result$parameter), NA),
        p_value = test_result$p.value
      )
    }
  }
}

# Convert results_list to a data frame
results_df <- do.call(rbind, results_list)
results_df$Outcome_Predictor <- rownames(results_df)
rownames(results_df) <- NULL

# Adjust p-values for multiple comparisons using the Bonferroni method
results_df <- results_df %>% 
  mutate(p_value_adj = p.adjust(p_value, method = "bonferroni"))

# Filter for significant results
significant_results <- results_df %>% 
  filter(p_value_adj < 0.05)


# Convert to a table suitable for publication using `kable` and `kableExtra`
significant_results_table <- significant_results %>%
  kable("html", caption = "Significant Results of Statistical Tests") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F)

# Print the table
print(significant_results_table)


library(writexl)

# Write the data frame to an Excel file
write_xlsx(significant_results, path = "significant_results7ABXXXNEWNEW.xlsx")









































# Load necessary libraries
library(dplyr)
library(ggplot2)
install.packages("GGally")
library(GGally)
library(psych)
library(corrplot)

# Assuming df is your DataFrame

# Load necessary libraries
library(readxl)
library(dplyr)
library(tidyr)
library(caret)

# Read the Excel file
Spondylodiscitis_Variability_Excel_NEW <- read_excel("OneDrive/Spondylodiscitis Variability/Spondylodiscitis_Variability_Excel_NEW.xlsx")

# Get all column names
all_column_names <- colnames(Spondylodiscitis_Variability_Excel_NEW)




# Replace non-alphanumeric characters with underscores
cleaned_column_names <- gsub("\\W", "_", all_column_names)

# Replace multiple underscores with a single underscore
cleaned_column_names <- gsub("_+", "_", cleaned_column_names)

# Remove leading underscores
cleaned_column_names <- gsub("^_", "", cleaned_column_names)

# Remove trailing underscores
cleaned_column_names <- gsub("_$", "", cleaned_column_names)




# Rename columns in the data frame
colnames(Spondylodiscitis_Variability_Excel_NEW) <- cleaned_column_names

# Print the cleaned column names
print(cleaned_column_names)


# Define the names of the outcome variables
outcomenames <- c("How_would_you_treat_case_1_Conservative_treatment",
                  "How_would_you_treat_case_1_antibiotics",
                  "How_would_you_treat_case_1_Decompression",
                  "How_would_you_treat_case_1_posterior_instrumentation",
                  "How_would_you_treat_case_1_interbody_fusion",
                  "How_would_you_treat_case_1_Posterior_decompression",
                  "How_would_you_treat_case_1_Other")                                            

# Get the names of all columns in the data frame
all_column_names <- colnames(Spondylodiscitis_Variability_Excel_NEW)

# Get the names of the predictor variables by excluding the outcome variables
predictor_names <- all_column_names[!all_column_names %in% outcomenames]




# Subset the data frame to only include the predictor variables
predictors <- Spondylodiscitis_Variability_Excel_NEW[, predictor_names]


# Calculate the correlation matrix for predictors
correlationMatrix <- cor(predictors)

# Find highly correlated variables (say, absolute correlation greater than 0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff = 0.75)

# Find column names that are highly correlated
highlyCorrelatedNames <- colnames(predictors)[highlyCorrelated]

# Exclude these highly correlated variables
predictors <- predictors[ , !(colnames(predictors) %in% highlyCorrelatedNames)]


# Select the first element of 'outcome' using indexing
Case1_conservative <- Spondylodiscitis_Variability_Excel_NEW$How_would_you_treat_case_1_Conservative_treatment

# Add outcome variable to predictors dataframe
predictors$Case1_conservative <- Case1_conservative
predictors

logmodel_case1 <- glm(Case1_conservative ~ ., data = predictors, family = binomial(link = 'logit'), control = list(maxit = 50))

# Print the logistic regression summary
summary(logmodel_case1)


# load the package
library(broom)

# tidy the model results
tidy_results <- broom::tidy(logmodel_case1)

# Filter the results for p < 0.05
significant_results <- tidy_results %>% 
  filter(p.value < 0.05)


# view the results
print(tidy_results)


# Print the significant results
print(significant_results)






library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(broom)
library(caret)
library(glmnet)

# Function to run logistic regression
run_logistic_regression <- function(file_path, outcome_name) {
  # Load data
  data <- read_excel(file_path)
  
  # Clean column names
  colnames(data) <- make.names(colnames(data))
  
  # Select predictor variables
  predictors <- data %>%
    select(-starts_with(outcome_name))
  
  # Calculate correlation matrix
  correlations <- cor(predictors, use = "pairwise.complete.obs")
  
  # Find highly correlated variables
  highly_correlated <- findCorrelation(correlations, cutoff = 0.75)
  
  # Exclude highly correlated variables
  predictors <- predictors[, -highly_correlated]
  
  # Add outcome variable to predictors dataframe
  predictors <- bind_cols(predictors, data %>% select(starts_with(outcome_name)))
  
  # Fit logistic regression model
  model <- glm(paste0(outcome_name, " ~ ."), data = predictors, family = "binomial")
  
  # Tidy results
  tidy_results <- tidy(model)
  
  # Filter significant results
  significant_results <- tidy_results %>%
    filter(p.value < 0.05)
  
  return(significant_results)
}

# Mapping of file paths to outcome variables
files_and_outcomes <- list(
  "OneDrive/Spondylodiscitis Variability/Spondylodiscitis_Variability_Excel_NEW.xlsx" = c(
    "How_would_you_treat_case_1_Conservative_treatment",
    "How_would_you_treat_case_1_antibiotics",
    "How_would_you_treat_case_1_Decompression",
    "How_would_you_treat_case_1_posterior_instrumentation",
    "How_would_you_treat_case_1_interbody_fusion",
    "How_would_you_treat_case_1_Posterior_decompression",
    "How_would_you_treat_case_1_Other"
  ))
  
# Iterate over all file paths and outcome variable names and apply the function
for (file_path in names(files_and_outcomes)) {
  for (outcome_name in files_and_outcomes[[file_path]]) {
    print(run_logistic_regression(file_path, outcome_name))
  }
}









































####################


# Get the names of the predictor variables by excluding the outcome variables
predictor_names <- colnames(Spondylodiscitis_Variability_Excel_NEW)[!colnames(Spondylodiscitis_Variability_Excel_NEW) %in% outcomenames]

# Assuming df is your DataFrame

# Load necessary libraries
library(readxl)
library(dplyr)
library(tidyr)
library(caret)
library(broom)
library(ggplot2)
library(MASS)
library(readxl)
library(dplyr)
library(tidyr)
library(purrr)



# Read the Excel file
Spondylodiscitis_Variability_Excel_NEW <- read_excel("OneDrive/Spondylodiscitis Variability/Spondylodiscitis_Variability_Excel_NEW.xlsx")

# Define the names of the outcome variables
outcomenames <- c("How_would_you_treat_case_1_Conservative_treatment",
                  "How_would_you_treat_case_1_antibiotics",
                  "How_would_you_treat_case_1_Decompression",
                  "How_would_you_treat_case_1_posterior_instrumentation",
                  "How_would_you_treat_case_1_interbody_fusion",
                  "How_would_you_treat_case_1_Posterior_decompression",
                  "How_would_you_treat_case_1_Other")                                            

# Get all column names
all_column_names <- colnames(Spondylodiscitis_Variability_Excel_NEW)

# Replace non-alphanumeric characters with underscores
cleaned_column_names <- gsub("\\W", "_", all_column_names)

# Replace multiple underscores with a single underscore
cleaned_column_names <- gsub("_+", "_", cleaned_column_names)

# Remove leading underscores
cleaned_column_names <- gsub("^_", "", cleaned_column_names)

# Remove trailing underscores
cleaned_column_names <- gsub("_$", "", cleaned_column_names)

# Rename columns in the data frame
colnames(Spondylodiscitis_Variability_Excel_NEW) <- cleaned_column_names

# Get the names of the predictor variables by excluding the outcome variables and columns that start with "How"
predictor_names <- colnames(Spondylodiscitis_Variability_Excel_NEW)[!colnames(Spondylodiscitis_Variability_Excel_NEW) %in% outcomenames & !grepl("^How", colnames(Spondylodiscitis_Variability_Excel_NEW))]

# Subset the data frame to only include the predictor variables
predictors <- Spondylodiscitis_Variability_Excel_NEW[, predictor_names]




# Iterate over each outcome variable
for (outcome in outcomenames) {
  
  # Generate the current predictor_names excluding the other "outcome" variables
  current_predictor_names <- setdiff(colnames(Spondylodiscitis_Variability_Excel_NEW), outcomenames)
  current_predictor_names <- current_predictor_names[!grepl("^How", current_predictor_names)]
  
  # Create a data frame that includes the current outcome and its predictors
  current_data <- Spondylodiscitis_Variability_Excel_NEW[c(outcome, current_predictor_names)]
  
  # Remove aliased variables
  aliased_vars <- alias(glm(as.formula(paste(outcome, "~ .")), data = current_data, family = binomial(link = 'logit'), control = list(maxit = 50)))$Complete[-1]
  current_data_no_aliased <- current_data[, !(colnames(current_data) %in% aliased_vars)]
  
  # Perform stepwise logistic regression using AIC as the criterion
  tryCatch({
    step_model <- step(glm(as.formula(paste(outcome, "~ .")), data = current_data_no_aliased, family = binomial(link = 'logit'), control = list(maxit = 50)), direction = "both", trace = FALSE)
    
    # Get the selected predictors
    selected_predictors <- names(step_model$coefficients)[-1]
    
    # Fit the final multivariate logistic regression model using the selected predictors
    if (length(selected_predictors) > 0) {
      multivariate_logmodel <- glm(as.formula(paste(outcome, "~", paste(selected_predictors, collapse = " + "))), data = current_data_no_aliased, family = binomial(link = 'logit'), control = list(maxit = 50))
      
      # Extract the coefficients, standard errors and p-values
      coef_table <- summary(multivariate_logmodel)$coefficients %>% as_tibble(rownames = "predictor") %>% select(predictor, Estimate = `Estimate`, `Std. Error` = `Std. Error`, `p-value` = `Pr(>|z|)`)
      
      # Add the outcome variable to the table
      coef_table <- coef_table %>% mutate(outcome = outcome) %>% select(outcome, everything())
      
      # Append the results to the tibble of results
      results <- bind_rows(results, coef_table)
    }
  }, error = function(e) {
    print(paste("Error:", e$message))
  })
}

# Print the tibble of results
print(results)



# Print the tibble of results ####DONE
results %>%
  rename("Outcome Variable" = outcome, 
         "Predictor Variable" = predictor, 
         "Estimate (Beta Coefficient)" = Estimate, 
         "Standard Error" = `Std. Error`, 
         "p-value" = `p-value`) %>%
  mutate("Estimate (Beta Coefficient)" = round(`Estimate (Beta Coefficient)`, 3),
         "Standard Error" = round(`Standard Error`, 3),
         "p-value" = round(`p-value`, 3)) %>%
  select("Outcome Variable", "Predictor Variable", "Estimate (Beta Coefficient)", "Standard Error", "p-value") %>%
  print(n = Inf) # Print all rows









# Required Libraries
library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(caret)
library(broom)
library(ggplot2)
library(MASS)

# Function to automate the process
run_pipeline <- function(excel_file_path, outcomenames) {
  # Read the Excel file
  data <- read_excel(excel_file_path)
  
  # Get all column names
  all_column_names <- colnames(data)
  
  # Replace non-alphanumeric characters with underscores
  cleaned_column_names <- gsub("\\W", "_", all_column_names)
  
  # Replace multiple underscores with a single underscore
  cleaned_column_names <- gsub("_+", "_", cleaned_column_names)
  
  # Remove leading underscores
  cleaned_column_names <- gsub("^_", "", cleaned_column_names)
  
  # Remove trailing underscores
  cleaned_column_names <- gsub("_$", "", cleaned_column_names)
  
  # Rename columns in the data frame
  colnames(data) <- cleaned_column_names
  
  # Get the names of the predictor variables by excluding the outcome variables
  predictor_names <- colnames(data)[!colnames(data) %in% outcomenames]
  
  # Create an empty tibble to store the results
  results <- tibble()
  
  # Iterate over each outcome variable
  for (outcome in outcomenames) {
    # Create predictors dataframe
    predictors <- data %>%
      select(all_of(c(outcome, predictor_names)))
    
    # Remove aliased variables
    aliased_vars <- alias(glm(as.formula(paste(outcome, "~ .")), data = predictors, family = binomial(link = 'logit'), control = list(maxit = 50)))$Complete[-1]
    predictors_no_aliased <- predictors[, !(colnames(predictors) %in% aliased_vars)]
    
    # Perform stepwise logistic regression using AIC as the criterion
    tryCatch({
      step_model <- step(glm(as.formula(paste(outcome, "~ .")), data = predictors_no_aliased, family = binomial(link = 'logit'), control = list(maxit = 50)), direction = "both", trace = FALSE)
      
      # Get the selected predictors
      selected_predictors <- names(step_model$coefficients)[-1]
      
      # Fit the final multivariate logistic regression model using the selected predictors
      if (length(selected_predictors) > 0) {
        multivariate_logmodel <- glm(as.formula(paste(outcome, "~", paste(selected_predictors, collapse = " + "))), data = predictors_no_aliased, family = binomial(link = 'logit'), control = list(maxit = 50))
        
        # Extract the coefficients, standard errors and p-values
        coef_table <- summary(multivariate_logmodel)$coefficients %>% as_tibble(rownames = "predictor") %>% select(predictor, Estimate = `Estimate`, `Std. Error` = `Std. Error`, `p-value` = `Pr(>|z|)`)
        
        # Add the outcome variable to the table
        coef_table <- coef_table %>% mutate(outcome = outcome) %>% select(outcome, everything())
        
        # Append the results to the tibble of results
        results <- bind_rows(results, coef_table)
      }
    }, error = function(e) {
      print(paste("Error:", e$message))
    })
  }
  
  # Print the tibble of results
  print(results)
  
  # Return the results as a dataframe
  return(results)
}

# Use the function
results <- run_pipeline("OneDrive/Spondylodiscitis Variability/Spondylodiscitis_Variability_Excel_NEW.xlsx", 
                        c("How_would_you_treat_case_1_Conservative_treatment",
                          "How_would_you_treat_case_1_antibiotics",
                          "How_would_you_treat_case_1_Decompression",
                          "How_would_you_treat_case_1_posterior_instrumentation",
                          "How_would_you_treat_case_1_interbody_fusion",
                          "How_would_you_treat_case_1_Posterior_decompression",
                          "How_would_you_treat_case_1_Other"))

# Print the tibble of results ####DONE
results %>%
  rename("Outcome Variable" = outcome, 
         "Predictor Variable" = predictor, 
         "Estimate (Beta Coefficient)" = Estimate, 
         "Standard Error" = `Std. Error`, 
         "p-value" = `p-value`) %>%
  mutate("Estimate (Beta Coefficient)" = round(`Estimate (Beta Coefficient)`, 3),
         "Standard Error" = round(`Standard Error`, 3),
         "p-value" = round(`p-value`, 3)) %>%
  select("Outcome Variable", "Predictor Variable", "Estimate (Beta Coefficient)", "Standard Error", "p-value") %>%
  print(n = Inf) # Print all rows









































#visualiseregressionresults
# install the package if you haven't already
install.packages("ggplot2")

# load the package
library(ggplot2)

# create a coefficient plot
ggplot(tidy_results, aes(x = estimate, y = reorder(term, estimate))) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point() +
  geom_errorbarh(aes(xmin = estimate - std.error * 1.96, 
                     xmax = estimate + std.error * 1.96),
                 height = .2) +
  labs(x = "Coefficient Estimate", y = "Variable") +
  theme_minimal()



#plot
# Histogram
hist(predictors$Years_in_practice[Case1_conservative==1], 
     col = 'red', 
     main = "Histogram of Years in Practice", 
     xlab = "Years in Practice",
     xlim = c(min(predictors$Years_in_practice), max(predictors$Years_in_practice)))
hist(predictors$Years_in_practice[Case1_conservative==0], 
     col = 'blue', 
     add = TRUE)
legend("topright", c("1", "0"), fill = c("red", "blue"))







#trial
# Assuming df is your DataFrame

# Load necessary libraries
library(readxl)
library(dplyr)
library(tidyr)
library(caret)
library(broom)
library(ggplot2)

# Read the Excel file
Spondylodiscitis_Variability_Excel_NEW <- read_excel("OneDrive/Spondylodiscitis Variability/Spondylodiscitis_Variability_Excel_NEW.xlsx")

# Get all column names
all_column_names <- colnames(Spondylodiscitis_Variability_Excel_NEW)

# Replace non-alphanumeric characters with underscores
cleaned_column_names <- gsub("\\W", "_", all_column_names)

# Replace multiple underscores with a single underscore
cleaned_column_names <- gsub("_+", "_", cleaned_column_names)

# Remove leading underscores
cleaned_column_names <- gsub("^_", "", cleaned_column_names)

# Remove trailing underscores
cleaned_column_names <- gsub("_$", "", cleaned_column_names)

# Rename columns in the data frame
colnames(Spondylodiscitis_Variability_Excel_NEW) <- cleaned_column_names

# Print the cleaned column names
print(cleaned_column_names)

# Define the names of the outcome variables
outcomenames <- c("How_would_you_treat_case_1_Conservative_treatment",
                  "How_would_you_treat_case_1_antibiotics",
                  "How_would_you_treat_case_1_Decompression",
                  "How_would_you_treat_case_1_posterior_instrumentation",
                  "How_would_you_treat_case_1_interbody_fusion",
                  "How_would_you_treat_case_1_Posterior_decompression",
                  "How_would_you_treat_case_1_Other")                                            

# Get the names of all columns in the data frame
all_column_names <- colnames(Spondylodiscitis_Variability_Excel_NEW)

# Get the names of the predictor variables by excluding the outcome variables
predictor_names <- all_column_names[!all_column_names %in% outcomenames]

# Subset the data frame to only include the predictor variables
predictors <- Spondylodiscitis_Variability_Excel_NEW[, predictor_names]

# Calculate the correlation matrix for predictors
correlationMatrix <- cor(predictors)

# Find highly correlated variables (say, absolute correlation greater than 0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff = 0.75)

# Find column names that are highly correlated
highlyCorrelatedNames <- colnames(predictors)[highlyCorrelated]

# Exclude these highly correlated variables
predictors <- predictors[ , !(colnames(predictors) %in% highlyCorrelatedNames)]

# Iterate over each outcome variable
for(outcome in outcomenames) {
  
  # Add outcome variable to predictors dataframe
  outcome_vector <- Spondylodiscitis_Variability_Excel_NEW[[outcome]]
  predictors[[outcome]] <- outcome_vector
  
  # Fit the logistic regression model
  logmodel <- glm(as.formula(paste(outcome, "~ .")), data = predictors, family = binomial(link = 'logit'), control = list(maxit = 50))
  
  # Print the logistic regression summary
  print(summary(logmodel))
  
  # tidy the model results
  tidy_results <- broom::tidy(logmodel)
  
  # Filter the results for p < 0.05
  significant_results <- tidy_results %>% filter(p.value < 0.05)
  
  # Print the significant results
  print(significant_results)
  
  # view the results
  print(tidy_results)
  
  # create a coefficient plot
  plot <- ggplot(tidy_results, aes(x = estimate, y = reorder(term, estimate))) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_point() +
    geom_errorbarh(aes(xmin = estimate - std.error * 1.96, 
                       xmax = estimate + std.error * 1.96),
                   height = .2) +
    labs(x = "Coefficient Estimate", y = "Variable") +
    theme_minimal()
  print(plot)
}





#experiment: univariate analysis

# Assuming df is your DataFrame

# Load necessary libraries
library(readxl)
library(dplyr)
library(tidyr)
library(caret)
library(broom)
library(ggplot2)

# Read the Excel file
Spondylodiscitis_Variability_Excel_NEW <- read_excel("OneDrive/Spondylodiscitis Variability/Spondylodiscitis_Variability_Excel_NEW.xlsx")

# Define the names of the outcome variables
outcomenames <- c("How_would_you_treat_case_1_Conservative_treatment",
                  "How_would_you_treat_case_1_antibiotics",
                  "How_would_you_treat_case_1_Decompression",
                  "How_would_you_treat_case_1_posterior_instrumentation",
                  "How_would_you_treat_case_1_interbody_fusion",
                  "How_would_you_treat_case_1_Posterior_decompression",
                  "How_would_you_treat_case_1_Other")                                            

# Get all column names
all_column_names <- colnames(Spondylodiscitis_Variability_Excel_NEW)

# Replace non-alphanumeric characters with underscores
cleaned_column_names <- gsub("\\W", "_", all_column_names)

# Replace multiple underscores with a single underscore
cleaned_column_names <- gsub("_+", "_", cleaned_column_names)

# Remove leading underscores
cleaned_column_names <- gsub("^_", "", cleaned_column_names)

# Remove trailing underscores
cleaned_column_names <- gsub("_$", "", cleaned_column_names)

# Rename columns in the data frame
colnames(Spondylodiscitis_Variability_Excel_NEW) <- cleaned_column_names

# Get the names of the predictor variables by excluding the outcome variables
predictor_names <- colnames(Spondylodiscitis_Variability_Excel_NEW)[!colnames(Spondylodiscitis_Variability_Excel_NEW) %in% outcomenames]

# Subset the data frame to only include the predictor variables
predictors <- Spondylodiscitis_Variability_Excel_NEW[, predictor_names]
# Create a list to store plots
plot_list <- list()

# Iterate over each outcome variable
for (outcome in outcomenames) {
  # Create a new list to store plots for each outcome
  outcome_plot_list <- list()
  
  # Add outcome variable to predictors dataframe
  predictors[[outcome]] <- Spondylodiscitis_Variability_Excel_NEW[[outcome]]
  
  # Iterate over each predictor variable
  for (predictor in predictor_names) {
    # Fit the logistic regression model
    logmodel <- glm(as.formula(paste(outcome, "~", predictor)), data = predictors, family = binomial(link = 'logit'), control = list(maxit = 50))
    
    # Get the coefficient and p-value
    coef <- summary(logmodel)$coefficients[2, 1]
    p_value <- summary(logmodel)$coefficients[2, 4]
    
    # Create scatter plot with regression line
    scatter_plot <- ggplot(predictors, aes_string(x = predictor, y = outcome)) +
      geom_point() +
      geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE, color = "red") +
      labs(title = paste(predictor, "\nCoefficient:", round(coef, 2), "p-value:", round(p_value, 2))) +
      theme_minimal()
    
    # Add scatter plot to the list
    outcome_plot_list[[predictor]] <- scatter_plot
  } }
  
  # Store the list of plots for the outcome variable
  plot_list[[outcome]] <- outcome_plot_list


  print(plot_list[[outcome]])
  
  
  
  
  
# Display the grid of scatter plots
plot_grid <- cowplot::plot_grid(plotlist = plot_list, nrow = 1)
print(plot_grid)
  



#only of p <0.05
# Iterate over each predictor variable
for (predictor in predictor_names) {
  # Fit the logistic regression model
  logmodel <- glm(as.formula(paste(outcome, "~", predictor)), data = predictors, family = binomial(link = 'logit'), control = list(maxit = 50))
  
  # Get the coefficient and p-value
  coef <- summary(logmodel)$coefficients[2, 1]
  p_value <- summary(logmodel)$coefficients[2, 4]
  
  # Check if the p-value is significant (p < 0.05)
  if (p_value < 0.05) {
    # Create scatter plot with regression line
    scatter_plot <- ggplot(predictors, aes_string(x = predictor, y = outcome)) +
      geom_point() +
      geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE, color = "red") +
      labs(title = paste(predictor, "\nCoefficient:", round(coef, 2), "p-value:", round(p_value, 2))) +
      theme_minimal()
    
    # Add scatter plot to the list
    outcome_plot_list[[predictor]] <- scatter_plot
  }
}

# Store the list of significant plots for the outcome variable
plot_list[[outcome]] <- outcome_plot_list

# Print the significant plots for the current outcome variable
for (plot in outcome_plot_list) {
  print(plot)
}

















  
#################################### THIS WORKS - UNIVARIATE ########################

library(ggplot2)
library(gridExtra)
library(ggplot2)
library(gridExtra)


library(ggplot2)
library(gridExtra)


library(ggplot2)
library(gridExtra)

library(ggplot2)
library(gridExtra)
library(cowplot)

library(ggplot2)
library(gridExtra)
library(cowplot)

library(ggplot2)
library(gridExtra)
library(cowplot)


library(ggplot2)
library(gridExtra)
library(extrafont)
library(cowplot)


library(ggplot2)





combined_plot <- do.call(grid.arrange, c(plot_list, ncol = 1))



####################### SCATTER PLOT

library(ggplot2)
library(gridExtra)
library(ggplot2)
library(gridExtra)

library(ggplot2)
library(gridExtra)


library(ggplot2)
library(gridExtra)



library(ggplot2)
library(gridExtra)
library(grid)
library(ggplot2)
library(gridExtra)
library(ggpubr)

# Create an empty list to store the plots for each outcome variable
plot_list <- list()

# Define the maximum number of scatter plots per grid
max_plots_per_grid <- 9

# Stepwise regression leading to multivariate logistic regression
# Iterate over each outcome variable
for (outcome in outcomenames) {
  # Create a new list to store plots for each outcome
  outcome_plot_list <- list()
  
  # Add outcome variable to predictors dataframe
  predictors[[outcome]] <- Spondylodiscitis_Variability_Excel_NEW[[outcome]]
  
  # Iterate over each predictor variable
  for (predictor in predictor_names) {
    # Fit the logistic regression model
    logmodel <- glm(as.formula(paste(outcome, "~", predictor)), data = predictors, family = binomial(link = 'logit'), control = list(maxit = 50))
    
    # Get the coefficient and p-value
    coef <- summary(logmodel)$coefficients[2, 1]
    p_value <- summary(logmodel)$coefficients[2, 4]
    
    # Check if the p-value is significant (p < 0.05)
    if (p_value < 0.05) {
      # Create scatter plot with regression line
      scatter_plot <- ggplot(predictors, aes_string(x = predictor, y = outcome)) +
        geom_point(color = "black", size = 1.5) +
        geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE, color = "red", linetype = "solid") +
        labs(title = paste(predictor, "\nCoefficient:", round(coef, 2), "p-value:", round(p_value, 2)),
             x = predictor,
             y = outcome) +
        theme(text = element_text(family = "Times New Roman", size = 7),
              plot.title = element_text(size = 7, face = "bold"),
              axis.title = element_text(size = 6, face = "bold"),
              axis.text = element_text(size = 5),
              legend.position = "bottom",
              panel.grid = element_blank(),
              panel.border = element_blank())
      
      # Add scatter plot to the list
      outcome_plot_list[[predictor]] <- scatter_plot
    }
  }
  
  # Store the list of significant plots for the outcome variable
  plot_list[[outcome]] <- outcome_plot_list
  
  # Check if there are significant scatter plots for the current outcome variable
  if (length(outcome_plot_list) > 0) {
    # Calculate the number of rows and columns for the grid based on the number of scatter plots
    n_plots <- length(outcome_plot_list)
    n_cols <- min(ceiling(sqrt(n_plots)), max_plots_per_grid)
    n_rows <- ceiling(n_plots / n_cols)
    
    # Arrange the significant scatter plots in a grid format
    grid_plot <- grid.arrange(grobs = outcome_plot_list, ncol = n_cols, nrow = n_rows, top = outcome)
    
    # Generate a unique filename for the plot
    plot_filename <- paste0("plot_", outcome, ".png")
    
    # Save the grid plot as a PNG file
    ggsave(filename = plot_filename, plot = grid_plot, width = 11, height = 11)
    
    # Add the filename to the list
    plot_list[[outcome]] <- plot_filename
    
    # Print the grid of scatter plots for the current outcome variable
    print(grid_plot)
  }
}



library(ggplot2)
library(gridExtra)
library(grid)

# Set the output directory
output_dir <- "OneDrive/Spondylodiscitis Variability/"



# Create an empty list to store the filenames of the saved grids
grid_filenames <- list()

# Define the maximum number of scatter plots per grid
max_plots_per_grid <- 9

# Stepwise regression leading to multivariate logistic regression
for (outcome in outcomenames) {
  outcome_plot_list <- list()
  predictors[[outcome]] <- Spondylodiscitis_Variability_Excel_NEW[[outcome]]
  
  for (predictor in predictor_names) {
    logmodel <- glm(as.formula(paste(outcome, "~", predictor)), data = predictors, family = binomial(link = 'logit'), control = list(maxit = 50))
    coef <- summary(logmodel)$coefficients[2, 1]
    p_value <- summary(logmodel)$coefficients[2, 4]
    
    if (p_value < 0.05) {
      scatter_plot <- ggplot(predictors, aes_string(x = predictor, y = outcome)) +
        geom_point(color = "black", size = 1.5) +
        geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE, color = "red", linetype = "solid") +
        labs(title = paste(predictor, "\nCoefficient:", round(coef, 2), "p-value:", round(p_value, 2)),
             x = predictor,  # Add X-axis label
             y = outcome)  # Add Y-axis label
      scatter_plot <- scatter_plot + theme(text = element_text(family = "Times New Roman", size = 10),
                                           plot.title = element_text(size = 12, face = "bold"),
                                           legend.position = "bottom",
                                           panel.grid = element_blank(),
                                           panel.border = element_blank())
      
      # Add scatter plot to the list
      outcome_plot_list[[predictor]] <- scatter_plot
    }
  }
  
  # Store the list of significant plots for the outcome variable
  plot_list[[outcome]] <- outcome_plot_list
  
  # Check if there are significant scatter plots for the current outcome variable
  if (length(outcome_plot_list) > 0) {
    # Calculate the number of rows and columns for the grid based on the number of scatter plots
    n_plots <- length(outcome_plot_list)
    n_cols <- min(ceiling(sqrt(n_plots)), max_plots_per_grid)
    n_rows <- ceiling(n_plots / n_cols)
    
    # Arrange the significant scatter plots in a grid format
    grid_plot <- grid.arrange(grobs = outcome_plot_list, ncol = n_cols, nrow = n_rows, top = outcome)
    
    # Generate a unique filename for the grid plot
    grid_filename <- paste0("grid_", outcome, ".png")
    
    # Save the grid plot as a PNG file
    ggsave(filename = file.path(output_dir, grid_filename), plot = grid_plot, width = 12, height = 12)
    
    # Store the filename in the list
    grid_filenames[[length(grid_filenames) + 1]] <- file.path(output_dir, grid_filename)
  }
}

# Print the filenames of the saved grid plots
print(grid_filenames)





library(ggplot2)
library(gridExtra)
library(grid)
library(ggplot2)
library(gridExtra)

# Set the output directory
output_dir <- "OneDrive/Spondylodiscitis Variability/"

# Create an empty list to store the filenames of the saved grids
grid_filenames <- list()

# Define the maximum number of scatter plots per grid
max_plots_per_grid <- 9

# Stepwise regression leading to multivariate logistic regression
for (outcome in outcomenames) {
  outcome_plot_list <- list()
  predictors[[outcome]] <- Spondylodiscitis_Variability_Excel_NEW[[outcome]]
  
  for (predictor in predictor_names) {
    logmodel <- glm(as.formula(paste(outcome, "~", predictor)), data = predictors, family = binomial(link = 'logit'), control = list(maxit = 50))
    coef <- summary(logmodel)$coefficients[2, 1]
    p_value <- summary(logmodel)$coefficients[2, 4]
    
    if (p_value < 0.05) {
      scatter_plot <- ggplot(predictors, aes_string(x = predictor, y = outcome)) +
        geom_point(color = "black", size = 1.5) +
        geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE, color = "red", linetype = "solid") +
        labs(title = paste(predictor, "\nCoefficient:", round(coef, 2), "p-value:", round(p_value, 2)),
             x = predictor,
             y = "") +  # Remove Y-axis label
        theme(text = element_text(family = "Times New Roman", size = 7),
              plot.title = element_text(size = 7, face = "bold"),
              axis.title = element_text(size = 6, face = "bold"),
              axis.text = element_text(size = 5),
              legend.position = "bottom",
              panel.grid = element_blank(),
              panel.border = element_blank())
      
      # Add scatter plot to the list
      outcome_plot_list[[predictor]] <- scatter_plot
    }
  }
  
  # Store the list of significant plots for the outcome variable
  plot_list[[outcome]] <- outcome_plot_list
  
  # Check if there are significant scatter plots for the current outcome variable
  if (length(outcome_plot_list) > 0) {
    # Calculate the number of rows and columns for the grid based on the number of scatter plots
    n_plots <- length(outcome_plot_list)
    n_cols <- min(ceiling(sqrt(n_plots)), max_plots_per_grid)
    n_rows <- ceiling(n_plots / n_cols)
    
    # Arrange the significant scatter plots in a grid format
    grid_plot <- grid.arrange(grobs = outcome_plot_list, ncol = n_cols, nrow = n_rows, top = outcome)
    
    # Generate a unique filename for the grid plot
    grid_filename <- paste0("grid_", outcome, ".png")
    
    # Save the grid plot as a PNG file
    ggsave(filename = file.path(output_dir, grid_filename), plot = grid_plot, width = 8, height = 8)
    
    # Store the filename in the list
    grid_filenames[[length(grid_filenames) + 1]] <- file.path(output_dir, grid_filename)
  }
}

# Print the filenames of the saved grid plots
print(grid_filenames)



#




































  


###############' CORRELATION
###############' 
###############' 
###############' 


# Subset the data frame to only include the predictor variables
predictors <- Spondylodiscitis_Variability_Excel_NEW[, predictor_names]

#####  CONSIDER DELETING IF ISSUE
outcomes <- Spondylodiscitis_Variability_Excel_NEW[, outcomenames]



# Calculate correlation matrix for predictors
cor_matrix <- cor(predictors, use = "complete.obs")

# Calculate the correlation matrix
cor_matrix <- cor(predictors, use = "complete.obs")

# Function to calculate p-values for correlation coefficients
cor_pvalue <- function(x, n) {
  df <- n - 2
  t_values <- x / sqrt((1 - x^2) / df)
  p_values <- 2 * pt(abs(t_values), df = df, lower.tail = FALSE)
  p_values
}

# Calculate the p-values for the correlation matrix
p_values <- cor_pvalue(cor_matrix, nrow(predictors))





library(ggplot2)
library(gridExtra)

# Calculate correlation matrix for predictors
cor_matrix <- cor(predictors, use = "complete.obs")

# Get the row and column indices of significant correlations
significant_indices <- which(abs(cor_matrix) >= 0.5 & upper.tri(cor_matrix), arr.ind = TRUE)

# Iterate over each significant correlation
for (i in 1:nrow(significant_indices)) {
  row <- significant_indices[i, "row"]
  col <- significant_indices[i, "col"]
  
  # Calculate p-value for the significant correlation
  p_value <- cor.test(predictors[[col]], predictors[[row]], method = "pearson")$p.value
  
  # Create scatter plot for the significant correlation
  scatter_plot <- ggplot(data = predictors, aes_string(x = colnames(predictors)[col], y = colnames(predictors)[row])) +
    geom_point() +
    labs(
      title = paste("Correlation:", round(cor_matrix[row, col], 2)),
      subtitle = paste("p-value:", round(p_value, 2))
    ) +
    theme_minimal()
  
  # Print the scatter plot
  print(scatter_plot)
  
  # Prompt the user to continue or exit
  if (i < nrow(significant_indices)) {
    cat("Press Enter to continue to the next plot. Press any other key followed by Enter to exit.")
    response <- readline()
    if (nzchar(response) && tolower(response) != "enter") {
      break
    }
  }
}


# Install and load the 'polycor' package
# Install and load the 'GGally' package
install.packages("GGally")
library(GGally)

# Install and load the 'GGally' package
install.packages("GGally")
library(GGally)
# Install and load the required packages
install.packages("GGally")
library(GGally)
library(ggplot2)

# Install and load the required packages
install.packages("GGally")
library(GGally)
library(ggplot2)

# Install and load the required packages
install.packages("GGally")
install.packages("gplots")
library(GGally)
library(gplots)

library(ggplot2)


library(ggplot2)

# Calculate correlation matrix and p-values for remaining predictors using the Pearson method
cor_matrix <- cor(predictors, use = "complete.obs")
p_values <- matrix(NA, nrow = ncol(predictors), ncol = ncol(predictors))
for (i in 1:ncol(predictors)) {
  for (j in 1:ncol(predictors)) {
    if (i != j) {
      cor_test <- cor.test(predictors[, i], predictors[, j], method = "pearson")
      p_values[i, j] <- cor_test$p.value
    }
  }
}

# Filter for significant correlations (>= 0.7 or <= -0.7) and p-value < 0.05
filtered_cor <- cor_matrix
filtered_cor[!( p_values < 0.05)] <- NA

# Create a data frame with row and column indices
cor_df <- as.data.frame.matrix(filtered_cor)
cor_df$row <- rownames(cor_df)
cor_df <- cor_df[order(cor_df$row), ]
rownames(cor_df) <- NULL

# Reshape the data frame into long format
cor_long <- reshape2::melt(cor_df, id.vars = "row", variable.name = "column", value.name = "correlation")

# Create the heatmap using ggplot2
heatmap_plot <- ggplot(cor_long, aes(x = column, y = row, fill = correlation)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "#1f78b4", mid = "white", high = "#e31a1c", midpoint = 0,
                       limits = c(-1, 1), na.value = "transparent") +
  labs(title = "Significant Correlations Heatmap",
       x = "Predictors",
       y = "Predictors",
       fill = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Adjust the plot dimensions
ggsave("heatmap.png", heatmap_plot, width = 6, height = 6, dpi = 300)

# Print the heatmap plot
print(heatmap_plot)






# Define a theme for nature style
theme_nature <- theme_minimal() +
  theme(
    text = element_text(family = "Helvetica", color = "black"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(2, 2, 2, 2, "cm")
  )

# Create the heatmap using ggplot2
heatmap_plot <- ggplot(cor_long, aes(x = column, y = row, fill = correlation)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "#4575b4", mid = "white", high = "#d73027", midpoint = 0,
                       limits = c(-1, 1), na.value = NA, oob = squish, guide = "colourbar") +
  labs(
    title = "Significant Correlations Heatmap",
    x = "Predictors",
    y = "Predictors",
    fill = "Correlation"
  ) +
  theme_nature +
  theme(plot.title = element_text(size = 14, face = "bold"),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = 0.5))

# Adjust the plot dimensions and save
ggsave("heatmap.png", heatmap_plot, width = 6, height = 6, dpi = 300)

# Print the heatmap plot
print(heatmap_plot)







#Mean computation

# Install and load required packages
install.packages(c("mice", "glmnet", "MASS", "ggplot2", "caret"))
library(mice)
library(glmnet)
library(MASS)
library(ggplot2)
library(caret)


# Filter the data and create dummy variables
filtered_data <- Spondylodiscitis_Variability_Excel_NEW %>%
  select(all_of(predictors), all_of(outcome))

# Perform mean imputation
completed_data <- filtered_data
for (col in colnames(filtered_data)) {
  completed_data[is.na(completed_data[, col]), col] <- mean(filtered_data[, col], na.rm = TRUE)
}

# Convert the outcome variable to a factor
completed_data[, outcome] <- lapply(completed_data[, outcome], as.factor)

# Define the formula
formula <- as.formula(paste(outcome[1], "~", paste(predictors, collapse = "+")))

# Perform logistic regression on the completed dataset
logistic_model <- glm(formula, data = completed_data, family = binomial)

# Print the logistic regression summary
summary(logistic_model)















###REGRESSION



install.packages("glmnet")
install.packages("MASS")

install.packages("glmnet")
install.packages("MASS")

install.packages("glmnet")
install.packages("MASS")

library(glmnet)
library(MASS)
library(ggplot2)

# Filter the data and create dummy variables
filtered_data <- Spondylodiscitis_Variability_Excel_NEW %>%
  select(all_of(predictors), all_of(outcome))

# Perform mode imputation for categorical variables
imputed_data <- filtered_data
for (col in colnames(filtered_data)) {
  if (is.factor(filtered_data[, col])) {
    mode_val <- as.character(stats::getMode(filtered_data[, col]))
    imputed_data[is.na(imputed_data[, col]), col] <- mode_val
  }
}

# Convert the outcome variable to a factor
imputed_data[, outcome] <- lapply(imputed_data[, outcome], as.factor)

# Define the formula
formula <- as.formula(paste(outcome[1], "~", paste(predictors, collapse = "+")))

# Perform logistic regression on the imputed dataset
logistic_model <- glm(formula, data = imputed_data, family = binomial)

# Convert the data to a model matrix format
X <- model.matrix(formula, data = imputed_data)[,-1]  # Exclude intercept
y <- imputed_data[, outcome[1]]  # The outcome variable

# Standardize the data for Lasso, Ridge, and Elastic Net
X_standard <- scale(X)
y_standard <- scale(y)

# Convert X_standard to a data frame
X_standard_df <- as.data.frame(X_standard)

# Impute missing values in the predictor matrix
X_standard_imputed <- makeX(X_standard_df)

# Lasso regression
set.seed(123)
lasso_model <- glmnet(X_standard_imputed, y_standard, family = "binomial", alpha = 1)
lasso_coefs <- coef(lasso_model, s = lasso_model$lambda.1se)  # Extract coefficients at 1 standard error rule
lasso_coefs <- data.frame(Variable = rownames(lasso_coefs), Coefficient = lasso_coefs)

# Ridge regression
set.seed(123)
ridge_model <- glmnet(X_standard, y_standard, family = "binomial", alpha = 0)
ridge_coefs <- coef(ridge_model, s = ridge_model$lambda.1se)  # Extract coefficients at 1 standard error rule
ridge_coefs <- data.frame(Variable = rownames(ridge_coefs), Coefficient = ridge_coefs)

# Elastic Net regression
set.seed(123)
elastic_net_model <- cv.glmnet(X_standard, y_standard, family = "binomial", alpha = 0.5)
elastic_net_coefs <- coef(elastic_net_model, s = elastic_net_model$lambda.1se)  # Extract coefficients at 1 standard error rule
elastic_net_coefs <- data.frame(Variable = rownames(elastic_net_coefs), Coefficient = elastic_net_coefs)

# Coefficient plots
ggplot(lasso_coefs, aes(Variable, Coefficient)) + 
  geom_bar(stat = "identity") + 
  coord_flip() +
  labs(title = "Lasso Regression Coefficients")

ggplot(ridge_coefs, aes(Variable, Coefficient)) + 
  geom_bar(stat = "identity") + 
  coord_flip() +
  labs(title = "Ridge Regression Coefficients")

ggplot(elastic_net_coefs, aes(Variable, Coefficient)) + 
  geom_bar(stat = "identity") + 
  coord_flip() +
  labs(title = "Elastic Net Regression Coefficients")






# Now you have a dataframe with one row per term in each model, and it includes
# the name of the outcome variable that was used to fit each model



# Pairwise analysis ---> significant negative relationships:

# between "Outcome: `How would you treat case 1?: Conservative treatment`"
#"Predictors: `Proportion of spinal infection cases referred to their service that are treated surgically` `Follow-up period (months) for discitis`)#
#estim…¹ std.e…² stati…³ p.value -2.59    1.28   -2.02  0.0430
#!!!!!!!!!!!! BELOW

#[1] "Outcome: `How would you treat case 1?: Posterior decompression`"
#[1] "Predictors: `Special interest in spine surgery` `Proportion of spinal infection cases referred to their service that are treated surgically`"
# A tibble: 1 × 5
#term        estimate std.error statistic  p.value
#<chr>          <dbl>     <dbl>     <dbl>    <dbl>
#  1 (Intercept)    -4.34      1.24     -3.50 0.000462

#[1] "Outcome: `How would you treat case 1?: Other`"
#[1] "Predictors: `Special interest in spine surgery` `Proportion of spinal infection cases referred to their service that are treated surgically`"
# A tibble: 1 × 5
#term        estimate std.error statistic p.value
#<chr>          <dbl>     <dbl>     <dbl>   <dbl>
#  1 (Intercept)    -4.22      1.34     -3.16 0.00158


###[1] "Outcome: `How would you treat case 1?: Posterior decompression`"
# "Predictors: `Number of spinal cases as responsible surgeon per year` `Proportion of spinal infection cases referred to their service that are treated surgically`"
# A tibble: 1 × 5 ?????????????????
#term        estimate std.error statistic  p.value
#chr>          <dbl>     <dbl>     <dbl>    <dbl>
  #1 (Intercept)    -3.16     0.932     -3.39 0.000692

#[1] "Outcome: `How would you treat case 1?: Conservative treatment`"
#[1] "Predictors: `Number of spinal cases as responsible surgeon per year` `Proportion of spinal infection cases referred to their service that are treated surgically`"
# A tibble: 4 × 5
#termm (Intercept)                                                    3.76   0.891    4.22 2.41e-5
#2 `Proportion of spinal infection cases referred to their se…   -1.78   0.753   -2.36 1.81e-2
#3 `Proportion of spinal infection cases referred to their se…   -3.88   1.00    -3.88 1.05e-4
#4 `Proportion of spinal infection cases referred to their se…   -4.85   1.05    -4.60 4.13e-6

for (i in 1:(length(predictors) - 1)) {
  for (j in (i + 1):length(predictors)) {
    for (outcome in outcomes) {
      print(paste("Outcome:", outcome))
      print(paste("Predictors:", predictors[i], predictors[j]))
      
      formula <- as.formula(paste(outcome, "~", predictors[i], "+", predictors[j]))
      model <- glm(formula, data = data, family = binomial)
      summary_df <- tidy(model)
      significant_vars <- filter(summary_df, p.value < 0.05)
      
      print(significant_vars)
    }
  }
}

# Interaction effects
for (i in 1:(length(predictors) - 1)) {
  for (j in (i + 1):length(predictors)) {
    for (outcome in outcomes) {
      print(paste("Outcome:", outcome))
      print(paste("Interaction predictors:", predictors[i], predictors[j]))
      
      formula <- as.formula(paste(outcome, "~", predictors[i], "*", predictors[j]))
      model <- glm(formula, data = data, family = binomial)
      summary_df <- tidy(model)
      significant_vars <- filter(summary_df, p.value < 0.05)
      
      print(significant_vars)
    }
  }
}



###################################################################



#Regression analysis for Case 1 

# Load necessary libraries
library(readxl)
library(dplyr)
library(tidyr)
library(caret)
library(broom)
Spondylodiscitis_Variability_Excel_NEW <- read_excel("OneDrive/Spondylodiscitis Variability/Spondylodiscitis_Variability_Excel_NEW.xlsx")


# Define the names of the outcome variables
outcomenames <- c("How_would_you_treat_case_1_Conservative_treatment",
                  "How_would_you_treat_case_1_antibiotics",
                  "How_would_you_treat_case_1_Decompression",
                  "How_would_you_treat_case_1_posterior_instrumentation",
                  "How_would_you_treat_case_1_interbody_fusion",
                  "How_would_you_treat_case_1_Posterior_decompression",
                  "How_would_you_treat_case_1_Other")                                            

# Get all column names
all_column_names <- colnames(Spondylodiscitis_Variability_Excel_NEW)




# Replace non-alphanumeric characters with underscores
cleaned_column_names <- gsub("\\W", "_", all_column_names)

# Replace multiple underscores with a single underscore
cleaned_column_names <- gsub("_+", "_", cleaned_column_names)

# Remove leading underscores
cleaned_column_names <- gsub("^_", "", cleaned_column_names)

# Remove trailing underscores
cleaned_column_names <- gsub("_$", "", cleaned_column_names)


# Rename columns in the data frame
colnames(Spondylodiscitis_Variability_Excel_NEW) <- cleaned_column_names

# Define three sets of predictor variables
predictors_1 <- c("Country_Sweden", "Country_Ireland", "Country_Spain", "Country_UK", "Country_Belgium", "Country_Italy", "Country_Netherlands", "Country_India", "Country_Germany", "Country_Mexico", "Country_Romania", "Country_Australia", "Country_Portugal", "Country_Switzerland", "Country_NA", "Country_Greece", "Country_Russia", "Country_Serbia", "Country_Jordan", "Country_Turkey", "Country_Austria", "Country_France", "Country_Qatar", "Country_Bulgaria", "Country_Ukraine", "Country_Croatia", "Country_Finland", "Country_Rumania", "Country_Pakistan", "Country_Nigeria", "Country_Czech_Republic", "Country_Denmark", "Country_Saudi_Arabia", "Country_Saudi_arabia", "Country_Nepal", "Country_Hungary", "Country_Japan", "Country_Uzbekistan")
predictors_2 <- c("Role_Neurosurgeon_in_training", "Role_Board_certified_neurosurgeon", "Number_of_spinal_cases_as_responsible_surgeon_per_year_<50", "Number_of_spinal_cases_as_responsible_surgeon_per_year_>300", "Number_of_spinal_cases_as_responsible_surgeon_per_year_101-200", "Number_of_spinal_cases_as_responsible_surgeon_per_year_50-100", "Number_of_spinal_cases_as_responsible_surgeon_per_year_201-300", "Special_interest_in_spine_surgery_No", "Special_interest_in_spine_surgery_Yes", "Special_interest_in_spine_surgery_NA")
predictors_3 <- c("Years_in_practice", "Routine_neurosurgery_follow-up_for_discitis", "Follow-up_period_(months)_for_discitis")

predictors_all <- list(predictors_1, predictors_2, predictors_3)

# Create a list to store plots
plot_list <- list()

# Load necessary libraries
library(broom)

# Create an empty dataframe to store results
regression_results <- data.frame()

# Loop through all predictor sets
for (j in 1:length(predictors_all)) {
  predictors <- predictors_all[[j]]
  
  # Loop through each outcome variable
  for (outcome in outcomenames) {
    
    # Loop through each predictor variable
    for (predictor in predictors) {
      # Check if the predictor has more than one level
      if (length(unique(Spondylodiscitis_Variability_Excel_NEW[[predictor]])) > 1) {
        # Fit the logistic regression model
        formula <- as.formula(paste(outcome, "~", predictor))
        logmodel <- glm(formula, data = Spondylodiscitis_Variability_Excel_NEW, family = binomial(link = 'logit'), control = list(maxit = 50))
        
        # Get the tidy output and exclude the intercept
        tidy_output <- tidy(logmodel)
        tidy_output <- tidy_output[tidy_output$term != "(Intercept)",]
        
        # Add additional information
        tidy_output$outcome <- outcome
        tidy_output$predictor <- predictor
        tidy_output$predictor_set <- j
        tidy_output$formula <- deparse(formula)  # Add formula to the output
        
        # Bind to the results dataframe
        regression_results <- rbind(regression_results, tidy_output)
      }
    }
  }
}


# ...

# Filter the results for p-values less than 0.05
significant_results <- regression_results[regression_results$p.value < 0.05,]

# Print the significant results
print(significant_results)

# Count the number of significant predictors
significant_predictor_count <- nrow(significant_results)

# Print the count
print(paste("Number of significant predictors: ", significant_predictor_count))




###############grid for Case 1


# Define the plot list
plot_list <- list()

# Define the plot list
plot_list <- list()

# Loop through all predictor sets
for (j in 1:length(predictors_all)) {
  predictors <- predictors_all[[j]]
  
  # Loop through each outcome variable
  for (outcome in outcomenames) {
    
    # Loop through each predictor variable
    for (predictor in predictors) {
      # Check if the predictor has more than one level
      if (length(unique(Spondylodiscitis_Variability_Excel_NEW[[predictor]])) > 1) {
        # Fit the logistic regression model
        logmodel <- glm(as.formula(paste(outcome, "~", predictor)), data = Spondylodiscitis_Variability_Excel_NEW, family = binomial(link = 'logit'), control = list(maxit = 50))
        
        # Get the tidy output and exclude the intercept
        tidy_output <- tidy(logmodel)
        tidy_output <- tidy_output[tidy_output$term != "(Intercept)",]
        
        # Check if the p-value is less than 0.05
        if (tidy_output$p.value[1] < 0.05) {
          # If predictor is binary
          if (length(unique(Spondylodiscitis_Variability_Excel_NEW[[predictor]])) == 2) {
            # Create data frame for geom_hline
            coef_df <- data.frame(predictor_level = c(0, 1),
                                  predicted_prob = exp(coef(logmodel)["(Intercept)"] + coef(logmodel)[predictor]*c(0, 1)) / 
                                    (1 + exp(coef(logmodel)["(Intercept)"] + coef(logmodel)[predictor]*c(0, 1))))
            
            p <- ggplot(Spondylodiscitis_Variability_Excel_NEW, aes_string(x = predictor, y = outcome)) +
              geom_jitter(width = 0.2, height = 0.1, alpha = 0.5) +
              geom_hline(data = coef_df, aes(yintercept = predicted_prob, color = as.factor(predictor_level))) +
              scale_color_manual(name = "Predictor Level", values = c("0" = "Blue", "1" = "Red")) +
              labs(x = predictor, y = outcome) + theme(axis.title.y = element_text(size = 6))
            
            plot_list[[length(plot_list) + 1]] <- p
          } else {  # Predictor is continuous
            p <- ggplot(Spondylodiscitis_Variability_Excel_NEW, aes_string(x = predictor, y = outcome)) +
              geom_point(alpha = 0.5) +
              geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE, color = "red") +
              labs(x = predictor, y = outcome) + theme(axis.title.y = element_text(size = 6))
            
            plot_list[[length(plot_list) + 1]] <- p
          }
        }
      }
    }
  }
}



# Install and load the gridExtra library
if (!requireNamespace("gridExtra", quietly = TRUE)) {
  install.packages("gridExtra")
}
library(gridExtra)

# Now we are going to create chunks of the plot_list
chunk_size <- 7 # Maximum 6 plots per graph
plot_chunks <- split(plot_list, ceiling(seq_along(plot_list)/chunk_size))

# Now for each chunk we create a separate plot
for(i in seq_along(plot_chunks)){
  current_plots <- plot_chunks[[i]]
  # Specify the number of rows and columns in the grid
  nrow <- 3
  ncol <- 3
  
  
  # Arrange the plots in the grid
  grid_plot <- grid.arrange(grobs = current_plots, nrow = nrow, ncol = ncol)
  
  # Save the grid plot to a file
}



###################################################################



#Regression analysis for Case 2

# Load necessary libraries
library(readxl)
library(dplyr)
library(tidyr)
library(caret)
library(broom)
Spondylodiscitis_Variability_Excel_NEW <- read_excel("OneDrive/Spondylodiscitis Variability/Spondylodiscitis_Variability_Excel_NEW.xlsx")


# Define the names of the outcome variables
# Define the names of the outcome variables
# Define the names of the outcome variables
outcomenames <- c(
  "How_would_you_treat_case_1_Conservative_treatment",
  "How_would_you_treat_case_1_antibiotics",
  "How_would_you_treat_case_1_Decompression",
  "How_would_you_treat_case_1_posterior_instrumentation",
  "How_would_you_treat_case_1_interbody_fusion",
  "How_would_you_treat_case_1_Posterior_decompression",
  "How_would_you_treat_case_1_Other",
  "How_would_you_monitor_treatment_response_of_case_1_more_than_one_option_possible_Repeated_CRP_measurement",
  "How_would_you_monitor_treatment_response_of_case_1_more_than_one_option_possible_MRI_follow_up_during_treatment",
  "How_would_you_monitor_treatment_response_of_case_1_more_than_one_option_possible_MRI_follow_up_at_end_of_treatment",
  "How_would_you_monitor_treatment_response_of_case_1_more_than_one_option_possible_Repeated_WBCC_measurement",
  "How_would_you_monitor_treatment_response_of_case_1_more_than_one_option_possible_Repeated_erythrocyte_sedimentation_rate_assessment",
  "How_would_you_monitor_treatment_response_of_case_1_more_than_one_option_possible_Other",
  "How_would_you_monitor_treatment_response_of_case_1_more_than_one_option_possible_NA",
  "How_long_would_you_treat_with_antibiotics_presuming_a_good_response_in_case_1_Not_sure",
  "How_long_would_you_treat_with_antibiotics_presuming_a_good_response_in_case_1_Two_weeks_i_v_followed_by_10_weeks_p_o",
  "How_long_would_you_treat_with_antibiotics_presuming_a_good_response_in_case_1_Two_weeks_i_v_followed_by_4_weeks_p_o",
  "How_long_would_you_treat_with_antibiotics_presuming_a_good_response_in_case_1_One_week_i_v_followed_by_5_weeks_p_o",
  "How_long_would_you_treat_with_antibiotics_presuming_a_good_response_in_case_1_Other",
  "How_long_would_you_treat_with_antibiotics_presuming_a_good_response_in_case_1_One_week_i_v_followed_by_11_weeks_p_o"
)






# Get all column names
all_column_names <- colnames(Spondylodiscitis_Variability_Excel_NEW)




# Replace non-alphanumeric characters with underscores
cleaned_column_names <- gsub("\\W", "_", all_column_names)

# Replace multiple underscores with a single underscore
cleaned_column_names <- gsub("_+", "_", cleaned_column_names)

# Remove leading underscores
cleaned_column_names <- gsub("^_", "", cleaned_column_names)

# Remove trailing underscores
cleaned_column_names <- gsub("_$", "", cleaned_column_names)


# Rename columns in the data frame
colnames(Spondylodiscitis_Variability_Excel_NEW) <- cleaned_column_names

# Define three sets of predictor variables
predictors_1 <- c("Country_Sweden", "Country_Ireland", "Country_Spain", "Country_UK", "Country_Belgium", "Country_Italy", "Country_Netherlands", "Country_India", "Country_Germany", "Country_Mexico", "Country_Romania", "Country_Australia", "Country_Portugal", "Country_Switzerland", "Country_NA", "Country_Greece", "Country_Russia", "Country_Serbia", "Country_Jordan", "Country_Turkey", "Country_Austria", "Country_France", "Country_Qatar", "Country_Bulgaria", "Country_Ukraine", "Country_Croatia", "Country_Finland", "Country_Rumania", "Country_Pakistan", "Country_Nigeria", "Country_Czech_Republic", "Country_Denmark", "Country_Saudi_Arabia", "Country_Saudi_arabia", "Country_Nepal", "Country_Hungary", "Country_Japan", "Country_Uzbekistan")
predictors_2 <- c("Role_Neurosurgeon_in_training", "Role_Board_certified_neurosurgeon", "Number_of_spinal_cases_as_responsible_surgeon_per_year_<50", "Number_of_spinal_cases_as_responsible_surgeon_per_year_>300", "Number_of_spinal_cases_as_responsible_surgeon_per_year_101-200", "Number_of_spinal_cases_as_responsible_surgeon_per_year_50-100", "Number_of_spinal_cases_as_responsible_surgeon_per_year_201-300", "Special_interest_in_spine_surgery_No", "Special_interest_in_spine_surgery_Yes", "Special_interest_in_spine_surgery_NA")
predictors_3 <- c("Years_in_practice", "Routine_neurosurgery_follow-up_for_discitis", "Follow-up_period_(months)_for_discitis")

predictors_all <- list(predictors_1, predictors_2, predictors_3)

# Create a list to store plots
plot_list <- list()

# Load necessary libraries
library(broom)

# Create an empty dataframe to store results
regression_results <- data.frame()

# Loop through all predictor sets
for (j in 1:length(predictors_all)) {
  predictors <- predictors_all[[j]]
  
  # Loop through each outcome variable
  for (outcome in outcomenames) {
    
    # Loop through each predictor variable
    for (predictor in predictors) {
      # Check if the predictor has more than one level
      if (length(unique(Spondylodiscitis_Variability_Excel_NEW[[predictor]])) > 1) {
        # Fit the logistic regression model
        formula <- as.formula(paste(outcome, "~", predictor))
        logmodel <- glm(formula, data = Spondylodiscitis_Variability_Excel_NEW, family = binomial(link = 'logit'), control = list(maxit = 50))
        
        # Get the tidy output and exclude the intercept
        tidy_output <- tidy(logmodel)
        tidy_output <- tidy_output[tidy_output$term != "(Intercept)",]
        
        # Add additional information
        tidy_output$outcome <- outcome
        tidy_output$predictor <- predictor
        tidy_output$predictor_set <- j
        tidy_output$formula <- deparse(formula)  # Add formula to the output
        
        # Bind to the results dataframe
        regression_results <- rbind(regression_results, tidy_output)
      }
    }
  }
}


# ...

# Filter the results for p-values less than 0.05
significant_results <- regression_results[regression_results$p.value < 0.05,]

# Print the significant results
print(significant_results)

# Count the number of significant predictors
significant_predictor_count <- nrow(significant_results)

# Print the count
print(paste("Number of significant predictors: ", significant_predictor_count))




###############grid for Case 1


# Define the plot list
plot_list <- list()

# Define the plot list
plot_list <- list()

# Loop through all predictor sets
for (j in 1:length(predictors_all)) {
  predictors <- predictors_all[[j]]
  
  # Loop through each outcome variable
  for (outcome in outcomenames) {
    
    # Loop through each predictor variable
    for (predictor in predictors) {
      # Check if the predictor has more than one level
      if (length(unique(Spondylodiscitis_Variability_Excel_NEW[[predictor]])) > 1) {
        # Fit the logistic regression model
        logmodel <- glm(as.formula(paste(outcome, "~", predictor)), data = Spondylodiscitis_Variability_Excel_NEW, family = binomial(link = 'logit'), control = list(maxit = 50))
        
        # Get the tidy output and exclude the intercept
        tidy_output <- tidy(logmodel)
        tidy_output <- tidy_output[tidy_output$term != "(Intercept)",]
        
        # Check if the p-value is less than 0.05
        if (tidy_output$p.value[1] < 0.05) {
          # If predictor is binary
          if (length(unique(Spondylodiscitis_Variability_Excel_NEW[[predictor]])) == 2) {
            # Create data frame for geom_hline
            coef_df <- data.frame(predictor_level = c(0, 1),
                                  predicted_prob = exp(coef(logmodel)["(Intercept)"] + coef(logmodel)[predictor]*c(0, 1)) / 
                                    (1 + exp(coef(logmodel)["(Intercept)"] + coef(logmodel)[predictor]*c(0, 1))))
            
            p <- ggplot(Spondylodiscitis_Variability_Excel_NEW, aes_string(x = predictor, y = outcome)) +
              geom_jitter(width = 0.2, height = 0.1, alpha = 0.5) +
              geom_hline(data = coef_df, aes(yintercept = predicted_prob, color = as.factor(predictor_level))) +
              scale_color_manual(name = "Predictor Level", values = c("0" = "Blue", "1" = "Red")) +
              labs(x = predictor, y = outcome) + theme(axis.title.y = element_text(size = 6))
            
            plot_list[[length(plot_list) + 1]] <- p
          } else {  # Predictor is continuous
            p <- ggplot(Spondylodiscitis_Variability_Excel_NEW, aes_string(x = predictor, y = outcome)) +
              geom_point(alpha = 0.5) +
              geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE, color = "red") +
              labs(x = predictor, y = outcome) + theme(axis.title.y = element_text(size = 6))
            
            plot_list[[length(plot_list) + 1]] <- p
          }
        }
      }
    }
  }
}



# Install and load the gridExtra library
if (!requireNamespace("gridExtra", quietly = TRUE)) {
  install.packages("gridExtra")
}
library(gridExtra)

# Now we are going to create chunks of the plot_list
chunk_size <- 7 # Maximum 6 plots per graph
plot_chunks <- split(plot_list, ceiling(seq_along(plot_list)/chunk_size))

# Now for each chunk we create a separate plot
for(i in seq_along(plot_chunks)){
  current_plots <- plot_chunks[[i]]
  # Specify the number of rows and columns in the grid
  nrow <- 3
  ncol <- 3
  
  
  # Arrange the plots in the grid
  grid_plot <- grid.arrange(grobs = current_plots, nrow = nrow, ncol = ncol)
  
  # Save the grid plot to a file
}




























##########################

# Define the first and second list of plots
plot_list1 <- list()
plot_list2 <- list()

# Loop through all predictor sets
for (j in 1:length(predictors_all)) {
  predictors <- predictors_all[[j]]
  
  # Loop through each outcome variable
  for (outcome in outcomenames) {
    
    # Loop through each predictor variable
    for (predictor in predictors) {
      
      # Check if the predictor has more than one level
      if (length(unique(Spondylodiscitis_Variability_Excel_NEW[[predictor]])) > 1) {
        
        # Fit the logistic regression model
        logmodel <- glm(as.formula(paste(outcome, "~", predictor)), data = Spondylodiscitis_Variability_Excel_NEW, family = binomial(link = 'logit'), control = list(maxit = 50))
        
        # Get the tidy output and exclude the intercept
        tidy_output <- tidy(logmodel)
        tidy_output <- tidy_output[tidy_output$term != "(Intercept)",]
        
        # Check if the p-value is less than 0.05
        if (tidy_output$p.value[1] < 0.05) {
          
          # If predictor is binary
          if (length(unique(Spondylodiscitis_Variability_Excel_NEW[[predictor]])) == 2) {
            
            # Create data frame for geom_hline
            coef_df <- data.frame(predictor_level = c(0, 1),
                                  predicted_prob = exp(coef(logmodel)["(Intercept)"] + coef(logmodel)[predictor]*c(0, 1)) / 
                                    (1 + exp(coef(logmodel)["(Intercept)"] + coef(logmodel)[predictor]*c(0, 1))))
            
            p <- ggplot(Spondylodiscitis_Variability_Excel_NEW, aes_string(x = predictor, y = outcome)) +
              geom_jitter(width = 0.2, height = 0.1, alpha = 0.5) +
              geom_hline(data = coef_df, aes(yintercept = predicted_prob, color = as.factor(predictor_level))) +
              scale_color_manual(name = "Predictor Level", values = c("0" = "Blue", "1" = "Red")) +
              labs(x = predictor, y = outcome) + theme(axis.title.y = element_text(size = 3))  # Adjust text size.
            
            if(length(plot_list1) < 8){
              plot_list1[[length(plot_list1) + 1]] <- p
            } else {
              plot_list2[[length(plot_list2) + 1]] <- p
            }
            
          } else {  # Predictor is continuous
            p <- ggplot(Spondylodiscitis_Variability_Excel_NEW, aes_string(x = predictor, y = outcome)) +
              geom_point(alpha = 0.5) +
              geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE, color = "red") +
              labs(x = predictor, y = outcome) + theme(axis.text.y = element_text(size = 3))  # Adjust text size.
            
            if(length(plot_list1) < 8){
              plot_list1[[length(plot_list1) + 1]] <- p
            } else {
              plot_list2[[length(plot_list2) + 1]] <- p
            }
          }
        }
      }
    }
  }
}

# Arrange the plots in two 3x3 grids
grid1 <- plot_grid(plotlist = plot_list1, ncol = 3)
grid2 <- plot_grid(plotlist = plot_list2, ncol = 3)


print(grid1)
print(grid2)


























#same size

# Now we are going to create chunks of the plot_list
chunk_size <- 6 # Maximum 6 plots per graph
plot_chunks <- split(plot_list, ceiling(seq_along(plot_list)/chunk_size))

# Now for each chunk we create a separate plot
for(i in seq_along(plot_chunks)){
  current_plots <- plot_chunks[[i]]
  
  # If the current chunk has less than 6 plots, add empty plots
  if(length(current_plots) < chunk_size) {
    for(j in (length(current_plots) + 1):chunk_size) {
      current_plots[[j]] <- ggplot() + theme_void()
    }
  }
  
  # Arrange the plots in the grid
  plot_grid(plotlist = current_plots, ncol = 3)
  
  # Now, you can save the grid plot to a file, or draw it directly
  # ggsave(filename = paste("grid_plot_", i, ".png", sep = ""), plot = last_plot())
}



## optimised

# Load necessary libraries
library(ggplot2)
library(cowplot)
library(broom)
library(gridExtra)

# Define a function to add p-value and Adjusted R-squared to the plot
# Define a function to add p-value and AIC to the plot
add_stats_to_plot <- function(plot, model) {
  p_value <- tidy(model)$p.value[1]
  aic <- glance(model)$AIC
  plot +
    annotate("text", x = Inf, y = Inf, label = paste0("p-value = ", round(p_value, 3), "\nAIC = ", round(aic, 3)), 
             hjust = 1, vjust = 1, size = 3, color = "black")
}

# Define the plot list
plot_list <- list()

# Loop through all predictor sets
for (j in 1:length(predictors_all)) {
  predictors <- predictors_all[[j]]
  
  # Loop through each outcome variable
  for (outcome in outcomenames) {
    
    # Loop through each predictor variable
    for (predictor in predictors) {
      # Check if the predictor has more than one level
      if (length(unique(Spondylodiscitis_Variability_Excel_NEW[[predictor]])) > 1) {
        # Fit the logistic regression model
        logmodel <- glm(as.formula(paste(outcome, "~", predictor)), data = Spondylodiscitis_Variability_Excel_NEW, family = binomial(link = 'logit'), control = list(maxit = 50))
        
        # Get the tidy output and exclude the intercept
        tidy_output <- tidy(logmodel)
        tidy_output <- tidy_output[tidy_output$term != "(Intercept)",]
        
        # Check if the p-value is less than 0.05
        if (tidy_output$p.value[1] < 0.05) {
          # If predictor is binary
          if (length(unique(Spondylodiscitis_Variability_Excel_NEW[[predictor]])) == 2) {
            # Create data frame for geom_hline
            coef_df <- data.frame(predictor_level = c(0, 1),
                                  predicted_prob = exp(coef(logmodel)["(Intercept)"] + coef(logmodel)[predictor]*c(0, 1)) / 
                                    (1 + exp(coef(logmodel)["(Intercept)"] + coef(logmodel)[predictor]*c(0, 1))))
            
            p <- ggplot(Spondylodiscitis_Variability_Excel_NEW, aes_string(x = predictor, y = outcome)) +
              geom_jitter(width = 0.2, height = 0.1, alpha = 0.5) +
              geom_hline(data = coef_df, aes(yintercept = predicted_prob, color = as.factor(predictor_level))) +
              scale_color_manual(name = "Predictor Level", values = c("0" = "Blue", "1" = "Red")) +
              labs(x = predictor, y = outcome, title = paste("Binary Predictor Plot: ", predictor, "vs", outcome)) +
              theme_minimal() +
              theme(plot.title = element_text(hjust = 0.5, color = "black", size = 14, face = "bold"),
                    axis.title.x = element_text(color = "black", size = 12),
                    axis.title.y = element_text(color = "black", size = 12))
            # Add statistics to the plot
            p <- add_stats_to_plot(p, logmodel)
            plot_list[[length(plot_list) + 1]] <- p
          } else {  # Predictor is continuous
            p <- ggplot(Spondylodiscitis_Variability_Excel_NEW, aes_string(x = predictor, y = outcome)) +
              geom_point(alpha = 0.5) +
              geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE, color = "red") +
              labs(x = predictor, y = outcome, title = paste("Continuous Predictor Plot: ", predictor, "vs", outcome)) +
              theme_minimal() +
              theme(plot.title = element_text(hjust = 0.5, color = "black", size = 14, face = "bold"),
                    axis.title.x = element_text(color = "black", size = 12),
                    axis.title.y = element_text(color = "black", size = 12))
            # Add statistics to the plot
            p <- add_stats_to_plot(p, logmodel)
            plot_list[[length(plot_list) + 1]] <- p
          }
        }
      }
    }
  }
}

# Now we are going to create chunks of the plot_list
chunk_size <- 15 # Maximum 6 plots per graph
plot_chunks <- split(plot_list, ceiling(seq_along(plot_list)/chunk_size))

# Now for each chunk we create a separate plot
for(i in seq_along(plot_chunks)){
  current_plots <- plot_chunks[[i]]
  # Specify the number of rows and columns in the grid
  nrow <- 5
  ncol <- 3
  
  # Arrange the plots in the grid
  grid_plot <- grid.arrange(grobs = current_plots, nrow = nrow, ncol = ncol)
  
}



























































#doubt about binarisation


# Import necessary libraries
library(tidyverse)
library(readxl)


# Import necessary libraries
library(tidyverse)
library(readxl)

# Import necessary libraries
library(tidyverse)
library(readxl)

# Load your initial dataset
Spondylodiscitis_Variability_Excel <- read_excel("OneDrive/Spondylodiscitis Variability/Spondylodiscitis Variability Excel.xlsx")

# Define the columns to be split
columns_to_split <- c("How would you treat case 1?", "How would you treat case 2?", "How would you treat case 3?",
                      "How would you treat case 4?", "How would you treat case 5?", "How would you treat case 6?",
                      "How would you treat case 7?", "Typical indications for surgery in spinal infections", 
                      "How would you monitor treatment response of case 1?  (more than one option possible)", 
                      "How would you monitor treatment response in case 2?  (more than one option possible)")

# Replace ";" with "," in the original columns
Spondylodiscitis_Variability_Excel[columns_to_split] <- lapply(Spondylodiscitis_Variability_Excel[columns_to_split], function(x) gsub(";", ", ", x))

# Define the additional categorical columns to be binarized
additional_columns_to_bin <- c("Country", "Role", "Number of spinal cases as responsible surgeon per year",
                               "Special interest in spine surgery", "Typical indications for surgery in spinal infections",
                               "Proportion of spinal infection cases referred to their service that are treated surgically",
                               "Routine neurosurgery follow-up for discitis", "Follow-up period (months) for discitis",
                               "How long would you treat with antibiotics, presuming a good response in case 1?",
                               "How long would you treat with antibiotics, presuming a good response in case 2?", 
                               "How long would you treat with antibiotics, presuming a good response in case 5?",
                               "How long would you treat with antibiotics, presuming a good response in case 6?")

# Create dummy variables for additional columns
for (col in additional_columns_to_bin) {
  Spondylodiscitis_Variability_Excel <- Spondylodiscitis_Variability_Excel %>%
    mutate(dummy = if_else(get(col) != "", 1, 0)) %>%
    pivot_wider(names_from = col, values_from = dummy, names_prefix = paste0(col, ": "), values_fill = 0)
}

# Apply the split_data function to the specified columns
new_columns <- lapply(columns_to_split, function(column) split_data(Spondylodiscitis_Variability_Excel, column))

# Append the new columns to the original dataframe
Spondylodiscitis_Variability_Excel_NEWCheck <- cbind(Spondylodiscitis_Variability_Excel, do.call(cbind, new_columns))

# Write the updated dataframe to a new Excel file
write.xlsx(Spondylodiscitis_Variability_Excel_NEWCheck, "Spondylodiscitis_Variability_Excel_NEWCheck.xlsx")

# View the updated dataframe
print(Spondylodiscitis_Variability_Excel_NEWCheck)

# Define a function to check if binarization was correct
check_binarization <- function(original_df, binarized_df, column) {
  
  # Split original column data into separate values
  original_values <- unique(unlist(strsplit(as.character(original_df[[column]]), ", ")))
  
  # Generate expected column names from original values
  expected_column_names <- paste(column, original_values, sep = ": ")
  
  # Check if all expected column names exist in the binarized dataframe
  missing_columns <- setdiff(expected_column_names, colnames(binarized_df))
  
  if (length(missing_columns) > 0) {
    print(paste("Binarization of column", column, "was not correct. The following columns are missing:"))
    print(missing_columns)
  } else {
    print(paste("Binarization of column", column, "was correct."))
  }
}

# Apply the check_binarization function to each column that was supposed to be binarized
lapply(columns_to_split, function(column) check_binarization(Spondylodiscitis_Variability_Excel, Spondylodiscitis_Variability_Excel_NEWCheck, column))

# Write the updated dataframe to a new Excel
print(Spondylodiscitis_Variability_Excel_NEWCheck)




















