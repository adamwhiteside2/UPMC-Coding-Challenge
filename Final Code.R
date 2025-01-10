# Question 1:

#load necessary packages
library(dplyr)
library(lubridate)
#load data
load("C:/Users/ADAMI/Downloads/upmc code/CodeChallenge2024.RData")
#load the IDs
ids <- readLines("IDs.txt")

# Convert IDs to numeric 
ids <- as.numeric(ids)

# Step 1: Filter `id_map` for relevant participants
id_map_filtered <- id_map[id_map$old_id %in% ids, ]  # Filter rows where old_id matches ids

#take the new ids, what we want
selected_ids <- id_map_filtered$new_id
print(selected_ids)

# Create a named vector for easy lookup
id_mapping <- setNames(id_map_filtered$new_id, id_map_filtered$old_id)

# Step 2: Filter `HAM_protect` using old_ids
HAM_protect_clean <- HAM_protect[HAM_protect$ID %in% names(id_mapping), ]

# Step 3: Replace old_id in `HAM_protect` with new_id from the mapping
HAM_protect_clean$new_id <- id_mapping[as.character(HAM_protect_clean$ID)]

# Step 4: Filter `HAM_sleep` using new_ids
HAM_sleep_clean <- HAM_sleep[HAM_sleep$ID %in% id_map_filtered$new_id, ]


# Step 5: Define a function to calculate HAM scores and clean data for HAM_Protect

clean_and_score <- function(data, selected_ids, id_column) {
  # Filter data for selected IDs (using the new_id column in HAM_protect_clean)
  data <- data[data[[id_column]] %in% selected_ids, ]
  
  # Identify HAM columns
  ham_columns <- grep("^ham_", names(data), value = TRUE)
  
  # Exclude ham_3a to ham_3e columns
  exclude_columns <- grep("^ham_3[abcde]$", ham_columns, value = TRUE)
  ham_columns <- setdiff(ham_columns, exclude_columns)
  
  # Check for non-numeric values before conversion
  print("Checking non-numeric values in HAM columns before conversion:")
  print(unique(unlist(data[ham_columns])))
  
  # Convert HAM columns to numeric (ignoring warnings)
  data[ham_columns] <- lapply(data[ham_columns], function(x) suppressWarnings(as.numeric(as.character(x))))
  
  # Check if any conversion failed (NA introduced where there shouldn't be any)
  if (any(is.na(data[ham_columns]))) {
    print("Warning: Some values could not be converted to numeric. Check the columns above.")
  }
  
  # Calculate total HAM score (sum, ignoring NAs, excluding 3a to 3e)
  data$total_ham <- apply(data[, ham_columns], 1, function(row) {
    valid_values <- row[!is.na(row)]
    if (length(valid_values) == 0) {
      return(NA)  # Return NA if all values are NA
    } else {
      return(sum(valid_values, na.rm = TRUE))  # Sum the non-NA values
    }
  })
  
  return(data)
}


#Apply the function to HAM_protect_clean
HAM_protect_clean <- clean_and_score(HAM_protect_clean, selected_ids, "new_id")

# Remove the ID column and rename new_id to ID
HAM_protect_clean <- HAM_protect_clean %>%
  select(-ID) %>%
  rename(ID = new_id)%>%
  select(ID, everything())

#repeat for sleep

clean_and_score_sleep <- function(data, selected_ids, id_column) {
  # Filter data for selected IDs (using the ID column in ham_sleep)
  data <- data[data[[id_column]] %in% selected_ids, ]
  
  # Identify HAM columns
  ham_columns <- grep("^ham_", names(data), value = TRUE)
  
  # Exclude ham_3a to ham_3e columns
  exclude_columns <- grep("^ham_3[abcde]$", ham_columns, value = TRUE)
  ham_columns <- setdiff(ham_columns, exclude_columns)
  
  # Check for non-numeric values before conversion
  print("Checking non-numeric values in HAM columns before conversion:")
  print(unique(unlist(data[ham_columns])))
  
  # Convert HAM columns to numeric (ignoring warnings)
  data[ham_columns] <- lapply(data[ham_columns], function(x) suppressWarnings(as.numeric(as.character(x))))
  
  # Check if any conversion failed (NA introduced where there shouldn't be any)
  if (any(is.na(data[ham_columns]))) {
    print("Warning: Some values could not be converted to numeric. Check the columns above.")
  }
  
  # Calculate total HAM score (sum, ignoring NAs, excluding 3a to 3e)
  data$total_ham <- apply(data[, ham_columns], 1, function(row) {
    valid_values <- row[!is.na(row)]
    if (length(valid_values) == 0) {
      return(NA)  # Return NA if all values are NA
    } else {
      return(sum(valid_values, na.rm = TRUE))  # Sum the non-NA values
    }
  })
  
  return(data)
}


# Call the function with the appropriate arguments for Ham_sleep
HAM_sleep_clean <- clean_and_score_sleep(HAM_sleep, id_map_filtered$new_id, "ID")


#remove rows with NA values for total_ham
HAM_protect_clean <- HAM_protect_clean[!is.na(HAM_protect_clean$total_ham), ]
HAM_sleep_clean <- HAM_sleep_clean[!is.na(HAM_sleep_clean$total_ham), ]

# Combine ham_protect_clean and ham_sleep_clean
combined_data <- bind_rows(HAM_protect_clean, HAM_sleep_clean)

# Ensure that 'timepoint' is a factor (so it doesn't interfere with date comparison)
combined_data$timepoint <- as.factor(combined_data$timepoint)

# Convert 'bq_date' and 'fug_date' to date format if they aren't already
combined_data$bq_date <- as.Date(combined_data$bq_date, format="%Y-%m-%d")
combined_data$fug_date <- as.Date(combined_data$fug_date, format="%Y-%m-%d")

# Create a new column for the first consent date: choose the earlier date between bq_date and fug_date
combined_data$first_consent_date <- pmin(combined_data$bq_date, combined_data$fug_date, na.rm = TRUE)


# For each unique ID, calculate the earliest consent date
combined_data <- combined_data %>%
  group_by(ID) %>%
  mutate(first_consent_date = min(first_consent_date, na.rm = TRUE)) %>%
  ungroup()


# Calculate mean HAM score (excluding 3a-3e)
combined_data <- combined_data %>%
  group_by(ID) %>%
  mutate(mean_ham = mean(total_ham, na.rm = TRUE)) %>%
  ungroup()

combined_data <- combined_data %>%
  group_by(ID) %>%
  mutate(
    # Calculate the time difference from first consent date for each row
    time_diff = abs(as.numeric(difftime(bq_date, first_consent_date, units = "days")) - 365),
    
    # Find the row with the smallest time difference (closest to 1 year after first consent)
    ham_one_year_after = total_ham[which.min(time_diff)],
    
    # Calculate the time difference from today's date for each row (latest score)
    latest_time_diff = abs(as.numeric(difftime(Sys.Date(), pmin(bq_date, fug_date, na.rm = TRUE), units = "days"))),
    
    # Find the latest HAM score closest to today's date
    latest_ham = total_ham[which.min(latest_time_diff)]
  ) %>%
  ungroup()

final_df <- combined_data %>%
  group_by(ID) %>%
  summarise(
    # Sum of total_ham for each ID
    total_ham_sum = sum(total_ham, na.rm = TRUE),
    
    # Mean of total_ham for each ID
    mean_ham = mean(total_ham, na.rm = TRUE),
    
    # Score closest to one year after first consent date
    ham_one_year_after = first(ham_one_year_after),
    
    # Latest score (closest to today's date)
    latest_ham = first(latest_ham)
  ) %>%
  ungroup()


library(reshape2)
library(ggplot2)
ggplot(final_df, aes(x = mean_ham, y = total_ham_sum, label = ID)) +
  geom_point(size = 3, color = "red") +
  geom_text(nudge_y = 10) +
  labs(title = "Mean HAM vs Total HAM Score", x = "Mean HAM", y = "Total HAM")


#Question 2:
#reload data #replace with necessary file path
load("C:/Users/ADAMI/Downloads/upmc code/CodeChallenge2024.RData")

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(scales)  # For percentage formatting
library(patchwork)  # For combining plots

# Calculate Summary Table
summary_table <- recruitment_data %>%
  group_by(RecruitSource, Gender, Group) %>%
  summarise(
    Total_Participants = n(),
    Percentage = n() / nrow(recruitment_data) * 100
  ) %>%
  arrange(desc(Total_Participants))


# Plot 1: Total participants by Recruitment Source with percentages
source_plot <- recruitment_data %>%
  count(RecruitSource) %>%
  ggplot(aes(x = reorder(RecruitSource, -n), y = n, label = scales::percent(n / sum(n)))) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  geom_text(vjust = -0.5, size = 3) +
  labs(
    title = "Total Participants by Recruitment Source (with Percentages)",
    x = "Recruitment Source",
    y = "Number of Participants"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(source_plot)

# Plot 2: Age Distribution as Percentages with Counts as Labels
age_percentage_plot <- ggplot(recruitment_data, aes(x = Age)) +
  geom_histogram(aes(y = (..count..) / sum(..count..)), 
                 bins = 10, fill = "skyblue", color = "black") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Age Distribution as Percentages",
    x = "Age",
    y = "Percentage"
  ) +
  theme_minimal() +
  geom_text(
    aes(y = (..count..) / sum(..count..), label = ..count..), 
    stat = "bin", 
    bins = 10, 
    vjust = -0.5, 
    size = 3
  )

print(age_percentage_plot)

# Plot 3: Total participants by Gender (with percentages)
gender_plot <- recruitment_data %>%
  count(Gender) %>%
  ggplot(aes(x = Gender, y = n, label = scales::percent(n / sum(n)))) +
  geom_bar(stat = "identity", fill = "pink", color = "black") +
  geom_text(vjust = -0.5, size = 3) +
  labs(
    title = "Total Participants by Gender (with Percentages)",
    x = "Gender",
    y = "Number of Participants"
  ) +
  theme_minimal()

print(gender_plot)

# Plot 4: Participants by Group as Percentages with Counts as Labels
group_percentage_plot <- ggplot(recruitment_data, aes(x = Group)) +
  geom_bar(aes(y = (..count..) / sum(..count..)), 
           fill = "lightgreen", color = "black") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Participants by Group as Percentages",
    x = "Group",
    y = "Percentage"
  ) +
  theme_minimal() +
  geom_text(
    aes(y = (..count..) / sum(..count..), label = ..count..), 
    stat = "count", 
    vjust = -0.5, 
    size = 3
  )

print(group_percentage_plot)





# Specify the file path where you want to save the CSV
       #file_path <- "C:/Users/ADAMI/Downloads/upmc code/final_df.csv"
# Export the data frame to a CSV file
       #write.csv(final_df, file = file_path, row.names = FALSE)

