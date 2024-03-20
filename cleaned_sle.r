# Load the dplyr and reshape2 packages
library(dplyr)
library(reshape2)

# Read the CSV file into a dataframe
# df <- read.csv('sle_agents.csv')

# Create a copy of the dataframe
df.2 <- df

# Remove the 'Inside.Outside' column from the dataframe
df.2["W.L"] <- NULL 

# Get the column names of the dataframe
names = colnames(df.2)

# For each column name
for (name in names) {
  # If the column name is not 'Names'
  if(name != 'Names') {
    # Replace 1t with the column name and other values with NA
    df.2[[name]] = ifelse(df.2[[name]] == 1, name, NA)
  }
}

# Melt the dataframe to long format, keeping 'Names' as the id variable
df.2_long <- melt(df.2, id.vars = "Names")

# Select only the 'Names' and 'W/L' (win/loss) columns from the dataframe
df <- select(df,'Names','W.L')

# Merge the selected columns with the melted dataframe
merged_df <- merge(df, df.2_long, by = "Names", all.x = TRUE)

# Remove the 'value' column from the merged dataframe
merged_df['variable'] <- NULL

# Rename the 'variable' column to 'Date'
names(merged_df)[names(merged_df) == "value"] <- "Date"

# Replace periods with slashes in the 'Date' column
merged_df$Date <- gsub("\\.", "/", merged_df$Date)

# Remove 'X' from the 'Date' column
merged_df$Date <- gsub("X", "", merged_df$Date)

# Convert the 'Date' column to date format
merged_df$Date <- as.Date(merged_df$Date, format = "%m/%d/%Y")

# Replace empty strings in the 'W.L' column with NA
merged_df$W.L[merged_df$W.L == ""] <- "D"


# Get weekday from date
merged_df$Week_Day <- weekdays(merged_df$Date)

# Count distinct names total
# Used later in calc
distinct_count <- df_summary$Names %>% 
  n_distinct()

# Group "merged_df" df by the "Date" column, calculate the distinct count of "Names" for each group, 
# then calculate the percentage of these counts relative to 44. 

df_summary <- merged_df %>%
  group_by(Date) 

df_summary <- df_summary %>% 
  summarise(distinct_rows = n_distinct(Names)) %>%
  mutate(percent_attended = distinct_rows/43) %>% # 43 number received from distinct count earlier
  ungroup() %>%
  right_join(merged_df, by = c("Date" = "Date"))

# Remove all rows with na in Weeday column
df_summary <- df_summary[complete.cases(df_summary$Week_Day),]

# Write to csv
#write.csv(df_summary,'appended_sle.csv')



