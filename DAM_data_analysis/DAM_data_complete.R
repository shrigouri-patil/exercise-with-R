library(tidyverse)

# Set the directory containing the data files
data_dir = "Raw_Data"

output_dir = "Processed_Data"

# Get a list of all data files in the directory (for all txt files from data_dir folder)
data_files = list.files(data_dir, pattern = ".txt$", full.names = TRUE)

# Create a function to process a single file
process_file = function(file_path) {
  # Step 1: Read the file
  data = read.delim(file_path, header = FALSE, sep = "\t")
  
  # Step 2: Format the data to readable form and filter the data
  # Rename and select required columns
  data = data %>%
    rename(number = V1, date = V2, Time = V3) %>%    # renames reuired columns
    select(-V4:-V10)                                 # removes columns from V4to V10
  
  # Create a Timestamp column. which has both date and Time in POSIXct form.
  # POSIXct is from lubridate package. It formats Time and Date in R readable form.
  # d: date; b: month abbreviation; y: year; H: hour; M: minute; S: second
  data$Timestamp = as.POSIXct(paste(data$date, data$Time), format = "%d %b %y %H:%M:%S")
  
  # Get the start Time: calculate to get the midnight of the 2nd day
  start_time = as.POSIXct(paste( as.Date(data$Timestamp[1]) + 2, "00:01:00"),  # select the midnight time from the third day
                           format = "%Y-%m-%d %H:%M:%S")   
  
  #Get the stop Time: Calculate to get the midnight of the 5th day 
  stop_time = as.POSIXct(paste(as.Date(data$Timestamp[1]) + 5, "00:00:00"),    # select the midnight time from the 3rd day (5thday) from start time
                          format = "%Y-%m-%d %H:%M:%S")   
  
  
  # Filter data after the midnight of the 3rd day to next 3days
  data_to_analyse = data[data$Timestamp >= start_time & data$Timestamp <= stop_time, ]
  
  # Step 3: Summation of the data to 30 minute activity
  # Create a 30-minute interval column
  data_to_analyse = data_to_analyse %>%
    mutate(Interval = floor_date(Timestamp, "30 minutes")) #floor_date rounds up to the nearest (next) 30minute group
  
  # Summarize data for each 30-minute interval
  halfanhour_data = data_to_analyse %>%
    group_by(Interval) %>%                               # in data_to_analyse it groups data based on Interval (30minutes)
    summarize(                                           # in each column, starting with 'V' (V11,V12...), based on above grouping it summate
      across(starts_with("V"), ~ sum(.x, na.rm = TRUE)),
      .groups = "drop"                                   # affter summaion, the data is ungrouped
    )
  
  # Now there is a possibility that due to technical issue like
  # there was no recording in any particular tube or column
  # we will identify these columns and delete before further processing
  # if there is no movement or recording for a continous 24hrs we will delete that column
  # to remove columns with 48 consecutive zeros
  # Select those columns in which we want to look for no activity 
  activity_cols_1 = grep("^V", colnames(halfanhour_data), value = TRUE) 
  
  # Identify which columns (from the recording columns) have 48 consecutive zeros
  cols_to_remove= sapply(activity_cols_1, function(col_names) {                    
    # Run Length Encoding (rle) is used to compress data by getting the repeated values 
    # using rle, we are trying to find the column which has continous 0 for 24hrs (48 readings)
    rle_values = rle(halfanhour_data[[col_names]])  # Apply rle to the column
    any(rle_values$values == 0 & rle_values$lengths >= 48)  # Check for 48 consecutive zeros
  })
  
  
  # To remove colmns found in cols_to_remove
  filtered_data = halfanhour_data[, !(colnames(halfanhour_data)               #!negates and %n% looks if the halfanhour_data has columns from columns_to rmove
                                       %in% activity_cols_1[cols_to_remove])]
  
  # Extract time from the interval
  filtered_data$Time = format(filtered_data$Interval, "%H:%M:%S")
  
  # Step4: Calculate the 3-day average activity
  
  avg_3day = filtered_data %>%
    group_by(Time) %>%                                     #group based on time
    summarize(                                             
      across(starts_with("V"), ~ mean(.x, na.rm = TRUE)),  # finds the mean value, among each group
      .groups = "drop"
    )
  
  # Step5: 24hrs of activity, Calculate the average and SEM for each time point
  
  activity_cols_2 = grep("^V", colnames(avg_3day), value = TRUE) # select all columns which starts with V in avg_3day
  
  avg_3day = avg_3day %>%
    mutate(
      average = rowMeans(select(., all_of(activity_cols_2)), na.rm = TRUE),
      SEM = apply(select(., all_of(activity_cols_2)), 1, function(x) {
        x = na.omit(x)
          return(sd(x) / sqrt(length(x)))
      })
    )
  
  # Select and return the final data
  data_24hr = avg_3day %>% select(Time, average,SEM)
  return(data_24hr)
}



# Initialize an empty data frame to store the consolidated results
consolidated_results = data.frame()

# Loop through all files and process them
for (file in data_files) {                  # files selected using data_files will be used
  # Process the file based on above mentioned function 
  processed_data = process_file(file) 
  
  # Extract the file name without the extension
  file_name = tools::file_path_sans_ext(basename(file))

  # Rename the columns based on the file name
  colnames(processed_data)[colnames(processed_data) == "average"] = file_name
  colnames(processed_data)[colnames(processed_data) == "SEM"] = paste0("SEM_", file_name)
  
  # Merge the processed data into the consolidated results
  if (nrow(consolidated_results) == 0) {
    # For the first file, use the entire data frame
    consolidated_results = processed_data
  } else {
    # For subsequent files, merge by the 'Time' column
    consolidated_results = merge(consolidated_results, processed_data, by = "Time", all = TRUE)
  }
} 
  


# Save the consolidated results to a single CSV file
output_file = file.path(output_dir, "consolidated_results.csv")
write.csv(consolidated_results, file = output_file, row.names = FALSE)

# Print a message
cat("Processing completed. Consolidated results saved to:", output_file, "\n")




# To Plot a graph

activity=read.csv(output_file)

# Seperating Mean values and SEM values using "dplyr"
Mean_data= activity%>%
  select(-starts_with("SEM_") )  # Select all columns except for those starting with "SEM_"

SEM_data= activity%>%
  select(Time, starts_with("SEM_") ) # Select Time and all columns starting with "SEM_"

# Converting wide format to long format using "tidyr" package

#reshape mean values
long_Mean_data = Mean_data %>%
  pivot_longer(
    cols = -"Time",  # Select columns other than "Time"
    names_to = "Variable",           
    values_to = "Value" )

#reshape SEM values
long_SEM_data= SEM_data%>%
  pivot_longer(
    cols=starts_with("SEM"), # Select columns starting with "SEM"
    names_to="SEM_Variable",
    values_to="SEM_Value")

# Rename the "SEM_variable" names to match the "Variable" to match with long_data
#"dplyr" package used
long_SEM_data <- long_SEM_data %>%
  mutate(Variable = gsub("SEM_", "", SEM_Variable)) %>%
  select(-SEM_Variable)

# Join the mean and SEM data by the 'Time' and 'Variable' columns
final_data = long_Mean_data %>%
  left_join(long_SEM_data, by = c("Variable", "Time"))

#Time column is in character form, to convert it into numeric form 

final_data$Time = hms::as_hms(final_data$Time) #converts to hms form
final_data$Time = as.numeric(final_data$Time)/3600 #converts into hours in numeric form

#"ggplot2" package was used to draw the graph
ggplot(data = final_data, aes(x = Time, y= Value, colour= Variable, group= Variable)) +
  geom_line(size=0.5)+geom_point(size=2)+
  geom_errorbar(aes(ymin = Value - SEM_Value, ymax = Value + SEM_Value),  # Error bars
                width = 0.2, size = 0.1) +           # Customize error bar width and size
  geom_vline(xintercept = 6, linetype = "dashed", color = "black") +  # Add a threshold at x = 6
  geom_vline(xintercept = 18, linetype = "dashed", color = "black") +  # Add a threshold at x = 18
  geom_rect(aes(xmin = -Inf, xmax = 6, ymin = -Inf, ymax = Inf),color = NA, show.legend=FALSE, fill = "gray", alpha = 0.01) + # To represent night: Add shaded background before the threshold (x < 6)
  geom_rect(aes(xmin = 18, xmax = Inf, ymin = -Inf, ymax = Inf),color = NA, show.legend=FALSE, fill = "gray", alpha = 0.01) + # To represent night: Add shaded background after the threshold (x > 18)
  labs(title = "Activity of flies", x="Time (in hrs)", y="Activity", colour="Conditions")+
  scale_color_brewer(palette = "Set2") +
  scale_x_continuous(limits = c(0, NA), expand = c(0, 0)) +  # Ensure x-axis starts at 0
  theme_classic()

