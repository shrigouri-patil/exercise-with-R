library(dplyr)
library(tidyr)
library(hms)
library(ggplot2)

#open the file with Data

activity=read.csv("D:\\Gouri\\R-course\\datasets\\fly activity avergae of 3 days.csv", header = TRUE)

#Seperating mean values and SEM values using "dplyr"
Mean_data= activity%>%
  select(Time, Control, Test1, Test2, Test3 )

Mean_data

SEM_data= activity%>%
  select(Time, SEM_Control, SEM_Test1, SEM_Test2, SEM_Test3 )

SEM_data

#Converting wide format to long format using "tidyr" package

#reshape mean values
long_Mean_data = Mean_data %>%
  pivot_longer(
    cols = starts_with("C")|starts_with("Test"),  # Select columns starting with "C" or "Test"
    names_to = "Variable",           
    values_to = "Value" )

long_Mean_data


#reshape SEM values
long_SEM_data= SEM_data%>%
  pivot_longer(
    cols=starts_with("SEM"), # Select columns starting with "SEM"
    names_to="SEM_Variable",
    values_to="SEM_Value")

long_SEM_data

# Rename the "SEM_variable" names to match the "Variable" to match with long_data
#"dplyr" package used
long_SEM_data <- long_SEM_data %>%
  mutate(Variable = gsub("SEM_", "", SEM_Variable)) %>%
  select(-SEM_Variable)

long_SEM_data


# Join the mean and SEM data by the 'time' and 'Variable' columns
final_data <- long_Mean_data %>%
  left_join(long_SEM_data, by = c("Variable", "Time"))

final_data

#Time column is in chracter form, to conver it into numeric form "hms" library is used
str(final_data)

final_data$Time <- hms::as_hms(final_data$Time) #converts to hms form
final_data$Time <- as.numeric(final_data$Time)/3600 #converts into hours in numeric form

str(final_data)

#"ggplot2" package was used to draw the graph
ggplot(data = final_data, aes(x = Time, y= Value, colour= Variable, group= Variable)) +
  geom_line(size=0.5)+geom_point(size=2)+
  geom_errorbar(aes(ymin = Value - SEM_Value, ymax = Value + SEM_Value),  # Error bars
                width = 0.2, size = 0.1) +           # Customize error bar width and size
  geom_vline(xintercept = 6, linetype = "dashed", color = "black") +  # Add a threshold at x = 6
  geom_vline(xintercept = 18, linetype = "dashed", color = "black") +  # Add a threshold at x = 18
  geom_rect(aes(xmin = -Inf, xmax = 6, ymin = -Inf, ymax = Inf),color = NA, show.legend=FALSE, fill = "gray", alpha = 0.01) + # Add shaded background before the threshold (x < 6)
  geom_rect(aes(xmin = 18, xmax = Inf, ymin = -Inf, ymax = Inf),color = NA, show.legend=FALSE, fill = "gray", alpha = 0.01) + # Add shaded background after the threshold (x > 18)
  labs(title = "Activity of flies", x="time (in hrs)", y="Activity", colour="Conditions")+
  scale_color_brewer(palette = "Set2") +
  theme_classic()

