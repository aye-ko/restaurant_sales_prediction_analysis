library(tidyverse)
library(lubridate)
library(ggplot2)
library(rmarkdown)
#load data 
data <- read.csv("9. Sales-Data-Analysis.csv")

#view dataset
colnames(data)
head(data)
str(data)


#Process and clean data and create copy of old data set 

data_clean <- data %>%
  mutate(
    Date = dmy(Date), #convert Date to format
    Manager = str_squish(Manager), #remove extra spaces
    City = str_squish(City), 
    Product = str_squish(Product)
  )

head(data_clean)
rm(data)

#Analysis

#Create daily Sales Summary
daily_sales <- data_clean %>%
  group_by(Date) %>%
  summarize(
    total_sales = sum(Price * Quantity),
    num_orders = n()
  ) %>%
  mutate(
    day_of_week = wday(Date, label = TRUE)
  ) %>%
  arrange(Date)

print(range(daily_sales$Date))
sales_average <- daily_sales %>%
  group_by(day_of_week) %>%
  summarize(mean_sales = mean(total_sales),
            mean_orders = mean(num_orders),
            average_cost_of_orders = mean_sales / mean_orders
            )

december_sales <- daily_sales %>%
        filter(Date >= "2022-12-01")
print(summary(december_sales$total_sales))


#Visualization

ggplot(daily_sales, aes(x= Date, y = total_sales)) +
  geom_line() +
  labs(
    title = "Sales Trend",
    x = "Date",
    y = "Total Sales",
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::label_number(scale = 1/1000, suffix = "K")) +
  scale_x_date(date_breaks = "5 days", date_labels = "%d/%m")