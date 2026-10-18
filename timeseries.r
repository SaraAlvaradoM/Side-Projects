#Time Series Work/ Analisis de Series de Tiempo

#Created by/Creado por Sara Alvarado

#The goal is to refresh by time series skills.
#Working through Hyndman & Athanasopoulos' Forecasting: Principles and Practice (3rd edition) book.

library(fpp3)
library(readxl)
library(tsibble)
library(tidyverse)
library(lubridate)
library(dplyr)
library(patchwork)
library(hrbrthemes)
library(gganimate)
library(gifski)
library(animate)

#pull in data on power and create that into a tibble object.

##future work: make a web api to full this data monthly and automatically.

getwd() #this tells me what working directory I am in
setwd("C:/Users/salva/Desktop/R/Side-Projects") #set the working directory to where the Excel file is located

data_raw <- read_excel("PowerUsageTracker.xlsx")
#view(data_raw)

tibble_test <- data_raw |>
    select(-c("Avg. High Temp", "Avg. Low Temp")) |>
    rename("StartDate" = "Start Date") |>
    rename("AvgTemp" = "Avg. Avg. Temp") |>
    #remove_columns(c("Avg. High Temp", "Avg. Low Temp")) |>
    mutate(StartDate = ymd(StartDate), 
            Year = year(StartDate),
            Month = month(StartDate),
            PowerPerDay = Kilowatts/Days,
            CostPerDay = Cost/Days,
            CostPerKilowatt = Cost/Kilowatts) |>
    as_tibble()
view(tibble_test)

#pivot data for ease of plotting multiple variables in one chart
data_pivot <- tibble_test |>
    select(StartDate, Kilowatts, AvgTemp, PowerPerDay, Cost, Year, Month) |>
    pivot_longer(cols = c(Kilowatts, AvgTemp, PowerPerDay, Cost), names_to = "Variable", values_to = "Value") |>
    mutate(Year = as.integer(Year)) |> #convert year to factor for plotting
    mutate(Month = as.factor(Month)) #convert month to factor for plotting
#view(data_pivot)

#move this to function above once done testing
#data_pivot$Year <- as.numeric(data_pivot$Year) #convert year to numeric for animation

tempVpower <- ggplot(data_pivot, aes(x = StartDate, y = Value)) +
    geom_line(data = subset(data_pivot, Variable =="PowerPerDay"), stat = "identity", color = "blue") +
    # get some form of trend line on these two  
    geom_line(data = subset(data_pivot, Variable =="AvgTemp"), stat = "identity", color = "red") +
    #geom_smooth(method = "loess", se = TRUE) 
    #+
    #facet_grid(rows = Variable ~ ., scales = "free_y", cols = Year) +
    theme_minimal()
tempVpower

tempGraph <- ggplot(data = subset(data_pivot, Variable =="AvgTemp"), aes(x = Month, y = Value, group = Year, color = Year)) +
    geom_line() +
    labs(title = "Average Temperature by Month", 
    x = "Month", 
    y = "Average Temperature (F)",
    caption = "Sara A.") +
    theme_minimal()
tempGraph

usageGraph <- ggplot(data = subset(data_pivot, Variable =="PowerPerDay"), aes(x = Month, y = Value, group = Year, color = Year)) +
    geom_line() +
    labs(title = "Power Usage by Month", 
    x = "Month", 
    y = "Power Usage (kWh)",
    caption = "Sara A.") +
    theme_minimal()
usageGraph


usageGraph <- ggplot(data = subset(data_pivot, Variable =="PowerPerDay"), aes(x = Month, y = Value, group = Year, color = Year)) +
    geom_line() +
    labs(title = "Power Usage by Month", 
    x = "Month", 
    y = "Power Usage (kWh)",
    caption = "Sara A.") +
    theme_minimal() +
    transition_time(Year) +
    ease_aes('linear')

#err: ! The animation object does not specify a save_animation method    
anim_save(filename = "usageGraphic.gif",   animation = animate(plot = usageGraph, rendered = gifski_renderer()))

#not returning .gif
animate(plot = usageGraph, rendered = gifski_renderer())


anim_save(filename = "usageGraphic.gif", animation = usageGraph)




#plot both together to show comparison
tempGraph/usageGraph

