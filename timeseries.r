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


#pull in data on power and create that into a tibble object.

getwd() #this tells me what working directory I am in
setwd("C:/Users/salva/Desktop/R/Side-Projects") #set the working directory to where the Excel file is located

data_raw <- read_excel("PowerUsageTracker.xlsx")
view(data_raw)

tibble_test <- data_raw |>
    rename("StartDate" = "Start Date") |>
    rename("Kilowatts" = "Kilowatts/Hour") |>
    rename("AvgTemp" = "Avg. Avg. Temp") |>
    mutate(StartDate = ymd(StartDate), 
            Year = year(StartDate),
            Month = month(StartDate),
            PowerPerDay = Kilowatts/Days,
            CostPerDay = Cost/Days,
            CostPerKilowatt = Cost/Kilowatts) |>
    as_tibble()
view(tibble_test)

#pivot data for ease of plotting multiple variables in one chart
data_pivot2 <- tibble_test |>
    select(StartDate, Kilowatts, AvgTemp, PowerPerDay, Cost, Year, Month) |>
    pivot_longer(cols = c(Kilowatts, AvgTemp, PowerPerDay, Cost), names_to = "Variable", values_to = "Value") |>
    mutate(Year = as.factor(Year)) |> #convert year to factor for plotting
    mutate(Month = as.factor(Month)) #convert month to factor for plotting
view(data_pivot2)

tempVpower <- ggplot(data_pivot2, aes(x = StartDate, y = Value)) +
    geom_line(data = subset(data_pivot, Variable =="PowerPerDay"), stat = "identity", color = "blue") +
    # get some form of trend line on these two  
    geom_line(data = subset(data_pivot2, Variable =="AvgTemp"), stat = "identity", color = "red") +
    #geom_smooth(method = "loess", se = TRUE) 
    #+
    #facet_grid(rows = Variable ~ ., scales = "free_y", cols = Year) +
    theme_minimal()
tempVpower

tempGraph <- ggplot(data = subset(data_pivot2, Variable =="AvgTemp"), aes(x = Month, y = Value, group = Year, color = Year)) +
    geom_line() +
    theme_minimal()
tempGraph

UsageGraph <- ggplot(data = subset(data_pivot2, Variable =="PowerPerDay"), aes(x = Month, y = Value, group = Year, color = Year)) +
    geom_line() +
    theme_minimal()
UsageGraph

tempVsUsageGraph <- 
    filter(data_pivot2ggplot(data = subset(data_pivot2, Variable = c("PowerPerDay", "AvgTemp")), aes(x = Month, y = Value, group = Year, color = Year)) +
    geom_line() +
    facet_grid(rows = Variable ~ ., scales = "free_y") +
    theme_minimal()
tempVsUsageGraph

