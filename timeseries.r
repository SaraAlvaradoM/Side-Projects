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


#pull in data on power and create that into a tsibble object.

getwd() #this tells me what working directory I am in
setwd("C:/Users/salva/Desktop/R/Side-Projects") #set the working directory to where the Excel file is located

data_raw <- read_excel("PowerUsageTracker.xlsx")
view(data_raw)


data_tsibble <- data_raw |>
    rename("StartDate" = "Start Date") |>
    rename("Kilowatts" = "Kilowatts/Hour") |> 
    rename("AvgTemp" = "Avg. Avg. Temp") |> #changed name, it was giving me issues
    mutate(StartDate = ymd(StartDate),
            Year = year(StartDate),
            Month = month(StartDate), #set up date format for tsibble prep
            PowerPerDay = Kilowatts/Days) |> #used to adjust for variation in the number of days per month
    as_tsibble(index = StartDate,
                key = c(Year, Month))
view(data_tsibble)

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

#basic plotting of power usage and average high temperature to identify seasonal patterns
p <- ggplot(data = data_tsibble, mapping = aes(x = StartDate, y = Kilowatts)) +
    geom_line() +
    theme_minimal()
p

t <- ggplot(data = data_tsibble, mapping = aes(x = StartDate, y = AvgHighTemp)) +
    geom_line() +
    theme_minimal()
t

#pivot data for ease of plotting multiple variables in one chart
data_pivot <- data_tsibble |>
    select(Kilowatts, AvgTemp, PowerPerDay, Cost, Year, Month) |>
    pivot_longer(cols = c(Kilowatts, AvgTemp, PowerPerDay, Cost), names_to = "Variable", values_to = "Value")
view(data_pivot)

#pivot data for ease of plotting multiple variables in one chart
data_pivot2 <- tibble_test |>
    select(StartDate, Kilowatts, AvgTemp, PowerPerDay, Cost, Year, Month) |>
    pivot_longer(cols = c(Kilowatts, AvgTemp, PowerPerDay, Cost), names_to = "Variable", values_to = "Value")
view(data_pivot2)

tempVpower <- ggplot(data_pivot, aes(x = StartDate, y = Value)) +
    geom_line(data = subset(data_pivot, Variable =="PowerPerDay"), stat = "identity", color = "blue") +
    # get some form of trend line on these two  
    geom_line(data = subset(data_pivot, Variable =="AvgTemp"), stat = "identity", color = "red") +
    #geom_smooth(method = "loess", se = TRUE) 
    #+
    #facet_grid(rows = Variable ~ ., scales = "free_y", cols = Year) +
    theme_minimal()
tempVpower

tempVpower2 <- ggplot(data_pivot2, aes(x = Month, y = Value)) +
    geom_line(data = subset(data_pivot2, Variable =="PowerPerDay"), stat = "identity", color = "blue") +
    # get some form of trend line on these two  
    geom_line(data = subset(data_pivot2, Variable =="AvgTemp"), stat = "identity", color = "red") +
    #geom_smooth(method = "loess", se = TRUE) +
    facet_grid(rows = Year ~ ., scales = "free_y") +
    theme_minimal()
tempVpower2

tempGraph <- ggplot(data = subset(data_pivot2, Variable =="AvgTemp"), aes(x = Month, y = Value, color = Year)) +
    geom_line() +
    theme_minimal()
tempGraph

