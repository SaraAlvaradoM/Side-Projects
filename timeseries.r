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
    rename("AvgHighTemp" = "Avg. High Temp") |> #changed name, it was giving me issues
    mutate(StartDate = ymd(StartDate), 
            Year = year(StartDate), 
            Month = month(StartDate)) |> #set up date format for tsibble prep
    #as.numeric(as.character($Kilowatts)) |>
    as_tsibble(index = StartDate,
                key = c(Year, Month))
view(data_tsibble)

#basic plotting of power usage and average high temperature to identify seasonal patterns
p <- ggplot(data = data_tsibble, mapping = aes(x = StartDate, y = Kilowatts)) +
    geom_line() +
    theme_minimal()
p

t <- ggplot(data = data_tsibble, mapping = aes(x = StartDate, y = AvgHighTemp)) +
    geom_line() +
    theme_minimal()
t

#look at doing a facetting or putting one plot on top of another
test <- ggplot(data = data_tsibble, mapping = aes(x = StartDate, y = Kilowatts)) +
    facet_grid()



ggplot(data = data_tsibble, mapping = aes(x = StartDate)) +
    geom_line(aes(y = Kilowatts), color = "blue", linewidth = 0.8) +
    geom_line(aes(y = AvgHighTemp), color = "red", linewidth = 0.8) +
    scale_y_discrete(name = "Power Usage",
        sec.axis = sec_axis(name = "Average Temperature", ~.-)) +
    theme_minimal()
