## Plots dB recording from a CSV file named road.csv

road.df <- read.csv("road.csv")
road.df <- unfactor(road.df)
road.df <- within(road.df, When <- paste(Date, Time))
road.df <- within(road.df, when <- as.POSIXct(When, format = "%d/%m/%Y %H:%M:%S"))
with(road.df, plot(Average ~ when))
with(road.df, plot(Peak ~ when))
