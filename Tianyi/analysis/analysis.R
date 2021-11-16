data <- read.csv("DATA/PART1/01.csv")
data <- data[,5]
timeseries <- ts(data, frequency = 12, start = c(2069,12))
timeseriescomponents <- decompose(timeseries)
plot(timeseriescomponents)