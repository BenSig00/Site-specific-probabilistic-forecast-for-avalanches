rm(list=ls()) #Clearing all variables
#setwd("C:/Users/benja/OneDrive/Documents/Universitetet/Masteroppgave/Data") #Choosing folder to work in

#Libraries ---------------------------------------------------------------


library(dplyr) #Manipulating data
library(ggplot2) #ggplot package
library(GGally) #Needed for ggplots
library(rpart) #Tree analysis
library(randomForest) #Random forests
library(scoringRules) #Scoring rules
library(gbm) #Boosting package
library(tscount) #PIT
library(stargazer) #Exporting tables to .tex-file
library(xtable) #To export table to latex format
library(tidyr) #To transform data (into a format where multiple lines can be plotted)
library(stringr) #Extracting string
library(caret) #Cross-validation 
library(boot) #Cross-validation
library(class) #Method of nearest neighbours


#Reading data ------------------------------------------------------------


#Date function to retrieve desired dates
datePrep = function(data, dato){
  data = data[dato > as.Date(("2019-12-07")) & dato < as.Date(("2020-06-01")) | 
                dato > as.Date(("2020-12-06")) & dato < as.Date(("2021-06-01")), ]
  return(data)
}


#Avalanche warnings
warningPrep = function(filename){
  warningData = read.csv(filename)
  warningData = warningData[warningData$Region.ID == 3010, ]
  warningData$Dato = as.Date(warningData$Dato)
  warningData = datePrep(warningData, warningData$Dato)
  table(warningData$Faregrad)
  return(warningData)
}
warningData = warningPrep("avalanche-bulletin_2019-12-01--2021-06-01_(2023-10-17_10-46-16)_v1.csv")
table(warningData$Faregrad) #Frequency table


#Weather forecast
weatherPrep = function(filename){
  #Finding the column names
  cnames = read.csv(filename, sep=";", header=FALSE, nrow = 2) 
  cnames = sapply(cnames, paste, collapse = "")
  
  #Reading the data
  weatherData = read.csv(filename, sep = ";", 
                         skip = 2, col.names = cnames)
  
  #Some adjustments
  colnames(weatherData)[c(2,3) ] = c("Date", "Height")
  weatherData[weatherData$precip0 > 1000, c(4:17)] = 0 #Correcting a clearly wrong observation
  
  return(weatherData)
}
weatherData=weatherPrep("all_data.csv")


#Dividing data into the different heights
data000300 = weatherData[weatherData$Height == "0-300", ]
data300600 = weatherData[weatherData$Height == "300-600", -c(4:17, 39:52)]
data600900 = weatherData[weatherData$Height == "600-900", -c(4:17, 39:52)]
data9001200 = weatherData[weatherData$Height == "900-1200", -c(4:17, 39:52)]
data12001500 = weatherData[weatherData$Height == "1200-1500", -c(4:17, 39:52)]

#Function for adding columns with the weather from the last 7 days
add_past = function(data) {
  d = data[, -c(1,2,3) ]
  dayBefore = function(data, label) {
    data_day_before = data
    data_day_before = rbind(NA, data_day_before[-nrow(data_day_before), ])
    names(data_day_before) = paste0(names(d), "_", label)
    
    return(data_day_before)
  }
  weatherData_1 = dayBefore(d, "1")
  weatherData_2 = dayBefore(weatherData_1, "2")
  weatherData_3 = dayBefore(weatherData_2, "3")
  weatherData_4 = dayBefore(weatherData_3, "4")
  weatherData_5 = dayBefore(weatherData_4, "5")
  weatherData_6 = dayBefore(weatherData_5, "6")
  weatherData_7 = dayBefore(weatherData_6, "7")
  
  weatherData = cbind(data, weatherData_1, weatherData_2, weatherData_3, 
                      weatherData_4, weatherData_5, weatherData_6, weatherData_7)
  
  weatherData = datePrep(weatherData, weatherData$Date)
  weatherData = weatherData[, -c(1,2,3)]
  return(weatherData)
}

#Adding the past
data000300 = add_past(data000300)
data300600 = add_past(data300600)
data600900 = add_past(data600900)
data9001200 = add_past(data9001200)
data12001500 = add_past(data12001500)


#Weather from weather station the day before specified data
weatherStationPrep = function(filename) {
  weatherStationData = read.csv(filename, sep=";")
  weatherStationData = head(weatherStationData,-1)
  colnames(weatherStationData) = c("Navn", "Stasjon", "Date", "Snow_depth_1", "Precip_1")
  
  weatherStationData$Precip_1 = as.numeric(gsub(",", ".", weatherStationData$Precip_1))
  weatherStationData$Snow_depth_1 = as.numeric(gsub(",", ".", weatherStationData$Snow_depth_1))
  weatherStationData[is.na(weatherStationData$Snow_depth_1), ]$Snow_depth_1 = 1
  
  weatherStationData$Precip_2 = c(NA, head(weatherStationData$Precip_1,-1))
  weatherStationData$Precip_3 = c(NA, head(weatherStationData$Precip_2,-1))
  weatherStationData$Precip_4 = c(NA, head(weatherStationData$Precip_3,-1))
  weatherStationData$Precip_5 = c(NA, head(weatherStationData$Precip_4,-1))
  weatherStationData$Precip_6 = c(NA, head(weatherStationData$Precip_5,-1))
  weatherStationData$Precip_7 = c(NA, head(weatherStationData$Precip_6,-1))
  
  weatherStationData$Snow_depth_2 = c(NA, head(weatherStationData$Precip_1, -1))
  weatherStationData$Snow_depth_3 = c(NA, head(weatherStationData$Precip_2, -1))
  weatherStationData$Snow_depth_4 = c(NA, head(weatherStationData$Precip_3, -1))
  weatherStationData$Snow_depth_5 = c(NA, head(weatherStationData$Precip_4, -1))
  weatherStationData$Snow_depth_6 = c(NA, head(weatherStationData$Precip_5, -1))
  weatherStationData$Snow_depth_7 = c(NA, head(weatherStationData$Precip_6, -1))
  
  weatherStationData$Date = as.Date(weatherStationData$Date, format = "%d.%m.%Y") + 1 #+1 to shift the data
  weatherStationData = datePrep(weatherStationData, weatherStationData$Date)
  return(weatherStationData[,-c(1, 2, 3)])
}
weatherStationData = weatherStationPrep("table.csv") #Data retrieved are from 30.11-30-05 due to shift


#Satellite data
satellitePrep = function(file1, file2) {
  satelliteData = rbind(read.csv(file1, sep = ";"), read.csv(file2, sep = ";"))
  satelliteData$Direction = "None"
  satelliteData[satelliteData$eksposisjonUtlopsomr >= 45 & satelliteData$eksposisjonUtlopsomr < 135, ]$Direction = "East"
  satelliteData[satelliteData$eksposisjonUtlopsomr >= 135 | satelliteData$eksposisjonUtlopsomr < -135, ]$Direction = "South"
  satelliteData[satelliteData$eksposisjonUtlopsomr >= -135 & satelliteData$eksposisjonUtlopsomr < -45, ]$Direction = "West"
  satelliteData[satelliteData$eksposisjonUtlopsomr >= -45 & satelliteData$eksposisjonUtlopsomr < 45, ]$Direction = "North"
  
  satelliteData$estimatedDate = as.POSIXct(satelliteData$estimatedDate, format = "%Y-%m-%d %H:%M:%S", tz = Sys.timezone())
  satelliteData$Date = as.Date(satelliteData$estimatedDate)
  
  return(satelliteData)
}
satelliteData = satellitePrep("satelittdata19-20.csv","satelittdata20-21.csv")
satelliteData = satelliteData[satelliteData$regStatus != "Godkjent kvalitet C", ]
satelliteDataWest = satelliteData[satelliteData$Direction == "West", ]


#Avalanches hitting road in Holmbuktura
avalanchePrep = function(filename){
  avalancheData = read.csv(filename, sep=";")
  avalancheData$Date = as.Date(avalancheData$start_time)
  return(avalancheData)
}
avalancheData = avalanchePrep("holmbuktura.csv")

#Data from danger zone
dz_avalancheData = avalancheData[grepl("DZ",avalancheData$affected_regions), ]
dz_avalancheData


#All data
dataPrep = function(data){
  
  #Adding the different types of detections
  addDetections = function(Detections, detections_data, lag){
    detections_data = datePrep(detections_data, detections_data$Date+lag)
    data[[Detections]] = 0
    for (i in 1:nrow(detections_data)){
      dato = detections_data[i,]["Date"]
      data[data$Dato == dato+lag, ][[Detections]] = data[data$Dato == dato+lag, ][[Detections]] + 1
    }
    return(data)
  }
  
  #Adding responses
  data = addDetections("radarDetections", avalancheData, 0)
  data = addDetections("radarDetectionsDZ", dz_avalancheData, 0)
  
  #Adding lagged values as explanatory variables
  for (i in (1:7)){
    data = addDetections(paste0("radarDetections_", i), avalancheData, i)
    data = addDetections(paste0("radarDetectionsDZ_", i), dz_avalancheData, i)
    data = addDetections(paste0("satDetections_", i), satelliteData, i)
    data = addDetections(paste0("satDetectionsWest_", i), satelliteDataWest, i)
  }
  
  #Function for adding weather#
  addWeather = function(dataAdd, label) {
    names(dataAdd) = paste0(names(dataAdd), "_", label)
    data = cbind(data, dataAdd)
    return(data)
  }
  
  #Adding weather
  data = addWeather(data000300, "150")
  data = addWeather(data300600, "450")
  data = addWeather(data600900, "750")
  data = addWeather(data9001200, "1050")
  data = addWeather(data12001500, "1350")
  data = addWeather(weatherStationData, "")
  
  data = subset(data, select = -c(Varslingsregioner, Region.ID, Faregrad) )
  return(data)
}

#All data
data_all = dataPrep(warningData)

#Summing all detections
sum(data_all$radarDetections)
sum(data_all$radarDetectionsDZ)


#Data exploration -------------------------------------------------------------------------------------------------------------

#setwd("C:/Users/benja/OneDrive/Documents/Universitetet/Masteroppgave/Figures")

#Correlation between explanatory variables
d = ggpairs(data_all[c("precip50_150", "temp50_150", "wind50_150", 
                       "new_snow50_150", "snow_depth50_150", "radarDetections", 
                       "satDetections_1", "satDetectionsWest_1")])
d
ggsave("ggpairs.png", width = 3.45*4, height = 2.65*4)


#ACF

#Opening a PNG graphics device
png("ACF_avalanches.png", width = 600, height = 450)

#Plot
acf(data_all$radarDetections, type = "correlation", 
    plot = TRUE, main="ACF of number of avalanches")

# Closing the device to save the plot
dev.off()

#Dividing data into winter seasons
data_all_1920 = data_all[data_all$Dato < as.Date("2020-08-01"), ]
data_all_2021 = data_all[data_all$Dato > as.Date("2020-08-01"), ]

#Time series plots

#Radar Holmbuktura
ggplot(data_all_1920, aes(x = Dato, y = radarDetections)) +
  geom_point() + 
  labs(x = "Date", y = "Radar detections", title = "Detections over time in Holmbuktura in winter 2019/2020") +
  theme_minimal() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b")
ggsave("detections_over_time_Holm_1920.png")

ggplot(data_all_2021, aes(x = Dato, y = radarDetections)) +
  geom_point() + 
  labs(x = "Date", y = "Radar detections", title = "Detections over time in Holmbuktura in winter 2020/2021") +
  theme_minimal() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b")
ggsave("detections_over_time_Holm_2021.png")

#Satellites Lavangsdalen
ggplot(data_all_1920, aes(x = Dato, y = satDetections_1)) +
  geom_point() + 
  labs(x = "Date", y = "Satellite detections", title = "Detections over time in Lavangsdalen winter 2019/2020 ") +
  theme_minimal() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b")
ggsave("detections_over_time_Lav_1920.png")

ggplot(data_all_2021, aes(x = Dato, y = satDetections_1)) +
  geom_point() + 
  labs(x = "Date", y = "Satellite detections", title = "Detections over time in Lavangsdalen winter 2020/2021 ") +
  theme_minimal() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b")
ggsave("detections_over_time_Lav_2021.png")

#Precipitation time series (only quantiles)
ts_precip = function(columns, filename, ylab, data){
  d = data
  
  # Transform it to plotable format
  d = pivot_longer(d, 
                   col = columns, 
                   names_to = "Series", values_to = "Value")
  d$Series = factor(str_extract(d$Series, "\\d+"),
                     levels = c("0", "5", "25", "50", "75", "95", "100"),
                     labels = c("0", "0.05", "0.25", "0.50", "0.75", "0.95", "1"))
  
  # Create the ggplot
  plot = ggplot(data = d, aes(x = Dato-7, y = Value, color = Series)) + 
    geom_line() +
    theme_minimal() + 
    labs(title = "Time series plot", x = "Date", y = ylab, color = "Quantile") +
    scale_color_viridis_d() 
  print(plot)
  ggsave(filename, width = 5.12, height = 4)
}
ts_precip(c("precip0_7_150", "precip5_7_150", "precip25_7_150", 
            "precip50_7_150", "precip75_7_150", "precip95_7_150", 
            "precip100_7_150"), 
          "time_series_precip_w1.png", "Precipitation (mm)", 
          data_all_1920[c(1:7), ])
ts_precip(c("precip_max0_7_150", "precip_max5_7_150", "precip_max25_7_150", 
            "precip_max50_7_150", "precip_max75_7_150", "precip_max95_7_150", 
            "precip_max100_7_150"), 
          "time_series_precip_max_w1.png", "Precipitation (mm)", 
          data_all_1920[c(1:7), ])
ts_precip(c("new_snow0_7_150", "new_snow5_7_150", "new_snow25_7_150", 
            "new_snow50_7_150", "new_snow75_7_150", "new_snow95_7_150", 
            "new_snow100_7_150"), 
          "time_series_new_snow_m1.png", "New snow (mm)", 
          data_all_1920[c(1:31), ])
ts_precip(c("new_snow_max0_7_150", "new_snow_max5_7_150", "new_snow_max25_7_150", 
            "new_snow_max50_7_150", "new_snow_max75_7_150", "new_snow_max95_7_150", 
            "new_snow_max100_7_150"), 
          "time_series_new_snow_max_m1.png", "Max New Snow (mm)", 
          data_all_1920[c(1:31), ])

#Rest of the weather 
ts_weather = function(columns, filename, ylab, data){
  d = data
  
  
  # Transform it to plotable format
  d = pivot_longer(d, 
                   col = columns, 
                   names_to = "Series", values_to = "Value")
  d$Series = factor(str_extract(d$Series, "\\d+.*"),
                     levels = c("0_7_150", "50_7_150", "100_7_150", "0_7_1350", "50_7_1350", "100_7_1350"),
                     labels = c("0_0m", "50_0m", "100_0m", "0_1500m", "50_1500m", "100_1500m"))
  
  # Create the ggplot
  plot = ggplot(data = d, aes(x = Dato-7, y = Value, color = Series)) + 
    geom_line() +
    theme_minimal() + 
    labs(title = "Time series plot", x = "Date", y = ylab, color = "Quantile_Height") +
    scale_color_viridis_d()
  print(plot)
  ggsave(filename, width = 5.12, height = 4)
}
ts_weather(c("temp0_7_150", "temp50_7_150", "temp100_7_150", 
             "temp0_7_1350", "temp50_7_1350", "temp100_7_1350"), 
           "time_series_temp_w1.png", "Temperature (C)", 
           data_all_1920[c(1:7), ])
ts_weather(c("wind0_7_150", "wind50_7_150", "wind100_7_150", 
             "wind0_7_1350", "wind50_7_1350", "wind100_7_1350" ), 
           "time_series_wind_w1.png", "Wind (m/s)", 
           data_all_1920[c(1:7), ])
ts_weather(c("snow_depth0_7_150", "snow_depth50_7_150", "snow_depth100_7_150", 
             "snow_depth0_7_1350", "snow_depth50_7_1350", "snow_depth100_7_1350"), 
           "time_series_snow_depth_y1.png", "Snow depth (mm)", 
           data_all_1920)


#Wind 0 all season
ts_weather(c("wind0_7_150"), "time_series_wind0_150_y1.png", "Wind (m/s)", data_all_1920)


#setwd("C:/Users/benja/OneDrive/Documents/Universitetet/Masteroppgave/Tables")


#Frequency tables --- -- ---- ---------------------------------------- -- -- --- --- -- -


#Avalanches in Holmbuktura

#Creating data frame
d = as.data.frame(table(data_all$radarDetections))
as.data.frame(table(data_all$radarDetectionsDZ))


#Changing column names
names(d) = c("Number of avalanches", "Frequency")

#Total number of avalanches
t(as.integer(as.character(d$`Number of avalanches`)))%*%d$Frequency #Number of avalanches

#To .tex-file
stargazer(t(d), type = "latex", 
          title = "Frequency table of radar observations of avalanches per day, from 8th December 2019, to 30th May 2020, and from 8th December 2020, to 30th May 2021", 
          header = FALSE, summary = FALSE, out = "freq_aval.tex", label="tab:freq_aval")


#Avalanches in Lavangsdalen


#Total number of avalanches
sum(data_all$satDetectionsWest_1)
sum(data_all$satDetections_1)

#Creating data frame of frequencies
d = as.data.frame(table(data_all$satDetections_1))
names(d) = c("Number of avalanches", "Frequency")

#Creating new data frame with frequencies of intervals
d = data_all
table(d$satDetections_1)
d[d$satDetections_1 >= 5 & d$satDetections_1 < 10, ]$satDetections_1 = 5
d[d$satDetections_1 >= 10 & d$satDetections_1 < 20, ]$satDetections_1 = 10
d[d$satDetections_1 >= 20, ]$satDetections_1 = 20
d = as.data.frame(table(d$satDetections_1))
names(d) = c("Number of avalanches", "Frequency")
t(as.integer(as.character(d$`Number of avalanches`)))%*%d$Frequency
d$`Number of avalanches` = as.character(d$`Number of avalanches`)
d[d$`Number of avalanches` == "5", ]$`Number of avalanches` = "5-9"
d[d$`Number of avalanches` == "10", ]$`Number of avalanches` = "10-19"
d[d$`Number of avalanches` == "20", ]$`Number of avalanches` = "20+"

#Showing frequency table
d

#Exporting .tex-file
stargazer(t(d), type = "latex", 
          title = "Frequency table of satellite observations of avalanches per day, from 30th November 2019, to 30th May 2020, and from 30th November 2020, to 30th May 2021", 
          header = FALSE, summary = FALSE, out = "freq_aval_sat.tex", label="tab:freq_aval_sat")


#Creating models using full set of explanatory variables ------------------------------------------------------------------
#setwd("C:/Users/benja/OneDrive/Documents/Universitetet/Masteroppgave/Figures")

set.seed(1)

#Datasets in use in analysis
data = subset(data_all, select = -c(1,3))
data_dz = subset(data_all, select = -c(1,2))

#Model matrix for analysis with all data
mod_mat = model.matrix(~ ., data[, -1])


#Tree --- -- -- -- --- -- -- - ----- -- -- -- -- - ---- -- -- - -- - -- -- -


#Ordinary decision tree


#Training the decision tree model
mod_tree = rpart(radarDetections ~ ., data = data)

#Opening a PNG graphics device
png("tree.png", width = 800, height = 600)

#Plot
plot(mod_tree, uniform = FALSE)
text(mod_tree)

#Saving plot
dev.off()

#Fitting danger zone model
mod_tree_dz = rpart(radarDetectionsDZ ~ ., data = data_dz)

#Opening a PNG graphics device
png("tree_dz.png", width = 800, height = 600)

#Plot
plot(mod_tree_dz, uniform = FALSE)
text(mod_tree_dz)

#Saving plot
dev.off()

#Pruning the tree using cross-validation
cv_tree = train(x = data[, -1], y = data$radarDetections, method = "rpart", trControl = trainControl(method = "cv", number = 5))

#Fitting pruned tree
mod_tree_pruned = prune(mod_tree, cp = as.numeric(cv_tree$bestTune))

#Opening a PNG graphics device
png("tree_pruned.png", width = 800, height = 600)

#Plotting the pruned tree
plot(mod_tree_pruned)
text(mod_tree_pruned)

#Saving the plot
dev.off()


#Random forest


#Cross-validation random forest
cv_rf = train(x = data[, -1], y = data$radarDetections, method = "rf", trControl = trainControl(method = "cv", number = 5)) #Computational heavy, only run when necassery
print(cv_rf) #Using this to choose mtry

#Fitting random forest
mod_rf = randomForest(radarDetections ~ ., data = data, mtry = 2, ntree = 1000, importance = TRUE)
#varImpPlot(mod_rf, type = 2, pch = 20)

#Variance importance
sort(importance(mod_rf)[, 2], decreasing = TRUE)

#Function for plotting relevant information
plotRelInf = function(var_imp, measure, boosting){
  if (boosting == TRUE){
    var_imp_df = as.data.frame(var_imp[1:40, ]) 
  }
  else {
    var_imp_df = var_imp
  }
  var_imp_df$Variable = rownames(var_imp_df)
  
  # Plotting time
  ggplot(var_imp_df, aes(x = reorder(Variable, rel.inf), y = rel.inf)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() +  # Flips the axes to make it horizontal; easier to read
    theme_minimal() +
    labs(title = "Variable Importance", x = "Variables", y = measure)
}

#Opening a PNG graphics device
png("varimp_rf.png", width = 800, height = 600)

#Plot
plotRelInf(var_imp = data.frame(rel.inf = sort(importance(mod_rf)[, 2], decreasing = TRUE)[1:40]),
           "Increase in node purity", FALSE )

#Saving plot
dev.off()


#Boosting


#Cross-validation boosting
cv_boost = train(x = data[, -1], y = data$radarDetections, method = "gbm", distribution = "poisson", trControl = trainControl(method = "cv", number = 5))
print(cv_boost) #Using this to choose parameters

#Fitting boosted model
mod_boost = gbm(radarDetections ~ ., data = data, n.trees = 50, interaction.depth = 2, distribution = "poisson")
d = summary(mod_boost, plotit = FALSE)

#Opening a PNG graphics device
png("varimp_boost.png", width = 800, height = 600)

#Plot
plotRelInf(summary(mod_boost, plotit = FALSE),
           "Relative influence", TRUE)

#Saving plot
dev.off()


#Nearest neighbours --- -- --- -- -- -- -- --- -- -- --- -- -- -- -- --- -- -- --- -- ---- --- -- -


data_scaled = predict(preProcess(data[, -1], method = "scale"), data[, -1])

cv_knn = train(x = data_scaled[, -1], y = data$radarDetections, method = "knn", tuneGrid = expand.grid(k = 1:20), trControl = trainControl(method = "cv", number = 5))
knn_2 = train(x = data_scaled[, -1], y = data$radarDetections, method = "knn", tuneGrid = expand.grid(k = 2), trControl = trainControl(method = "cv", number = 5))
knn_5 = train(x = data_scaled[, -1], y = data$radarDetections, method = "knn", tuneGrid = expand.grid(k = 6), trControl = trainControl(method = "cv", number = 5))

print(cv_knn)
print(knn_2)
print(knn_5)

predictions_best = predict(cv_knn, data_scaled)
predictions_2 = predict(knn_2, data_scaled)
predictions_5 = predict(knn_5, data_scaled)


#Creating models using reduced set of explanatory variables ------------------------------------------------------------------

set.seed(123)


#Reduced set of explanatory variables
data_small = subset(data_all, select = c("radarDetections", "precip_max0_150", "new_snow100_150", "temp95_1_750", "wind100_750", "Precip_1_", "snow_depth25_750"))

#Name change to please stargazer
colnames(data_small)[6] = "Precip_1"

#Model matrix for analysis with all data
mod_mat_small = model.matrix(~ ., data_small[, -1])

#Detections hitting road
data_dz_small = subset(data_all, select = c("radarDetectionsDZ", "precip_max0_150", "new_snow100_150", "temp95_1_750", "wind100_750", "Precip_1_", "snow_depth25_750"))

#Name change to please stargazer
colnames(data_dz_small)[6] = "Precip_1"


#Tree --- -- -- -- --- -- -- - ----- -- -- -- -- - ---- -- -- - -- - -- -- -


#Ordinary decision tree


#Training the decision tree model
mod_tree_small = rpart(radarDetections ~ ., data = data_small)

#Opening a PNG graphics device
png("tree_small.png", width = 800, height = 600)

#Plot
plot(mod_tree_small, uniform = FALSE)
text(mod_tree_small)

#Saving plot
dev.off()

#Fitting danger zone model
mod_tree_dz_small = rpart(radarDetectionsDZ ~ ., data = data_dz_small)

#Opening a PNG graphics device
png("tree_dz_small.png", width = 800, height = 600)

#Plot
plot(mod_tree_dz_small, uniform = FALSE)
text(mod_tree_dz_small)

#Saving plot
dev.off()

#Pruning the tree using cross-validation
cv_tree_small = train(x = data_small[, -1], y = data_small$radarDetections, method = "rpart", trControl = trainControl(method = "cv", number = 5))

#Fitting pruned tree
mod_tree_pruned_small = prune(mod_tree_small, cp = as.numeric(cv_tree_small$bestTune[1]))

#Opening a PNG graphics device
png("tree_pruned_small.png", width = 800, height = 600)

#Plotting the pruned tree
plot(mod_tree_pruned_small)
text(mod_tree_pruned_small, cex = 1.0)

#Saving the plot
dev.off()


#Random forest


#Cross-validation random forest
cv_rf_small = train(x = data_small[, -1], y = data_small$radarDetections, method = "rf", trControl = trainControl(method = "cv", number = 5))
print(cv_rf_small) #Using this to choose mtry

#Fitting random forest
mod_rf_small = randomForest(radarDetections ~ ., data = data_small, mtry = 2, ntree = 1000, importance = TRUE)

#Function for plotting relevant information
plotRelInfSmall = function(var_imp, measure, boosting){
  if (boosting == TRUE){
    var_imp_df = as.data.frame(var_imp) 
  }
  else {
    var_imp_df = var_imp
  }
  var_imp_df$Variable = rownames(var_imp_df)
  
  # Plotting time
  ggplot(var_imp_df, aes(x = reorder(Variable, rel.inf), y = rel.inf)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() +  # Flips the axes to make it horizontal; easier to read
    theme_minimal() +
    labs(title = "Variable Importance", x = "Variables", y = measure)
}

#Opening a PNG graphics device
png("varimp_rf_small.png", width = 800, height = 600)

#Plot
plotRelInfSmall(var_imp = data.frame(rel.inf = sort(importance(mod_rf_small)[, 2], decreasing = TRUE)),
           "Increase in node purity", FALSE )

#Saving plot
dev.off()


#Boosting


#Cross-validation boosting
cv_boost_small = train(x = data_small[, -1], y = data_small$radarDetections, method = "gbm", distribution = "poisson", trControl = trainControl(method = "cv", number = 5))
print(cv_boost_small) #Using this to choose parameters

#Fitting boosted model
mod_boost_small = gbm(radarDetections ~ ., data = data_small, n.trees = 50, distribution = "poisson")

#Model summary
summary(mod_boost_small, plotit = FALSE)

#Opening a PNG graphics device
png("varimp_boost_small.png", width = 800, height = 600)

#Plot
plotRelInfSmall(summary(mod_boost_small, plotit = FALSE),
           "Relative influence", TRUE)

#Saving plot
dev.off()


#Nearest neighbours --- -- --- -- -- -- -- --- -- -- --- -- -- -- -- --- -- -- --- -- ---- --- -- -


data_scaled_small = predict(preProcess(data_small[, -1], method = "scale"), data_small[, -1])

cv_knn_small = train(x = data_scaled_small[, -1], y = data_small$radarDetections, method = "knn", tuneGrid = expand.grid(k = 1:20), trControl = trainControl(method = "cv", number = 5))
knn_2_small = train(x = data_scaled_small[, -1], y = data_small$radarDetections, method = "knn", tuneGrid = expand.grid(k = 2), trControl = trainControl(method = "cv", number = 5))
knn_5_small = train(x = data_scaled_small[, -1], y = data_small$radarDetections, method = "knn", tuneGrid = expand.grid(k = 6), trControl = trainControl(method = "cv", number = 5))

print(cv_knn_small)
print(knn_2_small)
print(knn_5_small)

predictions_best_small = predict(cv_knn_small, data_scaled_small)
predictions_2_small = predict(knn_2_small, data_scaled_small)
predictions_5_small = predict(knn_5_small, data_scaled_small)


#GLM-- -- --- --- --- -- --- -- -- - --- -- --- -- -- -- - -- -- -- - -- - - - - - -- - --  -- -- - -- - - --


#setwd("C:/Users/benja/OneDrive/Documents/Universitetet/Masteroppgave/Tables")

#Fitting simple GLM
mod_glm = glm(radarDetections ~ ., family = poisson(link = log), data = data_small)

#Summary
summary(mod_glm)

#Exporting to latex
stargazer(t(round(summary(mod_glm)$coefficients[, 4], 3)), out = "table_glm.tex", label = "tab:pvals_GLM1", title = "P-values of the simple GLM")
          
#Fitting glm for dz
mod_glm_dz = glm(radarDetectionsDZ ~ ., family = poisson(link = log), data = data_dz_small)

#Summary
summary(mod_glm_dz)

#Exporting to latex
stargazer(t(round(summary(mod_glm_dz)$coefficients[, 4], 3)), out = "table_glm_dz.tex")

#Fitting GLM with interaction terms
mod_glm_2 = glm(radarDetections ~ .^2, data = data_small)

#Summary
summary(mod_glm_2)

#Exporting to latex
stargazer(t(round(summary(mod_glm_2)$coefficients[, 4], 3)), out = "table_glm_2.tex", label = "tab:pvals_GLM2", title = "P-values of the GLM with interaction terms")

#Fitting GLM with polynomials (gam is dropped because of train not being too compatible)
mod_glm_3 = glm(radarDetections ~ poly(precip_max0_150, 3) + poly(new_snow100_150, 3) + poly(temp95_1_750, 3) + poly(Precip_1, 3) + poly(snow_depth25_750, 3) + poly(wind100_750, 3), family = poisson(link = log), data = data_small)

#Summary
summary(mod_glm_3)

#Exporting to latex
stargazer(t(round(summary(mod_glm_3)$coefficients[, 4], 3)), out = "table_glm_3.tex", label = "tab:pvals_GLM3", title = "P-values of the GLM with polynomials")


#Ensembled model-- -- -- -- -- -- - -- -- -- -- -- - -- -- - -- -- - -- -- -- -- -- - -- - -- -- - -- - -- - -- -- - - -- -- - --- - -- -- - - -- - -- - - - -- -- - - -- -- - - - - - --- -- - - -- - - -- - - - - - -- - 


#Creating dataset with predictions from chosen models 
data_ensembled = data.frame(radarDetections = data_small$radarDetections,
                           glm_pred = exp(predict(mod_glm, newdata = data_small)),
                           rf_pred = predict(mod_rf_small, newdata = data_small),
                           knn_pred = predictions_best_small)

#Fitting ensembled model
starting_values = c(0, rep(1/(ncol(data_ensembled)-1), ncol(data_ensembled)-1)) #Needed in glm
mod_ensembled = glm(radarDetections ~ ., family = poisson(link = identity), data = data_ensembled, start = starting_values)

#Summary of ensembled model
summary(mod_ensembled)

#Exporting to latex
stargazer(round(summary(mod_ensembled)$coefficients[, 4], 3), out = "table_ensembled.tex")


#RF GLM -- -- -- -- -- -- - -- -- -- -- -- - -- -- - -- -- - -- -- -- -- -- - -- - -- -- - -- - -- - -- -- - - -- -- - --- - -- -- - - -- - -- - - - -- -- - - -- -- - - - - - --- -- - - -- - - -- - - - - - -- - 


#Creating dataset with predictions from rf glm 
data_rf = data.frame(radarDetections = data_small$radarDetections,
                            rf_pred = predict(mod_rf_small, newdata = data_small))

#Fitting rf glm model
starting_values_rf = c(0, rep(1/(ncol(data_rf)-1), ncol(data_rf)-1)) #Needed in glm
mod_rf_glm = glm(radarDetections ~ ., family = poisson(link = identity), data = data_rf, start = starting_values_rf)


#Summary of rf glm model
summary(mod_rf_glm)

#Exporting to latex
stargazer(round(summary(mod_rf_glm)$coefficients[, 4], 3), out = "table_rf_glm.tex")


#Evaluating models using full set of explanatory variables ------------------------------------------------------------------------------


#setwd("C:/Users/benja/OneDrive/Documents/Universitetet/Masteroppgave/Tables")


#Using scoring rules, smaller crps is better ---- --- -- - --- -- - -- -- -- - -- -- - -- -- -- -- -- - --- -- -- --- - - -- -- - --- - - - - -- - -- - 


#Trees


#CRPS for pruned tree
crps_pruned = crps_norm(data$radarDetections, mean = as.numeric(predict(mod_tree_pruned, newdata = data)), sd = 0)
mean(crps_pruned)

#CRPS for random forest
crps_rf = crps_norm(data$radarDetections, mean = as.numeric(predict(mod_rf, newdata = data)), sd = 0)
mean(crps_rf)

#CRPS for boosted model
crps_boosting = crps_norm(data$radarDetections, mean = as.numeric(exp(predict(mod_boost, newdata = data))), sd = 0)
mean(crps_boosting)

#Creating list of CRPS
list_crps_trees=c(mean(crps_pruned), mean(crps_rf), mean(crps_boosting))
names(list_crps_trees) = c("CRPS pruned tree", "CRPS random forest", "CRPS boosted tree")

#Exporting to latex
stargazer(list_crps_trees, out = "table_trees_CRPS.tex", label = "tab:trees_CRPS", title = "CRPS for tree models fitted using $X_{\text{full}}$")


#Neighbours


#2NN
crps_N2 = crps_norm(data$radarDetections, predictions_2, sd = 0)
mean(crps_N2)

#5NN
crps_N5 = crps_norm(data$radarDetections, predictions_5, sd = 0)
mean(crps_N5)

#BestNN
crps_Nbest = crps_norm(data$radarDetections, predictions_best, sd = 0)
mean(crps_Nbest)

#Creating list
list_crps_nn = c(mean(crps_N2), mean(crps_N5), mean(crps_Nbest))
names(list_crps_nn) = c("2 nearest neighbours", "5 nearest neighbours", 
                              "9 nearest neighbours")

#Exporting to latex
stargazer(list_crps_nn, out = "table_KNN_CRPS.tex", label = "tab:KNN_CRPS",
          title = "CRPS of models fitted with the method of nearest neighbours using $X_{full}$")


#Evaluating models using Cross-validation -- -- -- --- - -- -- - -- -- -- -- -- -- -- - -- -- - -- - - - - -- - -- - --- --- --


#Trees


#Result trees
print(min(cv_tree$results$RMSE))
print(min(cv_rf$results$RMSE))
print(min(cv_boost$results$RMSE))

#Creating list
list_RMSE_trees = c(min(cv_tree$results$RMSE), min(cv_rf$results$RMSE), min(cv_boost$results$RMSE))
names(list_RMSE_trees) = c("RMSE pruned tree", "RMSE random forest", "RMSE boosted tree")

#Exporting to latex
stargazer(list_RMSE_trees, out = "table_trees_RMSE.tex", label = "tab:RMSE_trees", title = "Root mean squared 5-fold cross-validation error for tree models fitted using $X_{\text{full}}$")


#Neighbours


#Result Nearest neighbours
print(min(cv_knn$results$RMSE))
print(min(knn_2$results$RMSE))
print(min(knn_5$results$RMSE))

#Creating list
list_RMSE_nn = c(cv_knn$results$RMSE[2], cv_knn$results$RMSE[5], min(cv_knn$results$RMSE))
names(list_RMSE_nn) = c("2 nearest neighbours", "5 nearest neighbours", "9 nearest neighbours")

#Exporting to latex
stargazer(list_RMSE_nn, out = "table_nn_RMSE.tex", label = "tab:RMSE_nn", title = "Root mean squared 5-fold cross-validation error of models fitted with the method of nearest neighbours using $X_{full}$")


#Predictions of different days -- -- -- -- --- -- -- --- --- -- -- -- -- -- -- --- -- -- --- - --- - --- - -- --- -- -- -- -- - -- -- -- - - - - -


#Prediction of day with 3 avalanches detected


#Extracting the day
data_3aval = data[26, ] 
data_3aval_ensembled = data_ensembled[26, ]
data[26, ]$radarDetections

#Tree predictions
exp(predict(mod_boost, newdata = data_3aval))
predict(mod_rf, newdata = data_3aval)
predict(mod_tree, newdata = data_3aval)

#List of tree predictions
list_3aval_trees = c(predict(mod_tree, newdata = data_3aval), predict(mod_rf, newdata = data_3aval), exp(predict(mod_boost, newdata = data_3aval)))
names(list_3aval_trees) = c("Pruned tree", "Random forest", "Boosted model")

#Exporting to latex
stargazer(list_3aval_trees, out = "table_trees_3aval.tex", label = "tab:trees_3aval", 
          title = "Prediction of tree models fittted using $X_{full}$ on a day with 3 avalanches detected")

#Predictions using method of nearest neighbours
predictions_2[26]
predictions_5[26]
predictions_best[26]

#List of KNN predictions
list_3aval_knns = c(predictions_2[26], predictions_5[26], predictions_best[26])
names(list_3aval_knns) = c("Model using 2 nearest neighbours", "Model using 5 nearest neighbours", "Model using 9 nearest neighbours")

#Exporting to latex
stargazer(list_3aval_knns, out = "table_knn_3aval.tex", label = "tab:knn_3aval", 
          title = "Prediction of models fitted using the method of nearest neighbour and $X_{full}$ on a day with 3 avalanches detected")

#Prediction of day with 1 avalanches detected


#Extracting the day
data_1aval = data[37, ] 
data_1aval_ensembled = data_ensembled[37, ]
data[37, ]$radarDetections

#Tree predictions
exp(predict(mod_boost, newdata = data_1aval))
predict(mod_rf, newdata = data_1aval)
predict(mod_tree, newdata = data_1aval)

#List of tree predictions
list_1aval_trees = c(predict(mod_tree, newdata = data_1aval), predict(mod_rf, newdata = data_1aval), exp(predict(mod_boost, newdata = data_1aval)))
names(list_1aval_trees) = c("Pruned tree", "Random forest", "Boosted model")

#Exporting to latex
stargazer(list_1aval_trees, out = "table_trees_1aval.tex", label = "tab:trees_1aval", 
          title = "Prediction of tree models fitted using $X_{full}$ on a day with 1 avalanche detected")

#Predictions using method of nearest neighbours
predictions_2[37]
predictions_5[37]
predictions_best[37]

#List of KNN predictions
list_1aval_knns = c(predictions_2[37], predictions_5[37], predictions_best[37])
names(list_1aval_knns) = c("Model using 2 nearest neighbours", "Model using 5 nearest neighbours", "Model using 9 nearest neighbours")

#Exporting to latex
stargazer(list_1aval_knns, out = "table_knn_1aval.tex", label = "tab:knn_1aval", 
          title = "Prediction of models fitted using the method of nearest neighbours and $X_{full}$ on a day with 1 avalanche detected")


#Prediction of day with 0 avalanches detected


#Extracting the day
data_0aval = data[4, ] 
data_0aval_ensembled = data_ensembled[4, ]
data[4, ]$radarDetections

#Tree predictions
exp(predict(mod_boost, newdata = data_0aval))
predict(mod_rf, newdata = data_0aval)
predict(mod_tree, newdata = data_0aval)

#List of tree predictions
list_0aval_trees = c(predict(mod_tree, newdata = data_0aval), predict(mod_rf, newdata = data_0aval), exp(predict(mod_boost, newdata = data_0aval)))
names(list_0aval_trees) = c("Pruned tree", "Random forest", "Boosted model")

#Exporting to latex
stargazer(list_0aval_trees, out = "table_trees_0aval.tex", label = "tab:trees_0aval", 
          title = "Prediction of tree models fitted using $X_{full}$ on a day with 0 avalanches detected")

#Predictions using method of nearest neighbours
predictions_2[4]
predictions_5[4]
predictions_best[4]

#List of KNN predictions
list_0aval_knns = c(predictions_2[4], predictions_5[4], predictions_best[4])
names(list_0aval_knns) = c("Model using 2 nearest neighbours", "Model using 5 nearest neighbours", "Model using 9 nearest neighbours")

#Exporting to latex
stargazer(list_0aval_knns, out = "table_knn_0aval.tex", label = "tab:knn_0aval", 
          title = "Predictions by models fitted using method of nearest neighbours and $X_{full}$ on a day with 0 avalanches detected")


#Evaluating models using reduced set of explanatory variables ------------------------------------------------------------------------------


#setwd("C:/Users/benja/OneDrive/Documents/Universitetet/Masteroppgave/Tables")


#Using scoring rules, smaller crps is better ---- --- -- - --- -- - -- -- -- - -- -- - -- -- -- -- -- - --- -- -- --- - - -- -- - --- - - - - -- - -- - 


#Trees


#CRPS for pruned tree
crps_pruned_small = crps_norm(data_small$radarDetections, mean = as.numeric(predict(mod_tree_pruned_small, newdata = data_small)), sd = 0)
mean(crps_pruned_small)

#CRPS for random forest
crps_rf_small = crps_norm(data_small$radarDetections, mean = as.numeric(predict(mod_rf_small, newdata = data_small)), sd = 0)
mean(crps_rf_small)

#CRPS for boosted model
crps_boosting_small = crps_norm(data_small$radarDetections, mean = as.numeric(exp(predict(mod_boost_small, newdata = data_small))), sd = 0)
mean(crps_boosting_small)

#Creating list of CRPS
list_crps_trees_small = c(mean(crps_pruned_small), mean(crps_rf_small), mean(crps_boosting_small))
names(list_crps_trees_small) = c("CRPS pruned tree", "CRPS random forest", "CRPS boosted tree")

#Exporting to latex
stargazer(list_crps_trees_small, float = FALSE, out = "table_trees_CRPS_small.tex", label = "tab:trees_CRPS_small", title = "CRPS for tree models fitted using $X_{reduced}$")


#Neighbours


#2NN
crps_N2_small = crps_norm(data_small$radarDetections, predictions_2_small, sd = 0)
mean(crps_N2_small)

#5NN
crps_N5_small = crps_norm(data_small$radarDetections, predictions_5_small, sd = 0)
mean(crps_N5_small)

#BestNN
crps_Nbest_small = crps_norm(data_small$radarDetections, predictions_best_small, sd = 0)
mean(crps_Nbest_small)

#Creating list
list_crps_nn_small = c(mean(crps_N2_small), mean(crps_N5_small), mean(crps_Nbest_small))
names(list_crps_nn_small) = c("2 nearest neighbours", "5 nearest neighbours", 
                        "20 nearest neighbours")

#Exporting to latex
stargazer(list_crps_nn_small, out = "table_KNN_CRPS_small.tex", label = "tab:KNN_CRPS_small",
          title = "CRPS of models fitted with the method of nearest neighbours using $X_{\reduced}$")


#GLMS


#GLM 1
crps_glm = crps_pois(data_small$radarDetections, 
                     as.numeric(exp(predict(mod_glm, 
                                            newdata = data_small))))
mean(crps_glm)

#GLM 2
crps_glm_2 = crps_pois(data_small$radarDetections, 
                       as.numeric(exp(predict(mod_glm_2, 
                                              newdata = data_small))))
mean(crps_glm_2)

#GLM 3
crps_glm_3 = crps_pois(data_small$radarDetections, 
                       as.numeric(exp(predict(mod_glm_3, 
                                              newdata = data_small))))
mean(crps_glm_3)

#Creating list of CRPS
list_crps_glm = c(mean(crps_glm), mean(crps_glm_2), mean(crps_glm_3))
names(list_crps_glm) = c("Simple", "With interaction terms", 
                              "With polynomials")

#Exporting to latex
stargazer(list_crps_glm, out = "table_GLM_CRPS.tex", label = "tab:GLM_CRPS",
          title = "CRPS for GLMs")


#Ensembled model


crps_ensembled = crps_pois(data_small$radarDetections, 
                     as.numeric(predict(mod_ensembled, newdata = data_ensembled)))
mean(crps_ensembled)

#Exporting to latex
list_crps_ensembled = c(mean(crps_ensembled))
stargazer(list_crps_ensembled, out = "table_ensembled_CRPS.tex", label = "tab:ensembled_CRPS",
          title = "CRPS for ensembled model")


#Rf glm

crps_rf_glm = crps_pois(data_small$radarDetections, 
                        as.numeric(predict(mod_rf_glm, newdata = data_rf)))
mean(crps_rf_glm)

#Exporting to latex
list_crps_rf_glm = c(mean(crps_rf_glm))
stargazer(list_crps_rf_glm, out = "table_rf_glm_CRPS.tex", label = "tab:rf_glm_CRPS",
          title = "CRPS for rf_glm  model")


#Evaluating models using Cross-validation -- -- -- --- - -- -- - -- -- -- -- -- -- -- - -- -- - -- - - - - -- - -- - --- --- --


#Shortening cross-validation expression
trainControl = trainControl(method = "cv", number = 5) #Needed in caret function


#GLMs


#Performing the cross-validation for GLMs
cv_glm_1 = train(radarDetections ~ ., data = data_small, method = "glm", family = poisson(link = log), trControl = trainControl)
cv_glm_2 = train(radarDetections ~ .^2, data = data_small, method = "glm", family = poisson(link = log), trControl = trainControl)
cv_glm_3 = train(radarDetections ~ poly(precip_max0_150, 3) + poly(new_snow100_150, 3) + poly(temp95_1_750, 3) + poly(Precip_1, 3) + poly(snow_depth25_750, 3) + poly(wind100_750, 3), data = data_small, method = "glm", family = poisson(link = log), trControl = trainControl)

#Results GLM
print(cv_glm_1$results$RMSE)
print(cv_glm_2$results$RMSE)
print(cv_glm_3$results$RMSE)

#Creating list
list_RMSE_GLMs = c(cv_glm_1$results$RMSE, cv_glm_2$results$RMSE, cv_glm_3$results$RMSE)
names(list_RMSE_GLMs) = c("Simple", "With interaction terms", "With polynomials")

#Exporting to latex
stargazer(list_RMSE_GLMs, out = "table_GLMS_RMSE.tex", label = "tab:RMSE_GLMS", title = "Root mean squared 5-fold cross-validation error for GLMs")


#Ensembled


#Performing the cross-validation for ensembled model
cv_ensembled = train(radarDetections ~ ., family = poisson(link = identity), data = data_ensembled, start = starting_values, method = "glm", trControl = trainControl)

#Results GLM
print(cv_ensembled$results$RMSE)

#Creating list
list_RMSE_ensembled= c(cv_ensembled$results$RMSE)

#Exporting to latex
stargazer(list_RMSE_ensembled, out = "table_ensembled_RMSE.tex", label = "tab:RMSE_ensembled", title = "Root mean squared 5-fold cross-validation error for ensembled model")


#Rf glm


#Performing the cross-validation for rf_glm
cv_rf_glm = train(radarDetections ~ ., family = poisson(link = identity), data = data_rf, start = starting_values_rf, method = "glm", trControl = trainControl)

#Results GLM
print(cv_rf_glm$results$RMSE)

#Creating list
list_RMSE_rf_glm= c(cv_rf_glm$results$RMSE)

#Exporting to latex
stargazer(list_RMSE_rf_glm, out = "table_rf_glm_RMSE.tex", label = "tab:RMSE_rf_glm", title = "Root mean squared 5-fold cross-validation error for rf_glm")


#Trees


#Result trees
print(min(cv_tree_small$results$RMSE))
print(min(cv_rf_small$results$RMSE))
print(min(cv_boost_small$results$RMSE))

#Creating list
list_RMSE_trees_small = c(min(cv_tree_small$results$RMSE), min(cv_rf_small$results$RMSE), min(cv_boost_small$results$RMSE[1]))
names(list_RMSE_trees_small) = c("RMSE pruned tree", "RMSE random forest", "RMSE boosted tree")

#Exporting to latex
stargazer(list_RMSE_trees_small, out = "table_trees_RMSE_small.tex", label = "tab:RMSE_trees_small", title = "Root mean squared 5-fold cross-validation error for tree models fitted using $X_{\text{reduced}}$")


#Neighbours


#Result Nearest neighbours
print(min(cv_knn_small$results$RMSE))
print(min(knn_2_small$results$RMSE))
print(min(knn_5_small$results$RMSE))

#Creating list
list_RMSE_nn_small = c(cv_knn_small$results$RMSE[2], cv_knn_small$results$RMSE[5], min(cv_knn_small$results$RMSE))
names(list_RMSE_nn_small) = c("2 nearest neighbours", "5 nearest neighbours", "20 nearest neighbours")

#Exporting to latex
stargazer(list_RMSE_nn_small, out = "table_nn_RMSE_small.tex", label = "tab:RMSE_nn_small", title = "Root mean squared 5-fold cross-validation error of models fitted with the method of nearest neighbours using $X_{reduced}$")

#Evaluating models using PIT -- -- -- --- - -- -- - -- -- -- -- -- -- -- - -- -- - -- - - - - -- - -- - --- --- --

#setwd("C:/Users/benja/OneDrive/Documents/Universitetet/Masteroppgave/Figures") #Choosing folder to work in


#GLM


#Opening a PNG graphics device
png("PIT_GLM.png", width = 800, height = 600)

#Plot for GLM 1
pit(data_small$radarDetections, as.numeric(exp(predict(mod_glm, newdata = data_small))), distr=c("poisson"), main="PIT of simple GLM")

#Closing the device to save the plot
dev.off()

#Opening a PNG graphics device
png("PIT_GLM_2.png", width = 800, height = 600)

#Plot for GLM 2
pit(data_small$radarDetections, as.numeric(exp(predict(mod_glm_2, newdata = data_small))), distr=c("poisson"), main="PIT of GLM with interaction terms")

#Closing the device to save the plot
dev.off()

#Opening a PNG graphics device
png("PIT_GLM_3.png", width = 800, height = 600)

#Plot for GLM 3
pit(data_small$radarDetections, as.numeric(exp(predict(mod_glm_3, newdata = data_small))), distr=c("poisson"), main="PIT of GLM with explanatory variables as polynomials")

#Closing the device to save the plot
dev.off()


#Ensembled model


#Opening a PNG graphics device
png("PIT_ensembled.png", width = 800, height = 600)

#Plot for ensembled model
pit(data_small$radarDetections, as.numeric(predict(mod_ensembled, newdata = data_ensembled)), distr=c("poisson"), main="PIT of ensembled model")

#Closing the device to save the plot
dev.off()


#Rf glm


#Opening a PNG graphics device
png("PIT_rf_glm.png", width = 800, height = 600)

#Plot for rf_glm
pit(data_rf$radarDetections, as.numeric(predict(mod_rf_glm, newdata = data_rf)), distr=c("poisson"), main="PIT of rf_glm")

#Closing the device to save the plot
dev.off()


#Predictions of different days -- -- -- -- --- -- -- --- --- -- -- -- -- -- -- --- -- -- --- - --- - --- - -- --- -- -- -- -- - -- -- -- - - - - -


#setwd("C:/Users/benja/OneDrive/Documents/Universitetet/Masteroppgave/Tables") #Choosing folder to work in


#Prediction of day with 3 avalanches detected


#Extracting the day
data_3aval_small = data_small[26, ] 
data_3aval_ensembled = data_ensembled[26, ]
data_3aval_rf_glm = data_rf[26, ]
data_small[26, ]$radarDetections

#Tree predictions
exp(predict(mod_boost_small, newdata = data_3aval_small))
predict(mod_rf_small, newdata = data_3aval_small)
predict(mod_tree_small, newdata = data_3aval_small)

#List of tree predictions
list_3aval_trees_small = c(predict(mod_tree_small, newdata = data_3aval_small), predict(mod_rf_small, newdata = data_3aval_small), exp(predict(mod_boost_small, newdata = data_3aval_small)))
names(list_3aval_trees_small) = c("Pruned tree", "Random forest", "Boosted model")

#Exporting to latex
stargazer(list_3aval_trees_small, out = "table_trees_3aval_small.tex", label = "tab:trees_3aval_small",
          title = "Prediction of tree models fitted using $X_{reduced}$ on a day with 3 avalanches detected")

#Predictions using method of nearest neighbours
predictions_2_small[26]
predictions_5_small[26]
predictions_best_small[26]

#List of KNN predictions
list_3aval_knns_small = c(predictions_2_small[26], predictions_5_small[26], predictions_best_small[26])
names(list_3aval_knns_small) = c("Model using 2 nearest neighbours", "Model using 5 nearest neighbours", "Model using x nearest neighbours")

#Exporting to latex
stargazer(list_3aval_knns_small, out = "table_knn_3aval_small.tex", label = "tab:knn_3aval_small", title = "Prediction of models fitted using the method of nearest neighbours and $X_{reduced}$ on a day with 3 avalanches detected")

#GLM predictions
exp(predict(mod_glm, newdata = data_3aval_small))
exp(predict(mod_glm_2, newdata = data_3aval_small))
exp(predict(mod_glm_3, newdata = data_3aval_small))

#List of GLM predictions
list_3aval_glms = c(exp(predict(mod_glm, newdata = data_3aval_small)), exp(predict(mod_glm_2, newdata = data_3aval_small)), exp(predict(mod_glm_3, newdata = data_3aval_small)))
names(list_3aval_glms) = c("Simple GLM", "GLM with interaction between explanatory variables", "GLM with explanatory variables as polynomials")

#Exporting to latex
stargazer(list_3aval_glms, out = "table_glm_3aval.tex", label = "tab:glm_3aval", title = "Prediction of GLMs on a day with 3 avalanches detected")

#Ensembled prediction
predict(mod_ensembled, newdata = data_3aval_ensembled)

#List of ensembled prediction
list_3aval_ensembled = c(predict(mod_ensembled, newdata = data_3aval_ensembled))

#Exporting to latex
stargazer(list_3aval_ensembled, out = "table_ensembled_3aval.tex", label = "tab:ensembled_3aval", title = "Prediction of ensembled model on a day with 3 avalanches detected")

#rf_glm prediction
predict(mod_rf_glm, newdata = data_3aval_rf_glm)

#List of rf_glm prediction
list_3aval_rf_glm = c(predict(mod_rf_glm, newdata = data_3aval_rf_glm))

#Exporting to latex
stargazer(list_3aval_rf_glm, out = "table_rf_glm_3aval.tex", label = "tab:rf_glm_3aval", title = "Prediction of ensembled model on a day with 3 avalanches detected")


#Prediction of day with 1 avalanches detected


#Extracting the day
data_1aval_small = data_small[37, ] 
data_1aval_ensembled = data_ensembled[37, ]
data_1aval_rf_glm = data_ensembled[37, ]
data_small[37, ]$radarDetections

#Tree predictions
exp(predict(mod_boost_small, newdata = data_1aval_small))
predict(mod_rf_small, newdata = data_1aval_small)
predict(mod_tree_small, newdata = data_1aval_small)

#List of tree predictions
list_1aval_trees_small = c(predict(mod_tree_small, newdata = data_1aval_small), predict(mod_rf_small, newdata = data_1aval_small), exp(predict(mod_boost_small, newdata = data_1aval_small)))
names(list_1aval_trees_small) = c("Pruned tree", "Random forest", "Boosted model")

#Exporting to latex
stargazer(list_1aval_trees_small, out = "table_trees_1aval_small.tex", label = "tab:trees_1aval_small", title = "Prediction of tree models fitted using $X_{reduced}$ on a day with 1 avalanche detected")

#Predictions using method of nearest neighbours
predictions_2_small[37]
predictions_5_small[37]
predictions_best_small[37]

#List of KNN predictions
list_1aval_knns_small = c(predictions_2_small[37], predictions_5_small[37], predictions_best_small[37])
names(list_1aval_knns_small) = c("Model using 2 nearest neighbours", "Model using 5 nearest neighbours", "Model using x nearest neighbours")

#Exporting to latex
stargazer(list_1aval_knns_small, out = "table_knn_1aval_small.tex", label = "tab:knn_1aval_small", title = "Prediction of models fitted using the method of nearest neighbours and $X_{reduced}$ on a day with 1 avalanche detected")

#GLM predictions
exp(predict(mod_glm, newdata = data_1aval_small))
exp(predict(mod_glm_2, newdata = data_1aval_small))
exp(predict(mod_glm_3, newdata = data_1aval_small))

#List of GLM predictions
list_1aval_glms = c(exp(predict(mod_glm, newdata = data_1aval_small)), exp(predict(mod_glm_2, newdata = data_1aval_small)), exp(predict(mod_glm_3, newdata = data_1aval_small)))
names(list_1aval_glms) = c("Simple GLM", "GLM with interaction between explanatory variables", "GLM with explanatory variables as polynomials")

#Exporting to latex
stargazer(list_1aval_glms, out = "table_glm_1aval.tex", label = "tab:glm_1aval", title = "Prediction of GLMs on a day with 1 avalanche detected")

#Ensembled prediction
predict(mod_ensembled, newdata = data_1aval_ensembled)

#List of ensembled prediction
list_1aval_ensembled = c(predict(mod_ensembled, newdata = data_1aval_ensembled))

#Exporting to latex
stargazer(list_1aval_ensembled, out = "table_ensembled_1aval.tex", label = "tab:ensembled_1aval", title = "Prediction of ensembled model on a day with 1 avalanche detected")

#rf_glm prediction
predict(mod_rf_glm, newdata = data_1aval_rf_glm)

#List of ensembled prediction
list_1aval_rf_glm = c(predict(mod_rf_glm, newdata = data_1aval_rf_glm))

#Exporting to latex
stargazer(list_1aval_rf_glm, out = "table_rf_glm_1aval.tex", label = "tab:rf_glm_1aval", title = "Prediction of rf_glm on a day with 1 avalanche detected")


#Prediction of day with 0 avalanches detected


#Extracting the day
data_0aval_small = data_small[4, ] 
data_0aval_ensembled = data_ensembled[4, ]
data_0aval_rf_glm = data_rf[4, ]
data_small[4, ]$radarDetections

#Tree predictions
exp(predict(mod_boost_small, newdata = data_0aval_small))
predict(mod_rf_small, newdata = data_0aval_small)
predict(mod_tree_small, newdata = data_0aval_small)

#List of tree predictions
list_0aval_trees_small = c(predict(mod_tree_small, newdata = data_0aval_small), predict(mod_rf_small, newdata = data_0aval_small), exp(predict(mod_boost_small, newdata = data_0aval_small)))
names(list_0aval_trees_small) = c("Pruned tree", "Random forest", "Boosted model")

#Exporting to latex
stargazer(list_0aval_trees_small, out = "table_trees_0aval_small.tex", label = "tab:trees_0aval_small", title = "Prediction of tree models fitted using $X_{reduced}$ on a day with 0 avalanches detected")

#Predictions using method of nearest neighbours
predictions_2_small[4]
predictions_5_small[4]
predictions_best_small[4]

#List of KNN predictions
list_0aval_knns_small = c(predictions_2_small[4], predictions_5_small[4], predictions_best_small[4])
names(list_0aval_knns_small) = c("Model using 2 nearest neighbours", "Model using 5 nearest neighbours", "Model using x nearest neighbours")

#Exporting to latex
stargazer(list_0aval_knns_small, out = "table_knn_0aval_small.tex", label = "tab:knn_0aval_small", title = "Prediction of models fitted using the method of nearest neighbours and $X_{reduced}$ on a day with 0 avalanches detected")

#GLM predictions
exp(predict(mod_glm, newdata = data_0aval_small))
exp(predict(mod_glm_2, newdata = data_0aval_small))
exp(predict(mod_glm_3, newdata = data_0aval_small))

#List of GLM predictions
list_0aval_glms = c(exp(predict(mod_glm, newdata = data_0aval_small)), exp(predict(mod_glm_2, newdata = data_0aval_small)), exp(predict(mod_glm_3, newdata = data_0aval_small)))
names(list_0aval_glms) = c("Simple GLM", "GLM with interaction between explanatory variables", "GLM with explanatory variables as polynomials")

#Exporting to latex
stargazer(list_0aval_glms, out = "table_glm_0aval.tex", label = "tab:glm_0aval", title = "Prediction of GLMs on a day with 0 avalanches detected")

#Ensembled prediction
predict(mod_ensembled, newdata = data_0aval_ensembled)

#List of ensembled prediction
list_0aval_ensembled = c(predict(mod_ensembled, newdata = data_0aval_ensembled))

#Exporting to latex
stargazer(list_0aval_ensembled, out = "table_ensembled_0aval.tex", label = "tab:ensembled_0aval", title = "Prediction of ensembled model on a day with 0 avalanches detected")

#rf_glm prediction
predict(mod_rf_glm, newdata = data_0aval_rf_glm)

#List of ensembled prediction
list_0aval_rf_glm = c(predict(mod_rf_glm, newdata = data_0aval_rf_glm))

#Exporting to latex
stargazer(list_0aval_rf_glm, out = "table_rf_glm_0aval.tex", label = "tab:rf_glm_0aval", title = "Prediction of rf_glm on a day with 0 avalanches detected")

#Evaluating other possible avalanche forecasts --------------------------------------------------------------------------------------------------------------------------------------------


#Avalanche danger level --- --- -- ---- --- --- --- ---- --- ---- --  ---------------- -- --- -- -- --- -- --- ---- --


#Adding radar detections
warningData$radarDetections = data$radarDetections

#Formatting so that danger level is a factor
warningData$Faregrad = as.factor(warningData$Faregrad)

#Creating model 
mod_warning = glm(radarDetections ~ Faregrad, family = poisson(link = log), data = warningData )
summary(mod_warning)

#Calculating CRPS
crps_warning = crps_pois(warningData$radarDetections, 
                         as.numeric(exp(predict(mod_warning, 
                                                newdata = warningData))))
mean(crps_warning)

#Calculating RMSE
cv_warning = train(radarDetections ~ Faregrad, data = warningData, method = "glm", family = poisson(link = log), trControl = trainControl)

#Results RMSE
RMSE_warning = cv_warning$results$RMSE
print(RMSE_warning)


#GLM using only intercept ------------------------------------------------------------------------------ ---------------------- -


mod_intercept = glm(radarDetections ~ 1, family = poisson(link = log), data = warningData )
summary(mod_intercept)

#Calculating CRPS
crps_intercept = crps_pois(warningData$radarDetections, 
                           as.numeric(exp(predict(mod_intercept, 
                                                  newdata = warningData))))
mean(crps_intercept)

#Calculating RMSE
RMSE_intercept = RMSE(warningData$radarDetections, 
     as.numeric(exp(predict(mod_intercept, 
                            newdata = warningData))))
print(RMSE_intercept)


#GLM with lag 1 as explanatory variable ------------ - -------------- ---------------------- ------ ---- ------ ---- --


mod_lag = glm(radarDetections ~ radarDetections_1, family = poisson(link = log), data = data )
summary(mod_lag)

#Calculating CRPS
crps_lag = crps_pois(data$radarDetections, 
                     as.numeric(exp(predict(mod_lag,
                                            newdata = data))))
mean(crps_lag)

#Calculating RMSE
RMSE_lag = RMSE(warningData$radarDetections, 
                      as.numeric(exp(predict(mod_lag, 
                                             newdata = data))))
print(RMSE_lag)



#Performance --- ---------- ----  -- -- -- - --  -- -- -  -- ---- - -- ---- -- -- --- --------------------- -


#List of CRPS
list_crps_other = c(mean(crps_warning), mean(crps_intercept), mean(crps_lag))
names(list_crps_other) = c("Danger level model", "Intercept-only model", "Lag 1 model")

#Exporting to latex
stargazer(list_crps_other, out = "table_crp_other.tex", label = "tab:crps_other", title = "CRPS of alternative forecasts")

#List of RMSE
list_RMSE_other = c(RMSE_warning, RMSE_intercept, RMSE_lag)
names(list_RMSE_other) = c("Danger level model", "Intercept-only model", "Lag 1 model")

#Exporting to latex
stargazer(list_RMSE_other, out = "table_crp_other.tex", label = "tab:crps_other", title = "CRPS of alternative forecasts")



#Illustration plots ----------------------------------------

#setwd("C:/Users/benja/OneDrive/Documents/Universitetet/Masteroppgave/Figures")


#CRPS poisson distribution


#Parameters
lambda = 5 # Mean of the Poisson distribution
obs_val = 7 # Observed value

#Poisson CDF
poisson_data = data.frame(x = 0:(obs_val * 2)) #Deciding the domain
poisson_data$CDF = ppois(poisson_data$x, lambda = lambda) #Creating poisson CDF

#Indicator function
indicator_function = data.frame(x = c(obs_val, obs_val),
                                y = c(0, 1))

#Calculate the y-value of the CDF
cdf_at_obs = ppois(obs_val, lambda)

#Plotting
ggplot() +
  geom_step(data = poisson_data, aes(x = x, y = CDF), color = 'blue', size = 1.2, linetype = "solid") +
  geom_segment(data = indicator_function, aes(x = obs_val, xend = obs_val, y = 0, yend = 1), color = 'red', size = 1.2) +
  geom_segment(aes(x = obs_val, xend = 0, y = 0, yend = 0), color = 'red', size = 1.2) +
  geom_segment(aes(x = obs_val, xend = obs_val * 2, y = 1, yend = 1), color = 'red', size = 1.2) +
  scale_x_continuous(name = "Value", breaks = seq(0, (obs_val * 2), by = 1)) +
  scale_y_continuous(name = "Probability", limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  ggtitle("CRPS of a Poisson distribution") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) 
ggsave("crps.png", width = 1200, height = 900, units = "px")


#CRPS point estimate


#Parameters
lambda = 5 # Point prediction
obs_val = 7 # Observed value

#Point Prediction
point_prediction =  data.frame(x = c(lambda, lambda),
                               y = c(0, 1))

#Indicator function
indicator_function = data.frame(x = c(obs_val, obs_val),
                                y = c(0, 1))

#Plotting
ggplot() +
  geom_segment(data = indicator_function, aes(x = lambda, xend = lambda, y = 0, yend = 1), color = 'blue', size = 1.2)  +
  geom_segment(aes(x = lambda, xend = 0, y = 0, yend = 0), color = 'blue', size = 1.2) +
  geom_segment(aes(x = lambda, xend = lambda * 2, y = 1, yend = 1), color = 'blue', size = 1.2) +
  geom_segment(data = indicator_function, aes(x = obs_val, xend = obs_val, y = 0, yend = 1), color = 'red', size = 1.2) +
  geom_segment(aes(x = obs_val, xend = 0, y = 0, yend = 0), color = 'red', size = 1.2) +
  geom_segment(aes(x = obs_val, xend = obs_val * 2, y = 1, yend = 1), color = 'red', size = 1.2) +
  scale_x_continuous(name = "Number of avalanches", breaks = seq(0, (obs_val * 2), by = 1)) +
  scale_y_continuous(name = "Probability", limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  ggtitle("CRPS of a point prediction") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) 
ggsave("crps_KNN.png", width = 1200, height = 900, units = "px")


#Tree example


tree_example = rpart(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, 
                     data = iris)

#Opening a PNG device
png("tree_example.png", width = 800, height = 600)

#Plotting example tree
plot(tree_example, uniform = TRUE)
text(tree_example)  # Add text to the plot to label the nodes

#Saving the plot
dev.off()


#Tables of data -------------------------------------------------------------------------------------


#Function for creating table in latex
latex_table = function(matrix){
  print(xtable(matrix),
        include.rownames = TRUE, 
        include.colnames = FALSE,
        only.contents = TRUE,
        hline.after = NULL, # Ingen linjer mellom rader
        sanitize.text.function = function(x){x},
        comment = FALSE)
}


#Performance ------------------------------------------------------------------ -- - -- --- -- -- --- -- -


#Trees


#Creating a matrix of all results
matrix_tree = matrix(c(as.numeric(list_crps_trees), 
                       as.numeric(list_crps_trees_small),
                       as.numeric(list_RMSE_trees),
                       as.numeric(list_RMSE_trees_small)), 
                     nrow = 2, 
                     byrow = TRUE)
#Adding rownames
rownames(matrix_tree) = c("CRPS","RMSE")

#Creating latex table that is copied to Overleaf
latex_tree = latex_table(matrix_tree)


#Nearest neighbours


#Creating a matrix of all results
matrix_nn = matrix(c(as.numeric(list_crps_nn), 
                     as.numeric(list_crps_nn_small),
                     as.numeric(list_RMSE_nn),
                     as.numeric(list_RMSE_nn_small)), 
                    nrow = 2, 
                    byrow = TRUE)
#Adding rownames
rownames(matrix_nn) = c("CRPS","RMSE")

#Creating latex table that is copied to Overleaf
latex_nn = latex_table(matrix_nn)


#GLMs


#Creating a matrix of all results
matrix_glm = matrix(c(as.numeric(list_crps_glm),
                      as.numeric(list_RMSE_GLMs)), 
                    nrow = 2, 
                    byrow = TRUE)
#Adding rownames
rownames(matrix_glm) = c("CRPS", "RMSE")

#Creating latex table that is copied to Overleaf
latex_glm = latex_table(matrix_glm)


#Ensembled


#Creating a matrix of all results
matrix_ensembled = matrix(c(as.numeric(list_crps_ensembled),
                            as.numeric(list_RMSE_ensembled)), 
                            nrow = 2, 
                            byrow = TRUE)
#Adding rownames
rownames(matrix_ensembled) = c("CRPS", "RMSE")

#Creating latex table that is copied to Overleaf
latex_ensembled = latex_table(matrix_ensembled)


#Rf glm


#Creating a matrix of all results
matrix_rf_glm = matrix(c(as.numeric(list_crps_rf_glm),
                            as.numeric(list_RMSE_rf_glm)), 
                          nrow = 2, 
                          byrow = TRUE)
#Adding rownames
rownames(matrix_rf_glm) = c("CRPS", "RMSE")

#Creating latex table that is copied to Overleaf
latex_rf_glm = latex_table(matrix_rf_glm)


#Other


#Creating a matrix of all results
matrix_other = matrix(c(as.numeric(list_crps_other),
                        as.numeric(list_RMSE_other)), 
                      nrow = 2, 
                      byrow = TRUE)
#Adding rownames
rownames(matrix_other) = c("CRPS", "RMSE")

#Creating latex table that is copied to Overleaf
latex_other = latex_table(matrix_other)


#Example dates ------------------------------------------------------------------ -- - -- --- -- -- --- -- -


#Trees


#Creating a matrix of all results
matrix_tree_dates = matrix(c(as.numeric(list_0aval_trees), 
                             as.numeric(list_0aval_trees_small),
                             as.numeric(list_1aval_trees),
                             as.numeric(list_1aval_trees_small), 
                             as.numeric(list_3aval_trees),
                             as.numeric(list_3aval_trees_small)),
                           nrow = 3, 
                           byrow = TRUE)
#Adding rownames
rownames(matrix_tree_dates) = c("Day 1", "Day 2", "Day 3")

#Creating latex table that is copied to Overleaf
latex_tree_dates = latex_table(matrix_tree_dates)


#Nearest neighbours


#Creating a matrix of all results
matrix_nn_dates = matrix(c(as.numeric(list_0aval_knns), 
                           as.numeric(list_0aval_knns_small),
                           as.numeric(list_1aval_knns),
                           as.numeric(list_1aval_knns_small), 
                           as.numeric(list_3aval_knns),
                           as.numeric(list_3aval_knns_small)),
                         nrow = 3, 
                         byrow = TRUE)
#Adding rownames
rownames(matrix_nn_dates) = c("Day 1", "Day 2", "Day 3")

#Creating latex table that is copied to Overleaf
latex_nn_dates = latex_table(matrix_nn_dates)


#GLMs


#Creating a matrix of all results
matrix_glm_dates = matrix(c(as.numeric(list_0aval_glms), 
                            as.numeric(list_1aval_glms),
                            as.numeric(list_3aval_glms)),
                          nrow = 3, 
                          byrow = TRUE)

#Adding rownames
rownames(matrix_glm_dates) = c("Day 1", "Day 2", "Day 3")

#Creating latex table that is copied to Overleaf
latex_glm_dates = latex_table(matrix_glm_dates)


#Ensembled


#Creating a matrix of all results
matrix_ensembled_dates = matrix(c(as.numeric(list_0aval_ensembled), 
                                  as.numeric(list_1aval_ensembled),
                                  as.numeric(list_3aval_ensembled)),
                                nrow = 3, 
                                byrow = TRUE)
#Adding rownames
rownames(matrix_ensembled_dates) = c("Day 1", "Day 2", "Day 3")

#Creating latex table that is copied to Overleaf
latex_ensembled_dates = latex_table(matrix_ensembled_dates)


#Rf glm


#Creating a matrix of all results
matrix_rf_glm_dates = matrix(c(as.numeric(list_0aval_rf_glm), 
                                  as.numeric(list_1aval_rf_glm),
                                  as.numeric(list_3aval_rf_glm)),
                                nrow = 3, 
                                byrow = TRUE)
#Adding rownames
rownames(matrix_rf_glm_dates) = c("Day 1", "Day 2", "Day 3")

#Creating latex table that is copied to Overleaf
latex_rf_glm_dates = latex_table(matrix_rf_glm_dates)


#Features of dates 


#Printing names of models
names(data_0aval_small)

#Creating a matrix of all results
matrix_features_dates = matrix(c(as.numeric(data_0aval_small[, -1]), 
                                 as.numeric(data_1aval_small[, -1]),
                                 as.numeric(data_3aval_small[, -1])),
                                nrow = 3, 
                                byrow = TRUE)
#Adding rownames
rownames(matrix_features_dates) = c("Day 1", "Day 2", "Day 3")

#Creating latex table that is copied to Overleaf
latex_ensembled_dates = latex_table(matrix_features_dates)


#Summary --- --- --- -------- ----- ------ ------ ---- ----- ---- ---- --- -- -- -- -


#Defining a function to format the values
format_values = function(x) {
  if (abs(x) < 0.005) {
    parts = unlist(strsplit(formatC(x, format = "e", digits = 2), "e"))
    sprintf("%s*10^{%s}", parts[1], as.numeric(parts[2]))
  } else {
    round(x,2)
  }
}


#Glm 1


#Printing names of coefficients for glm1
names(round(summary(mod_glm)$coefficients[, 4], 3))

#Creating a matrix of all results for glm1
matrix_glm1 = matrix(c(as.numeric(summary(mod_glm)$coefficients[, 4]), 
                       as.numeric(summary(mod_glm)$coefficients[, 1])),
                     nrow = 2, 
                     byrow = TRUE)

#Using apply to format all values in the matrix
matrix_glm1 = apply(matrix_glm1,c(1:nrow(matrix_glm1)), format_values)

#Adding rownames
rownames(matrix_glm1) = c("P-values", "Coefficients")


#Creating latex table that is copied to Overleaf
latex_matrix_glm1 = latex_table(matrix_glm1)


#Glm 2


#Printing names of coefficients for glm2
names(round(summary(mod_glm_2)$coefficients[, 4], 3))

#Creating a matrix of all results for glm2
matrix_glm2 = matrix(c(as.numeric(summary(mod_glm_2)$coefficients[, 4]), 
                       as.numeric(summary(mod_glm_2)$coefficients[, 1])),
                     nrow = 2, 
                     byrow = TRUE)

#Using apply to format all values in the matrix
matrix_glm2 = apply(matrix_glm2,c(1:nrow(matrix_glm2)), format_values)

#Adding rownames
rownames(matrix_glm2) = c("P-values", "Coefficients")


#Creating latex table that is copied to Overleaf
latex_matrix_glm2 = latex_table(matrix_glm2)


#Glm 3


#Printing names of coefficients for glm3
names(round(summary(mod_glm_3)$coefficients[, 4], 3))

#Creating a matrix of all results for glm3
matrix_glm3 = matrix(c(as.numeric(summary(mod_glm_3)$coefficients[, 4]), 
                       as.numeric(summary(mod_glm_3)$coefficients[, 1])),
                     nrow = 2, 
                     byrow = TRUE)

#Using apply to format all values in the matrix
matrix_glm3 = apply(matrix_glm3,c(1:nrow(matrix_glm3)), format_values)

#Adding rownames
rownames(matrix_glm3) = c("P-values", "Coefficients")


#Creating latex table that is copied to Overleaf
latex_matrix_glm3 = latex_table(matrix_glm3)


#Ensembled


#Printing names of coefficients for glm3
names(round(summary(mod_ensembled)$coefficients[, 4], 3))

#Creating a matrix of all results for glm3
matrix_ensembled_1 = matrix(c(as.numeric(summary(mod_ensembled)$coefficients[, 4]), 
                       as.numeric(summary(mod_ensembled)$coefficients[, 1])),
                     nrow = 2, 
                     byrow = TRUE)

#Using apply to format all values in the matrix
matrix_ensembled_1 = apply(matrix_ensembled_1,c(1:nrow(matrix_ensembled_1)), format_values)

#Adding rownames
rownames(matrix_ensembled_1) = c("P-values", "Coefficients")


#Creating latex table that is copied to Overleaf
latex_ensembled_1 = latex_table(matrix_ensembled_1)


#Rf glm


#Printing names of coefficients for glm3
names(round(summary(mod_rf_glm)$coefficients[, 4], 3))

#Creating a matrix of all results for glm3
matrix_rf_glm_1 = matrix(c(as.numeric(summary(mod_rf_glm)$coefficients[, 4]), 
                              as.numeric(summary(mod_rf_glm)$coefficients[, 1])),
                            nrow = 2, 
                            byrow = TRUE)

#Using apply to format all values in the matrix
matrix_rf_glm_1 = apply(matrix_rf_glm_1,c(1:nrow(matrix_rf_glm_1)), format_values)

#Adding rownames
rownames(matrix_rf_glm_1) = c("P-values", "Coefficients")


#Creating latex table that is copied to Overleaf
latex_rf_glm_1 = latex_table(matrix_rf_glm_1)







#Plot of predictions -------------------------------------------------------------------------------------------------


#setwd("C:/Users/benja/OneDrive/Documents/Universitetet/Masteroppgave/Figures")

#Function for creating prediction plots
predictionPlot = function(list_glms, list_deterministic, day, max_avalanche_count) {
  #Set names for GLMs and deterministic models
  names(list_glms) = c("Simple GLM", "Ensembled", "RF ensembled")
  names(list_deterministic) = c("Tree-based model", "KNN model")
  
  #A sequence of avalanche counts the predictions are evaluated on
  x_values = 0:max_avalanche_count
  
  #A data frame for plotting multiple Poisson cumulative distributions
  poisson_cdf_df = do.call(rbind, lapply(1:length(list_glms), function(i) {
    lambda = list_glms[[i]]
    data.frame(
      x = x_values,
      Cumulative_Probability = ppois(x_values, lambda = lambda),
      Distribution = names(list_glms)[i]
    )
  }))
  
  #A data frame for the step function cumulative distributions
  step_cdf_df = do.call(rbind, lapply(1:length(list_deterministic), function(i) {
    mu = list_deterministic[[i]]
    data.frame(
      x = x_values,
      Cumulative_Probability = ifelse(x_values < mu, 0, 1),
      Distribution = names(list_deterministic)[i]
    )
  }))
  
  #Combining Poisson and step function data frames
  combined_cdf_df = rbind(poisson_cdf_df, step_cdf_df)
  
  #Plot of the cumulative distribution functions
  ggplot(combined_cdf_df, aes(x = x, y = Cumulative_Probability, color = Distribution)) +
    geom_step(size = 1.2) +
    geom_point(size = 2) +
    labs(title = paste("CDF for day", day),
         x = "Avalanche count",
         y = "Cumulative Probability",
         color = "Distribution") +
    theme_minimal()
  ggsave(paste0("predictions_day_", day, ".png"), width = 1200, height = 900, units = "px")
}

#0 avalanches
predictionPlot(list_glms = c(list_0aval_glms[1],
                             list_0aval_ensembled[1],
                             list_0aval_rf_glm[1]),
               list_deterministic = c(list_0aval_trees_small[2],
                                      list_0aval_knns_small[3]),
               day = "1",
               max_avalanche_count = 3)

#1 avalanche
predictionPlot(list_glms = c(list_1aval_glms[1],
                             list_1aval_ensembled[1],
                             list_1aval_rf_glm[1]),
               list_deterministic = c(list_1aval_trees_small[2],
                                      list_1aval_knns_small[3]),
               day = "2",
               max_avalanche_count = 4)

#3 avalanches
predictionPlot(list_glms = c(list_3aval_glms[1],
                             list_3aval_ensembled[1],
                             list_3aval_rf_glm[1]),
               list_deterministic = c(list_3aval_trees_small[2],
                                      list_3aval_knns_small[3]),
               day = "3",
               max_avalanche_count = 6)




