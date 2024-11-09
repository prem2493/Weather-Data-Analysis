proj.path<-getwd()

library("dplyr")


dataset <- read.csv("C:/Users/Prem Kumar/Downloads/archive (1)/Temperature_And_Precipitation_Cities_IN/Delhi_NCR_1990_2022_Safdarjung.csv")
View(dataset)
dataset$time<-as.Date(dataset$time,format = "%d-%m-%Y")
tail(dataset$time)
year=as.numeric(format(dataset$time,"%Y"))
month=as.numeric(format(dataset$time,"%m"))
day=as.numeric(format(dataset$time,"%d"))
dataset<-cbind(dataset,year,month,day)

View(dataset)

yearly_averages <- dataset %>%
  group_by(year) %>%
  summarise(average_temperature = mean(tavg, na.rm = TRUE))

View(yearly_averages)


# Plot the temperature trends
yearly_averages %>%
  ggplot(aes(x = year, y = average_temperature)) +
  geom_line(col="orange") +
  scale_x_continuous(breaks = 1990:2022  ,labels = 1990:2022) +
  scale_y_continuous(limits = c(20, 30)) +
  labs(title = 'Yearly Average Temperature Trends',
       x = 'Year',
       y = 'Temperature') +
  theme_minimal()


yearly_max <- dataset %>%
  group_by(year) %>%
  summarise(max_temperature = max(tmax, na.rm = TRUE))

View(yearly_max)

# Plot the temperature trends
yearly_max %>%
  ggplot(aes(x = year, y = max_temperature)) +
  geom_line(col="red") +
  scale_x_continuous(breaks = 1990:2022  ,labels = 1990:2022) +
  scale_y_continuous(limits = c(40, 50)) +
  labs(title = 'Yearly Maximum Temperature Trends',
       x = 'Year',
       y = 'Temperature') +
  theme_minimal()



# To identify the hottest and coldest months
max_temp_month <- dataset %>%
  group_by(month) %>%
  summarise(max_temperature = max(tmax, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(month = factor(month, levels = 1:12, labels = month.name[1:12])) %>%
  arrange(desc(max_temperature))

View(max_temp_month)

plot(max_temp_month$month, max_temp_month$max_temperature, 
     xlab="month", ylab="Temperature", col="orange",
     type="l", main=" Max Temperature Recorded for each month about 30 years", 
     xlim=c(1,12), 
     ylim=c(30,50))

ggplot(max_temp_month, aes(x = month, y = max_temperature)) +
  geom_line() +
  geom_point(color = "red") +
  labs(title = " Max Temperature Recorded for each month about 30 years", 
       x = "Month", 
       y = "Temperature")


par(mfrow=c(1,1))

mydata=dataset[which(dataset$year==1990),]

View(mydata)

mydata2021=subset(dataset,subset=dataset$time>='2021-01-01' & dataset$time<='2021-12-31')


View(mydata2021)

plot(mydata[,1],mydata[,2],xlab="month",ylab="tavg",type="l",col="orange")
lines(mydata[,1],mydata2021[,2],lwd=2,col="red")
legend(x = "topleft", box.col = "brown", 
       bg ="white", box.lwd = 2 , title="Years",  
       legend=c(1990, 2021),  
       fill = c("orange","red"))

par(mfrow=c(3,1))
boxplot(dataset$tmin ~ dataset$year,main="temp_sereis",ylim=c(0,50),
        ylab="Tmin in celsius",xlab="year",col="yellow")
boxplot(dataset$tavg ~ dataset$year,main="temp_sereis",ylim=c(0,50),
        ylab="Tavg in celsius",xlab="year",col="orange")
boxplot(dataset$tmax ~ dataset$year,main="temp_sereis",ylim=c(0,50),
        ylab="Tmax in celsius",xlab="year",col="red")


min_temp_month <- dataset %>%
  group_by(month) %>%
  summarise(min_temperature = min(tmin, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(month = factor(month, levels = 1:12, labels = month.name[1:12])) %>%
  arrange(min_temperature)

ggplot(min_temp_month,aes(x=month,y=min_temperature))+geom_point()



#rain prediction

library(caTools)

file_path <- "C:/Users/Prem Kumar/Downloads/IndianWeatherRepository.csv/IndianWeatherRepository.csv"

weather_data <- read.csv(file_path)

View(weather_data)

x<-as.Date(weather_data$last_updated)
year<-as.numeric(format(x,'%Y'))
month<-as.numeric(format(x,'%m'))
day<-as.numeric(format(x,'%d'))
weather_data=cbind(weather_data,year,month,day)

View(weather_data)

split <- sample.split(weather_data$precip_mm, SplitRatio = 0.8)
train_data <- subset(weather_data,split==TRUE)
test_data <- subset(weather_data,split==FALSE)

View(train_data)

View(test_data)

model <- lm( precip_mm ~ temperature_celsius + latitude + longitude + condition_text + pressure_in + humidity + wind_kph + cloud, data = weather_data)

predictions <- predict(model, newdata = test_data)

library("Metrics")
par(mfrow=c(1,1))
rmse <- rmse(test_data$precip_mm,predictions)
cat("Root Mean Squared Error (RMSE):", rmse, "\n")

plot(test_data$precip_mm,col="red")
points(predictions)

library("ggplot2")






  