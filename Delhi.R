proj.path<-getwd()

#install.packages("dplyr")
library("dplyr")
#install.packages("ggplot2")
library(ggplot2)
#install.packages("Metrics")
library("Metrics")
#install.packages("caTools")
library("caTools")


dataset <- read.csv(paste(proj.path,'/Delhi_data.csv',sep=''))

View(dataset)

dataset$time<-as.Date(dataset$time,format = "%d-%m-%Y")

head(dataset$time)

year=as.numeric(format(dataset$time,"%Y"))
month=as.numeric(format(dataset$time,"%m"))
day=as.numeric(format(dataset$time,"%d"))
dataset<-cbind(dataset,year,month,day)

print(dataset)

yearly_averages <- dataset %>%
    group_by(year) %>%
    summarise(average_temperature = mean(tavg, na.rm = TRUE))

print(yearly_averages)


# Plot the temperature trends
yearly_averages %>%
    ggplot(aes(x = year, y = average_temperature)) +
    geom_line(col="orange",lwd=1) +
    scale_x_continuous(breaks = 1990:2022  ,labels = 1990:2022) +
    scale_y_continuous(limits = c(20, 30)) +
    labs(title = 'Yearly Average Temperature Trends',
         x = 'Year',
         y = 'Temperature in celsius') +
    theme_minimal()


yearly_max <- dataset %>%
    group_by(year) %>%
    summarise(max_temperature = max(tmax, na.rm = TRUE))

View(yearly_max)

# Plot the temperature trends
yearly_max %>%
    ggplot(aes(x = year, y = max_temperature)) +
    geom_line(col="red",lwd=1) +
    scale_x_continuous(breaks = 1990:2022  ,labels = 1990:2022) +
    scale_y_continuous(limits = c(40, 50)) +
    labs(title = 'Yearly Maximum Temperature Trends',
         x = 'Year',
         y = 'Temperature') +
    theme_minimal()



# To identify the hottest and coldest months
max_temp_month <- dataset %>%
    group_by(month) %>%
    summarise(max_temperature = max(tmax, na.rm = TRUE))
    

View(max_temp_month)

ggplot(max_temp_month, aes(x = month, y = max_temperature)) +
    geom_line() +
    geom_point(color = "red",size=2) +
    scale_x_continuous(breaks = 1:12  ,labels = c(month.name[1:12])) +
    scale_y_continuous(limits = c(20, 50)) +
    labs(title = " Max Temperature Recorded for each month about 30 years", 
         x = "Month",
         y = "Temperature")+
    theme_minimal()


min_temp_month <- dataset %>%
    group_by(month) %>%
    summarise(min_temperature = min(tmin, na.rm = TRUE))

View(min_temp_month)

ggplot(min_temp_month, aes(x = month, y = min_temperature)) +
    geom_line() +
    geom_point(color = "red",size=2) +
    scale_x_continuous(breaks = 1:12  ,labels = c(month.name[1:12])) +
    scale_y_continuous(limits = c(0, 30)) +
    labs(title = " Min Temperature Recorded for each month about 30 years", 
         x = "Month", 
         y = "Temperature")+
    theme_minimal()





mydata=dataset[which(dataset$year==1990),]

View(mydata)

mydata2021=subset(dataset,subset=dataset$time>='2021-01-01' & dataset$time<='2021-12-31')


View(mydata2021)

plot(mydata[,1],mydata[,2],xlab="month",ylab="tavg",main = "1990 and 2021 Temperature comparision",type="l",col="orange")
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



#rain prediction


weather_data <- read.csv(paste(proj.path,'/IndianWeather.csv',sep=''))

View(weather_data)

x<-as.Date(weather_data$last_updated)
year<-as.numeric(format(x,'%Y'))
month<-as.numeric(format(x,'%m'))
day<-as.numeric(format(x,'%d'))
weather_data=cbind(weather_data,year,month,day)

y=levels(factor(weather_data$condition_text))
print(y)
weather_data$condition_text=factor(weather_data$condition_text,
                                   levels = y,
                                   labels = 1:33)

View(weather_data[,c(2,4,5,8,9,11,13,17,19,20,21)])


split <- sample.split(weather_data$precip_mm, SplitRatio = 0.8)
train_data <- subset(weather_data,split==TRUE)
test_data <- subset(weather_data,split==FALSE)

View(train_data)

View(test_data)

model <- lm( precip_mm ~ temperature_celsius + latitude + longitude + condition_text + pressure_in + humidity + wind_kph + cloud, data = weather_data)

predictions <- predict(model, test_data)

par(mfrow=c(1,1))
rmse <- rmse(test_data$precip_mm,predictions)
cat("Root Mean Squared Error (RMSE):", rmse, "\n")

plot(test_data$precip_mm,col="red",ylab="precipitation",main = "Comparision")
    lines(predictions)
    legend(x = "topright", box.col = "brown", 
           bg ="white", box.lwd = 2 , title="Comparision",  
           legend=c("Original", "Predicted"),  
           fill = c("red","black"))


    
    
