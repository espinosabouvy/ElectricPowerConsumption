# Data Exploratory analysis. Coursera
#script for ploting an histogram for Global Active Power (kw)

plot2 <- function(){
     ##requires dplyr and lubridate library
     
     #reading and filtering data (read 10 rows to check names and skip, seps, etc)
     datos <<- read.table("household_power_consumption.txt", sep = ";", 
                          header = TRUE, stringsAsFactors = FALSE, na.strings = "?")
     #changing date class to date
     datos[,1] <- dmy(datos[,1])
     datos[,2] <- hms(datos[,2])
     #filter interesting data
     plot02 <<- datos%>%
          filter(Date >= ymd(20070201) & Date <= ymd(20070202))%>%
          select(Date, Time, Global_active_power)
     xdata <- unique(wday(plot02$Date, label = T))
     
     
     #creating histogram and saving as png
     plot(plot02$Global_active_power, type = "l", 
          ylab = "Global Active Power (kilowatts)",xaxt="n", col = "magenta")
     axis(side = 1, at = seq(xdata), labels = xdata)
     box()
     
     #creating png
     dev.copy(png, filename = "plot2.png",width = 480, height = 480)
     dev.off()
}