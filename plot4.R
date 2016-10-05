# Data Exploratory analysis. Coursera
#script for ploting an histogram for Global Active Power (kw)

plot4 <- function(){
     ##requires dplyr and lubridate library
     
     
     #reading and filtering data (read 10 rows to check names and skip, seps, etc)
     datos <<- read.table("household_power_consumption.txt", sep = ";", 
                          header = TRUE, stringsAsFactors = FALSE, na.strings = "?")
     #changing date class to date
     datos[,1] <- dmy(datos[,1])
     datos[,2] <- hms(datos[,2])
     

     plot04 <<- datos%>%
          filter(Date >= ymd(20070201) & Date <= ymd(20070202))%>%
          select(Date, Time, Global_active_power, Sub_metering_1, 
                 Sub_metering_2, Sub_metering_3, Global_reactive_power, Voltage)
     
     #2 rows, 2 columns
     par(mfrow=c(2,2), mar=c(2,4,2,1))
     #Plot 1 of 4
     plot(plot04$Global_active_power, type = "l", 
          ylab = "Global Active Power (kilowatts)", xlab = "", axes = FALSE, 
          col = "red")
     axis(side = 1, at = c(1,nrow(plot04)),  
          labels = c(as.character(wday(plot04[1,1], label = TRUE)),
           as.character(wday(plot04[nrow(plot04),1],label = T))))
     axis(2)
     box()
     
     #Plot 2 of 4
     plot(plot04$Voltage, type = "l", 
          ylab = "Voltage", xlab = "", axes = FALSE, col = "black")
     axis(side = 1, at = c(1,nrow(plot04)),  
          labels = c(as.character(wday(plot04[1,1], label = TRUE)),
           as.character(wday(plot04[nrow(plot04),1],label = T))))
     axis(2)
     box()
     
     #Plot 3 of 4
     #creating histogram and saving as png
     plot(plot04$Sub_metering_1, type = "l", col= "black", 
                       ylab = "Energy sub metering",xlab = "", axes = FALSE)
     lines(plot04$Sub_metering_2, type = "l", col= "red")
     lines(plot04$Sub_metering_3, type = "l", col = "blue")
     axis(side = 1, at = c(1,nrow(plot04)),  
          labels = c(as.character(wday(plot04[1,1], label = TRUE)),
           as.character(wday(plot04[nrow(plot04),1],label = T))))
     axis(2)
     box()
     legend("topright", legend = names(plot04)[4:6],
            col = c("black","red","blue"), pch = "____")
     
     #Plot 4 of 4
     plot(plot04$Global_reactive_power, type = "l", 
          ylab = "Global Reactive Power", xlab = "", axes = FALSE, col = "black")
     axis(side = 1, at = c(1,nrow(plot04)),  
          labels = c(as.character(wday(plot04[1,1], label = TRUE)),
                     as.character(wday(plot04[nrow(plot04),1],label = T))))
     axis(2)
     box()
     
     #creating png
     dev.copy(png, filename = "plot4.png",width = 480, height = 480)
     dev.off()
}