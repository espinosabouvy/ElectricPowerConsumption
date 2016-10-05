# Data Exploratory analysis. Coursera
#script for ploting Sub_metering

plot3 <- function(){
     ##requires dplyr and lubridate library
     
     #reading and filtering data (read 10 rows to check names and skip, seps, etc)
     datos <<- read.table("household_power_consumption.txt", sep = ";", 
                          header = TRUE, stringsAsFactors = FALSE, na.strings = "?")
     #changing date class to date
     datos[,1] <- dmy(datos[,1])
     datos[,2] <- hms(datos[,2])
     
     #reset par if building many plots and scripts
     par(mfrow=c(1,1), mar=c(5.1,4.1,4.1,2.1))
     
     #filter interesting data
     plot03 <<- datos%>%
          filter(Date >= ymd(20070201) & Date <= ymd(20070202))%>%
          select(Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3)
     
     
     #creating histogram and saving as png
     with(plot03, plot(plot03$Sub_metering_1, type = "l", col= "black", 
                       ylab = "Energy sub metering",xlab = "", axes = FALSE))
     with(plot03, lines(Sub_metering_2, type = "l", col= "red"))
     with(plot03, lines(Sub_metering_3, type = "l", col = "blue"))
     axis(side = 1, at = c(1,nrow(plot03)),  labels = c(as.character(wday(plot03[1,1], label = TRUE)),
                    as.character(wday(plot03[nrow(plot03),1],label = T))))
     axis(2)
     box()
     legend("topright", legend = names(plot03)[3:5],col = c("black","red","blue"), pch = "____")
     
     #creating png
     dev.copy(png, filename = "plot3.png",width = 480, height = 480)
     dev.off()
}