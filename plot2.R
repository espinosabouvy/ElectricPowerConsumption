# Data Exploratory analysis. Coursera
#script for ploting in time 

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
     
     #reset par if building many plots and scripts
     par(mfrow=c(1,1), mar=c(5.1,4.1,4.1,2.1))
     
     #creating histogram and saving as png
     plot(plot02$Global_active_power, type = "l", 
          ylab = "Global Active Power (kilowatts)", xlab = "", axes = FALSE, col = "red")
     axis(side = 1, at = c(1,nrow(plot02)),  
          labels = c(as.character(wday(plot02[1,1], 
          label = TRUE)),
          as.character(wday(plot02[nrow(plot02),1],label = T))))
     axis(2)
     box()
     
     #creating png
     dev.copy(png, filename = "plot2.png",width = 480, height = 480)
     dev.off()
}