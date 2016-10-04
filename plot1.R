# Data Exploratory analysis. Coursera
#script for ploting an histogram for Global Active Power (kw)

plot1 <- function(){
     ##requires dplyr and lubridate library

     #reading and filtering data (read 10 rows to check names and skip, seps, etc)
     datos <<- read.table("household_power_consumption.txt", sep = ";", 
               header = TRUE, stringsAsFactors = FALSE, na.strings = "?")
     #changing date class to date
     datos[,1] <- dmy(datos[,1])
     #filter interesting data
     plot01 <<- datos%>%
          filter(Date >= ymd(20070201) & Date <= ymd(20070202))%>%
          select(Global_active_power)
     
     #creating histogram and saving as png
     hist(plot01$Global_active_power, col = "green", 
          main = "Global Active Power", xlab = "Global Active Power (kilowatts)")
     dev.copy(png, filename = "plot1.png",width = 480, height = 480)
     dev.off()
}