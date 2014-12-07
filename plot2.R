# Exploratory Data Analysis, Project 1, plot2.R

## This function runs the program by calling the appropriate functions.
main <- function() {
    readData()
    subsetData()
    dateTimeConvert()
    makePlot2()
}

## Read in data from already downloaded file
readData <- function() {
    myData <<- read.csv("./household_power_consumption.txt", sep=";",
                  colClasses = c("character","character","numeric","numeric",
                                 "numeric","numeric","numeric","numeric",
                                 "numeric"),
                  strip.white = TRUE, na.strings = c("?",""))
}

## Subset for only Feb 1 - 2, 2007
subsetData <- function() {
    myFilter <- grepl("^1/2/2007$|^2/2/2007$", myData$Date)
    myData <<- myData[myFilter, ]
    rownames(myData) <<- NULL  # Renumber rows starting at 1.
    #myData$Date <<- as.Date(myData$Date, "%d%b%Y")
}

## Add a new POSIXlt column which combines the Date and Time columns.
dateTimeConvert <- function() {
    temp <- paste(myData$Date, myData$Time)
    myData$DateTime <<- as.POSIXlt(temp, format = "%d/%m/%Y %H:%M:%S")    
}

## Second plot for project.
makePlot2 <- function() {
    par(mar = c(5.1, 6.1, 4.1, 3.1)) # Bottom is side 1, left is side 2, ...
    plot(myData$DateTime, myData$Global_active_power, type="l",
         ylab = "Global Active Power (kilowatts)", xlab = "")
    dev.copy(png, file = "plot2.png")
    #png(file = "plot1.png", width = 480, height = 480)
    dev.off()
}