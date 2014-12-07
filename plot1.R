# Exploratory Data Analysis, Project 1, plot1.R

## This function runs the program by calling the appropriate functions.
main <- function() {
    readData()
    subsetData()
    makePlot1()
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

## Plot first histogram and save in file.
makePlot1 <- function() {
    par(mar = c(5.1, 7.1, 4.1, 3.1)) # Bottom is side 1, left is side 2, ...
    hist(myData$Global_active_power, col = "red", main = "Global Active Power",
         xlab = "Global Active Power (kilowatts)")
    dev.copy(png, file = "plot1.png")
    dev.off()
}