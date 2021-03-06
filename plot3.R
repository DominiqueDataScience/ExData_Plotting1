#   This 'plot3.R'-script will download the power consumption dataset, if it is missing.
#   Then it will read in the data, take the wanted subset and create a plot where
#   the 3 different sub_metering measurements are plotted against the time of days of interest,
#   all 3 will be in a single graph with different colors.

plot3 <- function()
{
    #### First the required packages will be installed and loaded ####
    
    ## Packages used: dplyr, plyr
    # Check&Install function for packages    
    packages<-function(x){
        x<-as.character(match.call()[[2]])
        if (!require(x,character.only=TRUE)){
            install.packages(pkgs=x,repos="http://cran.r-project.org")
            require(x,character.only=TRUE)
        }
    }
    
    # Installation of required packages, only if they are not installed yet.
    packages("plyr")
    packages("dplyr")
    
    # Load the required packages and suppress all messages
    suppressMessages(library(plyr))
    suppressMessages(library(dplyr))
    
    
    #### Get the data & Read it all in  ####
    
    # Download data and extract it, unless the required file is already there
    required_file <- c("./household_power_consumption.txt")
    
    if (!file.exists(required_file)) {
        url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
        target_file <- "./exdata%2Fdata%2Fhousehold_power_consumption.zip"
        if (grepl("Windows", sessionInfo()$running)) download.file(url, target_file) else {
            download.file(url, target_file, method = "curl")
        }
        unzip(target_file)
    }
    
    # First just read all the data in and then subset to only 1st and 2nd of February 2007
    powerdata <- read.csv(file = "./household_power_consumption.txt", sep = ";")
    powerdata <- powerdata[powerdata$Date == "1/2/2007" | powerdata$Date == "2/2/2007",]
    
    # Convert the variables Date and Time to appropriate data/time format and place in DateTime variable
    powerdata$Date <- as.Date(powerdata$Date, format = "%d/%m/%Y")
    powerdata$Date <- paste(powerdata$Date, powerdata$Time)
    # Also remove the old, unused Time variable
    powerdata <- rename(powerdata, DateTime = Date) %>% select(., -Time)
    powerdata$DateTime <- strptime(powerdata$DateTime, format = "%Y-%m-%d %H:%M:%S", tz="UTC")
    
    # Convert Sub_metering_1&2 variables to a numeric class (3 is already numeric by default normally, but...)
    powerdata$Sub_metering_1 <- as.numeric(as.character(powerdata$Sub_metering_1))
    powerdata$Sub_metering_2 <- as.numeric(as.character(powerdata$Sub_metering_2))
    powerdata$Sub_metering_3 <- as.numeric(as.character(powerdata$Sub_metering_3))
    
    #### Create plot3 ####
    
    # Start by opening png device
    png(file = "plot3.png", width = 480, height = 480)
    
    # Then construct the plot itself
    with(powerdata, plot(DateTime, powerdata$Sub_metering_1, type = "n", ylab = "Energy sub metering", xlab = ""))
    points(powerdata$DateTime, powerdata$Sub_metering_1, type = "l", col = "black")
    points(powerdata$DateTime, powerdata$Sub_metering_2, type = "l", col = "red")
    points(powerdata$DateTime, powerdata$Sub_metering_3, type = "l", col = "blue")
    legend("topright", legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), 
           col = c("black", "red", "blue"), lty = 1)
    
    # And close the device
    dev.off()
}