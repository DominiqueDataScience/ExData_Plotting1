#   This 'plot1.R'-script will download the power consumption dataset, if it is missing.
#   Then it will read in the data, take the wanted subset and create a plot where
#   the frequency is plotted against the global active power. 

plot1 <- function()
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
    powerdata <- rename(powerdata, DateTime = Date) %>% select(., -Time)
    powerdata$DateTime <- strptime(powerdata$DateTime, format = "%Y-%m-%d %H:%M:%S")
    
    # Convert Global_active_power variable to a numeric class
    powerdata$Global_active_power <- as.numeric(as.character(powerdata$Global_active_power))
    
    
    #### Create plot1 ####
    
    # Start by opening png device
    png(file = "plot1.png", width = 480, height = 480)
    
    # Then construct the plot itself
    hist(powerdata$Global_active_power, col = "red", 
         xlab = "Global Active Power (kilowatts)", main = "Global Active Power")
    
    # And close the device
    dev.off()
}