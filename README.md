# Assignment 1 of Exploratory Data Analysis

## Introduction

This assignment uses data from
the <a href="http://archive.ics.uci.edu/ml/">UC Irvine Machine
Learning Repository</a>, a popular repository for machine learning
datasets. In particular, we will be using the "Individual household
electric power consumption Data Set" which I have made available on
the course web site:


* <b>Dataset</b>: <a href="https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip">Electric power consumption</a> [20Mb]

* <b>Description</b>: Measurements of electric power consumption in
one household with a one-minute sampling rate over a period of almost
4 years. Different electrical quantities and some sub-metering values
are available.


The following descriptions of the 9 variables in the dataset are taken
from
the <a href="https://archive.ics.uci.edu/ml/datasets/Individual+household+electric+power+consumption">UCI
web site</a>:

<ol>
<li><b>Date</b>: Date in format dd/mm/yyyy </li>
<li><b>Time</b>: time in format hh:mm:ss </li>
<li><b>Global_active_power</b>: household global minute-averaged active power (in kilowatt) </li>
<li><b>Global_reactive_power</b>: household global minute-averaged reactive power (in kilowatt) </li>
<li><b>Voltage</b>: minute-averaged voltage (in volt) </li>
<li><b>Global_intensity</b>: household global minute-averaged current intensity (in ampere) </li>
<li><b>Sub_metering_1</b>: energy sub-metering No. 1 (in watt-hour of active energy). It corresponds to the kitchen, containing mainly a dishwasher, an oven and a microwave (hot plates are not electric but gas powered). </li>
<li><b>Sub_metering_2</b>: energy sub-metering No. 2 (in watt-hour of active energy). It corresponds to the laundry room, containing a washing-machine, a tumble-drier, a refrigerator and a light. </li>
<li><b>Sub_metering_3</b>: energy sub-metering No. 3 (in watt-hour of active energy). It corresponds to an electric water-heater and an air-conditioner.</li>
</ol>

## Loading the data

When running the R-scripts, the data will be downloaded if necessary and loaded automatically.
Only data from the dates 2007-02-01 and 2007-02-02 will be used to create the plots.

## Making Plots

The overall goal here was simply to examine how household energy usage
varies over a 2-day period in February, 2007. The plots are already available in this repo, and can easily be reproduced by running the R-scripts.

The plot[1-4].png are created by running the scripts plot[1-4].R respectively.
Each plot will have a width of 480 and a height of 480 and be in the PNG format.

All that needs to be done is:

1. Start R console
2. Make sure working directory is correct and contains the required files
3. Execute following commands:
```{r eval=FALSE}    
    source("./plot1.R")  # Or plot2.R, plot3.R, plot4.R
    plot1()   # Or plot2(), plot3(), plot4()
```
Any needed/missing raw data will be automatically downloaded. If all the required data from `Individual household
electric power consumption Data Set` is already available, then it will not be re-downloaded.

Requirements for this script to be run:
- `plot[1-4].R` scripts themselves
- **R** (version 3.2.1)






