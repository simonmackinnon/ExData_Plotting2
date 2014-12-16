# set this directory to working directory
this.dir <- dirname(parent.frame(2)$ofile) 
setwd(this.dir) 

# Get the list of installed packages
packagesDF <- data.frame (installed.packages())

#check if ggplot2 is already installed, if not, install it
if (!("ggplot2" %in% packagesDF$Package))
{
  install.packages ("ggplot2")
}

#Remove no longer needed variable
rm (packagesDF)

#load the ggplot2 library
library(ggplot2)

# download file path
destUrl <- "./FNEI_data.zip"
if (!file.exists(destUrl))
{
  print ("Downloading data, this may take a few seconds...")
  url <- url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"  
  download.file(url, destfile = destUrl)
  print ("Done downloading data!")
}

# location of extracted text files
classificationsFileUrl <- "./Source_Classification_Code.rds"
summaryFileUrl <- "./summarySCC_PM25.rds"

# extract the files into direco if it hasn't been already
if (!file.exists(classificationsFileUrl) | !file.exists(summaryFileUrl))
{
  print ("Unpacking data, this may take a few seconds...")
  unzip(destUrl)
  print ("Done unpacking data!")
}

# extract the files into direco if it hasn't been already
if (!exists("NEI") | !exists("SCC"))
{
  ## This first line will likely take a few seconds. Be patient!
  print ("Reading data, this may take a few seconds...")
  NEI <- readRDS(summaryFileUrl)
  SCC <- readRDS(classificationsFileUrl)
  print ("Done reading data!")
}

#3. Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
#     which of these four sources have seen decreases in emissions from 1999-2008 for Baltimore City? 
#   Which have seen increases in emissions from 1999-2008? 
#   Use the ggplot2 plotting system to make a plot answer this question.

# Aggregate the data emissions for each year
sumBaltimoreCityNEI <- subset(NEI, fips == "24510")
sumBaltimoreCityNEI <- sumBaltimoreCityNEI[c("Emissions", "type", "year")]
sumBaltimoreCityNEI <- aggregate(x = sumBaltimoreCityNEI, 
                                 by = list(sumBaltimoreCityNEI$type, 
                                           sumBaltimoreCityNEI$year),  
                                 FUN = function(x){sum(as.numeric(x))})

sumBaltimoreCityNEI$type <- sumBaltimoreCityNEI$Group.1
sumBaltimoreCityNEI$year <- sumBaltimoreCityNEI$Group.2
sumBaltimoreCityNEI$Group.1 <- NULL
sumBaltimoreCityNEI$Group.2 <- NULL

# use the png device, set size 1045 x 405
png ("plot3.png",
     width = 1045,
     height = 405,
     units = "px")

g <- qplot(year, 
           Emissions, 
           data = sumBaltimoreCityNEI, 
           facets = .~type) 
print (   g + 
          geom_smooth(method = "lm") +
          labs(title = "Emissions in Baltimore City\nby Type") + 
          labs(x = "Year", y = paste("Emissions", expression(PM[2.5]), "(tons)")))

dev.off()
