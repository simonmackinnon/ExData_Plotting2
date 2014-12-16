# set this directory to working directory
this.dir <- dirname(parent.frame(2)$ofile) 
setwd(this.dir) 

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

if (!exists("NEI") | !exists("SCC"))
{
  ## This first line will likely take a few seconds. Be patient!
  print ("Reading data, this may take a few seconds...")
  NEI <- readRDS(summaryFileUrl)
  SCC <- readRDS(classificationsFileUrl)
  print ("Done reading data!")
}

#2. Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008? 
#   Use the base plotting system to make a plot answering this question.

# subset the data for Maryland
MarylandNEI = subset(NEI, fips == "24510")

# Aggregate the data emissions for each year
sumMarylandNEI <- MarylandNEI[c("year", "Emissions")]
sumMarylandNEI <- aggregate(x = sumMarylandNEI, by = list(MarylandNEI$year),  FUN = function(x){sum(as.numeric(x))})
sumMarylandNEI$year <- sumMarylandNEI$Group.1
sumMarylandNEI$Group.1 <- NULL

# use the png device, set size 720 x 480
png("plot2.png", 
    width = 720,
    height = 480,
    units = "px")

#set the margins
par(mar=c(5, 5, 5, 5))

# plot the points with lines in between
plot(sumMarylandNEI$year, 
     sumMarylandNEI$Emissions, 
     type = 'b', 
     main="Total Emissions in Maryland", 
     xlab="Year",      
     ylab="Emissions(PM2.5)",     
     col = "blue")

# get the trend line formula
regEq = lm(sumMarylandNEI$Emissions ~ sumMarylandNEI$year)

# add the trendline to the plot
abline(regEq, col = "red")

# add the formula as text to the plot
text(x = 2003,
     y = 1.25 * (max(sumMarylandNEI$Emissions) + min(sumMarylandNEI$Emissions)) / 2, 
     labels = c(paste ("y = ", 
                       as.character(round(regEq$coefficients[2], digits = 2)), 
                       "x",
                       sep = "")),
     col = "red")
text(x = 2003,
     y = 1.225 * (max(sumMarylandNEI$Emissions) + min(sumMarylandNEI$Emissions)) / 2,  
     labels = c(paste ("+ ", 
                       as.character(round(regEq$coefficients[1]), digits = 2), 
                       sep = "")),
     col = "red")

dev.off()