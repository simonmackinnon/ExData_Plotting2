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

#5. How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City?

#Define emissions from vehicles as SCC$SCC.Level.Two values containing "Vehicle"
SCCVehiclesTF <- grepl("^.*?Vehicle.*?$", SCC$SCC.Level.Two)
SCCVehicles = subset(SCC, SCCVehiclesTF)

# # subset for NEI where NEI$SCC matches SCCVehicles$SCC, and for Baltimore City (fips == "24510")
vehiclesNEI <- subset(NEI, SCC %in% SCCVehicles$SCC & fips == "24510")

# Aggregate per year
vehiclesNEI <- vehiclesNEI[c("year", "Emissions")]
vehiclesNEI <- aggregate (x = vehiclesNEI, by = list(vehiclesNEI$year),  FUN = function(x){sum(as.numeric(x))})
vehiclesNEI$year <- vehiclesNEI$Group.1
vehiclesNEI$Group.1 <- NULL

# use the png device, set size 720 x 480
png("plot5.png", 
    width = 720,
    height = 480,
    units = "px")

#set the margins
par(mar=c(5, 5, 5, 5))

plot (vehiclesNEI$year, 
      vehiclesNEI$Emissions,
      type = 'b', 
      main="Emissions from Motor Vehicle Sources\nin Baltimore City (1999-2008)", 
      xlab="Year",      
      ylab="Emissions(PM2.5)",     
      col = "blue") 

regEq = lm (vehiclesNEI$Emissions ~ vehiclesNEI$year) 

abline (regEq, col = "red")

# add the formula as text to the plot
text(x = 2003,
     y = 1.25 * (max(vehiclesNEI$Emissions) + min(vehiclesNEI$Emissions)) / 2, 
     labels = c(paste ("y = ", 
                       as.character(round(regEq$coefficients[2], digits = 2)), 
                       "x",
                       sep = "")),
     col = "red")
text(x = 2003,
     y = 1.20 * (max(vehiclesNEI$Emissions) + min(vehiclesNEI$Emissions)) / 2,  
     labels = c(paste ("+ ", 
                       as.character(round(regEq$coefficients[1]), digits = 2), 
                       sep = "")),
     col = "red")

dev.off()
