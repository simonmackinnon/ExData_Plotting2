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

#6. Compare emissions from motor vehicle sources in Baltimore City with emissions from motor 
#     vehicle sources in Los Angeles County, California (fips == "06037"). 
#   Which city has seen greater changes over time in motor vehicle emissions?

SCCVehiclesTF <- grepl("^.*?Vehicle.*?$", SCC$SCC.Level.Two)
SCCVehicles = subset(SCC, SCCVehiclesTF)

#Baltimore City
# # subset for NEI where NEI$SCC matches SCCVehicles$SCC
vehiclesBaltimorCityNEI <- subset(NEI, SCC %in% SCCVehicles$SCC & fips == "24510")

# Aggregate per year
vehiclesBaltimorCityNEI <- vehiclesBaltimorCityNEI[c("year", "Emissions")]
vehiclesBaltimorCityNEI <- aggregate (x = vehiclesBaltimorCityNEI, 
                                      by = list(vehiclesBaltimorCityNEI$year),  
                                      FUN = function(x){sum(as.numeric(x))})
vehiclesBaltimorCityNEI$year <- vehiclesBaltimorCityNEI$Group.1
vehiclesBaltimorCityNEI$Group.1 <- NULL

#Los Angeles
# # subset for NEI where NEI$SCC matches SCCVehicles$SCC
vehiclesLosAngelesNEI <- subset(NEI, SCC %in% SCCVehicles$SCC & fips == "06037")

# Aggregate per year
vehiclesLosAngelesNEI <- vehiclesLosAngelesNEI[c("year", "Emissions")]
vehiclesLosAngelesNEI <- aggregate (x = vehiclesLosAngelesNEI, 
                                    by = list(vehiclesLosAngelesNEI$year),  
                                    FUN = function(x){sum(as.numeric(x))})
vehiclesLosAngelesNEI$year <- vehiclesLosAngelesNEI$Group.1
vehiclesLosAngelesNEI$Group.1 <- NULL



regEqBaltimoreCity = lm (vehiclesBaltimorCityNEI$Emissions ~ vehiclesBaltimorCityNEI$year) 

# use the png device, set size 1320 x 500
png("plot6.png", 
    width = 1320,
    height = 500,
    units = "px")

par (mfrow = c(1, 2))

plot (vehiclesBaltimorCityNEI$year, 
      vehiclesBaltimorCityNEI$Emissions,
      type = 'b', 
      main="Emissions from Motor Vehicle Sources\nin Baltimore City (1999-2008)", 
      xlab="Year",      
      ylab="Emissions(PM2.5)",     
      col = "blue") 

abline (regEqBaltimoreCity, col = "red")

# add the formula as text to the plot
text(x = 2003,
     y = 1.25 * (max(vehiclesBaltimorCityNEI$Emissions) + min(vehiclesBaltimorCityNEI$Emissions)) / 2, 
     labels = c(paste ("y = ", 
                       as.character(round(regEqBaltimoreCity$coefficients[2], digits = 2)), 
                       "x",
                       sep = "")),
     col = "red")
text(x = 2003,
     y = 1.20 * (max(vehiclesBaltimorCityNEI$Emissions) + min(vehiclesBaltimorCityNEI$Emissions)) / 2,  
     labels = c(paste ("+ ", 
                       as.character(round(regEqBaltimoreCity$coefficients[1], digits = 2)), 
                       sep = "")),
     col = "red")

regEqLosAngeles = lm (vehiclesLosAngelesNEI$Emissions ~ vehiclesLosAngelesNEI$year) 

plot (vehiclesLosAngelesNEI$year, 
      vehiclesLosAngelesNEI$Emissions,
      type = 'b', 
      main="Emissions from Motor Vehicle Sources\nin Los Angeles (1999-2008)", 
      xlab="Year",      
      ylab="Emissions(PM2.5)",     
      col = "blue") 

abline (regEqLosAngeles, col = "red")

# add the formula as text to the plot
text(x = 2003,
     y = 0.98 * (max(vehiclesLosAngelesNEI$Emissions) + min(vehiclesLosAngelesNEI$Emissions)) / 2, 
     labels = c(paste ("y = ", 
                       as.character(round(regEqLosAngeles$coefficients[2], digits = 2)), 
                       "x",
                       sep = "")),
     col = "red")
text(x = 2003,
     y = 0.97 * (max(vehiclesLosAngelesNEI$Emissions) + min(vehiclesLosAngelesNEI$Emissions)) / 2,  
     labels = c(paste ("+ ", 
                       as.character(round(regEqLosAngeles$coefficients[1], digits = 2)), 
                                    sep = "")),
                col = "red")
     
dev.off()    
