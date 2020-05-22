library(readr)
library(dplyr)
library(ggplot2)
options(scipen = 999)
#Read in data
temp <- tempfile()
download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip",temp)
NEI<-read_rds("summarySCC_PM25.rds")
SCC<-read_rds("Source_Classification_Code.rds")
unlink(temp)

#Sum total emissions by year
total_em_by_year<-NEI%>%group_by(year)%>%summarise(sums = sum(Emissions))
png("total_emissions.png")

#Plot total emissions
with(total_em_by_year, 
     plot(year, sums, type = "l", 
          main = "Total Emissions by Year", 
          xlab = "Year", 
          ylab = "Total Emissions"))

dev.off()

#Filter and summarise Baltimore emissions data
baltimore<-NEI%>%filter( fips == "24510")%>%
        group_by(year)%>%
        summarise(sums = sum(Emissions))

#Plot data
png("balt_total_emission.png")
with(baltimore, plot(year, sums, type = "l", 
                     main = "Total Emissions in Baltimore by Year",
                     xlab = "Year", 
                     ylab = "Total Emissions"))

dev.off()

#Filter and summarise Baltimore emissions by type
baltimore_by_type<-NEI%>%filter(fips == "24510")%>%
        group_by(type, year)%>%
        summarise(sums = sum(Emissions))

#Plot data
b<-ggplot(data = baltimore_by_type, aes(x = year, y = sums, group = type, color = type))+
        geom_line()+
        labs(x = "Year",
             y = "Total Emissions",
             title = "Total Emissions by Type by Year in Baltimore")
last_plot()
ggsave("emissions_by_type_balt.png", width = 5, height = 5)

#Use coal related SCC to subset NEI and summarise by group
coal_comb_source<-SCC%>%filter(grepl(pattern = "Coal", x = EI.Sector))
coal_scc<-coal_comb_source%>%distinct(SCC)
coal<- subset(NEI, coal_scc$SCC %in% NEI$SCC)
coal_by_year<-coal%>%group_by(year)%>%
        summarise(sums = sum(Emissions))
#Plot data
c<-ggplot(data = coal_by_year, aes(x = year, y = sums))+
        geom_line()+
        labs(x = "Year", 
             y = "Total Emissions",
             title = "Coal Emissions by Year")
#save plot as .png
last_plot()
ggsave("coal_by_year.png", width = 5, height = 5)

#Use motor vehicle related SCC to subset NEI and then summarise by group
motor_vehicle_source<-SCC%>%filter(grepl(pattern = "Mobile Sources", x = SCC.Level.One))
motor_scc<-motor_vehicle_source%>%distinct(SCC)
motor<-subset(NEI, motor_scc$SCC %in% NEI$SCC)
motor_by_year_balt<-motor%>%
        filter(fips == "24510")%>%
        group_by(year)%>%
        summarise(sums = sum(Emissions))

#Plot data
m<-ggplot(data = motor_by_year_balt, aes(x = year, y = sums))+
                 geom_line()+
        labs(x = "Year", 
             y = "Total Emissions", 
             title = "Total Motor Vehicle Emissions in Baltimore by Year")

#Save plot as .png
last_plot()
ggsave("motor_by_year_balt.png", width = 5, height = 5)

#Use previous motor subset and filter for Baltimore and Los Angeles fips
motor_by_year_los_balt<-motor%>%
        filter(fips == "24510" | fips == "06037")%>%
        group_by(fips, year)%>%
        summarise(sums = sum(Emissions))

#Plot data
mlb<-ggplot(data = motor_by_year_los_balt, aes(x = year, y = sums, group = fips, color = fips))+
                          geom_line()+
        labs(x = "Year", 
             y = "Total Emissions", 
             title = "Vehicle Emissions for LA and Baltimore by Year")

#Save plot as .png
last_plot()
ggsave("motor_by_year_los_balt.png", width = 5, height = 5)
