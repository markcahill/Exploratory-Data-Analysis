# load required R libraries
library(reshape2)
library(plyr)
library(ggplot2)

# Loads RDS

NEI <- readRDS("data/summarySCC_PM25.rds")
SCC <- readRDS("data/Source_Classification_Code.rds")

# Samples data for testing
NEIsample <- NEI[sample(nrow(NEI), size = 1000, replace = F), ]

# Aggregates
Emissions <- aggregate(NEI[, 'Emissions'], by = list(NEI$year), FUN = sum)
Emissions$PM <- round(Emissions[, 2] / 1000, 2)

## plot1 - Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
## Using the base plotting system, make a plot showing the total PM2.5 emission from all sources 
## for each of the years 1999, 2002, 2005, and 2008.

png(filename = "plot1.png")
barplot(Emissions$PM, names.arg = Emissions$Group.1, main = expression('Total Emission of PM'[2.5]), xlab = 'Year', ylab = expression(paste('PM', ''[2.5], ' in Kilotons')))
dev.off()

# Subsets data and appends two years in one data frame
MD <- subset(NEI, fips == '24510')

## plot 2 - Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008? 
## Use the base plotting system to make a plot answering this question.

png(filename = 'plot2.png')
barplot(tapply(X = MD$Emissions, INDEX = MD$year, FUN = sum), main = 'Total Emission in Baltimore City, MD', xlab = 'Year', ylab = expression('PM'[2.5]))
dev.off()


## plot 3 - Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
## which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? 
## Which have seen increases in emissions from 1999–2008? 
## Use the ggplot2 plotting system to make a plot answer this question.

# Baltimore City, Maryland == fips 24510
MD <- subset(NEI, fips == 24510)
MD$year <- factor(MD$year, levels = c('1999', '2002', '2005', '2008'))

# Sum up the emmission for each year
MD_Sum <- tapply(MD$Emissions, list(MD$type, MD$year), sum)

# Use melt to reorgainse dataframe
MD_Sum_ordered = melt(MD_Sum)

# Rename Var1 = Type, Var2 = Year, value = Emissions
MD_Sum_ordered <- rename(MD_Sum_ordered, c("Var1"="Type", "Var2"="Year", "value" = "Emissions"))

png('plot3.png', width = 800, height = 500, units = 'px')

ggplot(data=MD_Sum_ordered, aes(x=Year, y=Emissions, group=Type, colour=Type)) +
  geom_line() +
  geom_point() +
  ggtitle('Emissions per Type in Baltimore City, Maryland')

dev.off()

## plot 4 - Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?

# Coal-related SCC
SCC.coal = SCC[grepl("coal", SCC$Short.Name, ignore.case = TRUE), ]

# Merges two data sets
merge <- merge(x = NEI, y = SCC.coal, by = 'SCC')
merge.sum <- aggregate(merge[, 'Emissions'], by = list(merge$year), sum)
colnames(merge.sum) <- c('Year', 'Emissions')

png(filename = 'plot4.png')
ggplot(data = merge.sum, aes(x = Year, y = Emissions / 1000)) + geom_line(aes(group = 1, col = Emissions)) + geom_point(aes(size = 2, col = Emissions)) + ggtitle(expression('Total Emissions of PM'[2.5])) + ylab(expression(paste('PM', ''[2.5], ' in kilotons'))) + geom_text(aes(label = round(Emissions / 1000, digits = 2), size = 2, hjust = 1.5, vjust = 1.5)) + theme(legend.position = 'none') + scale_colour_gradient(low = 'black', high = 'red')
dev.off()


## plot 5 - How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?

# Baltimore City, Maryland == fips
MD.onroad <- subset(NEI, fips == 24510 & type == 'ON-ROAD')

# Aggregates
MD.df <- aggregate(MD.onroad[, 'Emissions'], by = list(MD.onroad$year), sum)
colnames(MD.df) <- c('year', 'Emissions')

png('plot5.png')
ggplot(data = MD.df, aes(x = year, y = Emissions)) + geom_bar(aes(fill = year), stat = "identity") + guides(fill = F) + ggtitle('Total Emissions of Motor Vehicle Sources in Baltimore City, Maryland') + ylab(expression('PM'[2.5])) + xlab('Year') + theme(legend.position = 'none') + geom_text(aes(label = round(Emissions, 0), size = 1, hjust = 0.5, vjust = 2))
dev.off()


## plot 6 - Compare emissions from motor vehicle sources in Baltimore City with emissions 
## from motor vehicle sources in Los Angeles County, California (fips == "06037"). Which city has seen greater changes over time in motor vehicle emissions?

# Baltimore City, Maryland
# Los Angeles County, California
MD.onroad <- subset(NEI, fips == '24510' & type == 'ON-ROAD')
CA.onroad <- subset(NEI, fips == '06037' & type == 'ON-ROAD')

# Aggregates
MD.DF <- aggregate(MD.onroad[, 'Emissions'], by = list(MD.onroad$year), sum)
colnames(MD.DF) <- c('year', 'Emissions')
MD.DF$City <- paste(rep('MD', 4))

CA.DF <- aggregate(CA.onroad[, 'Emissions'], by = list(CA.onroad$year), sum)
colnames(CA.DF) <- c('year', 'Emissions')
CA.DF$City <- paste(rep('CA', 4))

DF <- as.data.frame(rbind(MD.DF, CA.DF))

png('plot6.png')
ggplot(data = DF, aes(x = year, y = Emissions)) + geom_bar(aes(fill = year),stat = "identity") + guides(fill = F) + ggtitle('Total Emissions of Motor Vehicle Sources\nLos Angeles County, California vs. Baltimore City, Maryland') + ylab(expression('PM'[2.5])) + xlab('Year') + theme(legend.position = 'none') + facet_grid(. ~ City) + geom_text(aes(label = round(Emissions, 0), size = 1, hjust = 0.5, vjust = -1))
dev.off()
