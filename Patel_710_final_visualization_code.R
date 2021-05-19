# Download the necessary packages
list.of.packages <- c("readr", "dplyr", "tidyverse", "tidyr", "countrycode", "ggplot2", "hrbrthemes", "gapminder", "shadowtext", "rworldmap", "RColorBrewer")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Load required R packages
library (readr)
library (dplyr)
library(tidyverse)
library(tidyr)
library(countrycode)
library(ggplot2)
library(hrbrthemes)
library(gapminder)
library(shadowtext)
library(rworldmap)
library(RColorBrewer)

###########################################################################################################
### DATA MANAGEMENT
###########################################################################################################

# Read in Covid Data
# 215
urlfile="https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/latest/owid-covid-latest.csv"
dat<-read.csv(url(urlfile))
summary(dat)

#204
# remove rows with "OWID_" string
dat2<- dat[- grep("OWID_", dat$iso_code),]

#150
# removed ppl without vacc data
vac <- dat2 %>% filter(!is.na(people_vaccinated_per_hundred))

########################################################################################################
########################################################################################################

#Download map data
world_map <- map_data("world")

#merging in isocode data to the worldmap file using the countrycode package
world_map$isocode <- countrycode(world_map[,"region"], origin = 'country.name', destination = 'iso3c')

#Merge map data with vaccine data
vac.map <- inner_join(vac, world_map, by = c("iso_code" = "isocode"))
#colnames(vac.map)[which(names(vac.map) == "total_vaccinations_per_hundred")] <- "Vaccine_doses_per_100_people"

# compare location and region and identify where they are different
vac.map$compare = ifelse(vac.map$location==vac.map$region,"Yes","No")
Diff <- subset(vac.map, compare=="No")

vac.map$region2 = ifelse(vac.map$location==vac.map$region, vac.map$location, vac.map$region)
vac.map$compare2 = ifelse(vac.map$region2==vac.map$region,"Yes","No")
Diff2 <- subset(vac.map, compare2=="No")

# remove duplicates from vac.map to get unique rows with merged world.map values
#149
vac.map_sh= vac.map %>% 
  # Base the removal on the "Age" column
  distinct(region2, .keep_all = TRUE)

###########################################################################################################
### PLOT 1: WORLD MAP
###########################################################################################################

# Read in Covid Data
urlfile="https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv"
old<-read.csv(url(urlfile))

# Filter data to use the most recent date
old2 = old %>% group_by(location) %>% filter(date == max(date))

#Download map data
map <- map_data("world")

#standardize iso code for world map data
map$isocode <- countrycode(map[,"region"], origin = 'country.name', destination = 'iso3c')

#Merge map data with vaccine data
vac.join <- inner_join(old2, map, by = c("iso_code" = "isocode"))
colnames(vac.join)[which(names(vac.join) == "total_vaccinations_per_hundred")] <- "Vaccine_doses_per_100_people"

# min / max orders or top 20 & bottom 20 vaccinations
min = old2[order(old2$total_vaccinations_per_hundred), ]
min2 = min[1:20, 1]

max = old2[order(old2$total_vaccinations_per_hundred, decreasing=TRUE), ]
max2 = max[1:20, 1]

# mean values of the latitude and longitude
region.lab.data <- vac.join %>% group_by(region) %>% summarise(long = mean(long), lat = mean(lat))

#filter your wanted countries
region.lab.data.min = region.lab.data %>% filter(region %in% c("Cameroon", "DemocraticRepublicofCongo", "Libya", "Niger", "SouthSudan", "Syria", "Armenia", "Kyrgyzstan", "PapuaNewGuinea", "Zambia", "Mauritania", "Algeria", "Mozambique", "Timor", "Taiwan", "Mali", "Congo", "Sudan", "Ethiopia", "CapeVerde", "Gibraltar", "Seychelles", "Falkland Islands", "Israel", "United Arab Emirates", "Cayman Islands", "Bermuda", "Isle of Man", "San Marino", "Jersey", "Wales", "Chile", "Saint Helena", "Malta", "Scotland", "Maldives", "Bahrain", "England", "Aruba", "USA"))

#insert your labels in plot
plabels <- geom_shadowtext(data = region.lab.data.min, fontface = "bold", color = "#747474", bg.color = "white", aes(label=region, x=long, y=lat), size = 3, inherit.aes=FALSE)

#World map of vaccines per hundred with white outline
p <- ggplot(vac.join, aes(map_id = region, fill = Vaccine_doses_per_100_people))+
  geom_map(map = vac.join,  color = "grey")+
  expand_limits(x = vac.join$long, y = vac.join$lat)+
  scale_fill_distiller(palette = "Purples", direction = 1) +
  labs(title = "Global Coronavirus Vaccination Count", x = "Latitude", y = "Longitude") + theme_void()

p + plabels +  theme_void()

###########################################################################################################
### PLOT 2: BAR CHART BY CONTINENT
###########################################################################################################

positions2 <- c("Oceania", "Africa", "Asia", "South America", "North America", "Europe")

ggplot(vac.map, aes(x = new_cases_per_million, y = continent, main="Vaccination Doses by continent per 100 people")) + geom_bar(stat = "identity", fill = "slateblue3") + scale_y_discrete(limits = positions2, name="Continents")+ scale_x_continuous(name="New Cases per million people", labels=scales::comma) + theme(axis.text=element_text(size=15), axis.title=element_text(size=17, face="bold")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))

###########################################################################################################
### PLOT 3: DONUT CHART BY CONTINENT
###########################################################################################################

aggregate(vac.map_sh[, "people_vaccinated_per_hundred"], list(vac.map_sh$continent), sum)
aggregate(data[, "fraction"], list(data$continent), sum)

# Create dataset with parameters 
data <- data.frame(
  Continents =c("Oceania","Africa", "South America", "Asia", "North America", "Europe"),
  count=c(10.33, 117.96, 152.25, 413.98, 588.67, 1161.01)
)

# Compute percentages
data$fraction = data$count / sum(data$count)

# Compute the cumulative percentages (top of each rectangle)
data$ymax = cumsum(data$fraction)

# Compute the bottom of each rectangle
data$ymin = c(0, head(data$ymax, n=-1))

# Compute label position
data$labelPosition <- (data$ymax + data$ymin) / 2

# Make the plot
ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Continents)) +
  geom_rect() + # x here controls label position (inner / outer)
  scale_fill_brewer(palette = "Purples") + 
  coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
  xlim(c(2, 4)) +  theme_void() + labs(title = "Global Vaccinations per Hundred People") + theme(legend.text =element_text(size=17), legend.title=element_text(size=19, face="bold"), plot.title = element_text(size=21, face="bold", vjust = 10, hjust = 1))

###########################################################################################################
### PLOT 4: LOLIPOP PLOT
###########################################################################################################

#Download country name library to use for continent based grouping
gap_with_colors <- data.frame(gapminder, cc = I(country_colors[match(gapminder$country, names(country_colors))]))

#Subset and clean up data 
gap_with_colors2 <- subset(gap_with_colors,select=c(country,continent, cc))
gap_with_colors3 <-unique(gap_with_colors2[,1:3])

#Merge data with dataset 
allyearscolors <- merge(vac,gap_with_colors3, by.x ='location', by.y = 'country')

small2 = allyearscolors %>% filter(new_deaths_smoothed != 0)

###Lollipop chart
lollipp <-ggplot(small2, aes(x=location, y=new_deaths)) +
  geom_segment( aes(x=location, xend=location, y=0, yend=new_deaths), color="mediumpurple") +
  geom_point( color="mediumpurple", size=4, alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  ) + scale_x_discrete(limits=rev, name="Countries")  + scale_y_continuous(name="Number of New Deaths on May 4, 2021") + theme(axis.title=element_text(size=12, face="bold"))

lollipp
