library(dplyr)
library(readr)
library(MASS)

setwd("~/Desktop/world-development-indicators")

indicators <- read_csv("Indicators.csv")

#The dataset includes regional data, which is not of interest here since we're curious about specific countries
not_countries <- list("Arab World", "Caribbean small states", "Central Europe and the Baltics", "Channel Islands", "Dominica", "East Asia & Pacific (all income levels)", "East Asia & Pacific (developing only)", "Europe & Central Asia (all income levels)", "Europe & Central Asia (developing only)", "European Union", "Fragile and conflict affected situations", "Heavily indebted poor countries (HIPC)", "High income", "High income: nonOECD", "High income: OECD", "Latin America & Caribbean (all income levels)", "Latin America & Caribbean (developing only)", "Least developed countries: UN classification", "Low & middle income", "Low income", "Lower middle income", "Middle East & North Africa (all income levels)", "Middle East & North Africa (developing only)", "Middle income", "OECD members", "Other small states", "Sub-Saharan Africa (all income levels)", "Sub-Saharan Africa (developing only)", "West Bank and Gaza", "World", "Euro area", "North America", "Pacific island small states", "Small states", "South Asia", "Upper middle income")

country_indicators <- indicators[ ! indicators$CountryName %in% not_countries, ]

#2012 was the most recent year that per capita electricity use was reported
country_indicators_2012 <- filter(country_indicators, Year == "2012")

country_idc_table_2012 <- split(country_indicators_2012, country_indicators_2012$IndicatorCode)

merge_tables <- function(idc1, idc2){ new_table <- merge(idc1, idc2, by="CountryCode"); return(new_table)}

#EG.USE.ELEC.KH.PC electric power consumption per capita
#NY.GDP.PCAP.CD GDP per capita
elec_gdppc <- merge_tables(country_idc_table_2012$NY.GDP.PCAP.CD, country_idc_table_2012$EG.USE.ELEC.KH.PC)

#Let's see what a double log plot of per capita GDP vs. per capita electricity use looks like...
log_elec = unlist(lapply(elec_gdppc$Value.y, log10))

log_gdp = unlist(lapply(elec_gdppc$Value.x, log10))

logelec_gdp = data.frame( logelec = log_elec, loggdp = log_gdp)

#Try fitting to normal distribution
fitdistr(log_gdp, "normal")

#Plot the histogram
h<-hist(log_gdp, main="", xlab="log Per Capita GDP (2015 USD)", col="blue", breaks=10)
h<-hist(log_gdp, main="", xlab="log Per Capita GDP (2015 USD)", col="blue", breaks=20)
xfit<-seq(min(log_gdp), max(log_gdp), length = 1320)
yfit<-dnorm(xfit, mean=3.89, sd=0.59)
yfit<-yfit*diff(h$mids[1:2])*length(log_gdp)
lines(xfit,yfit, col="green")

#Who is in the oversampled bin?
filter(elec_gdppc, elec_gdppc$Value.x <= 10^4.7, elec_gdppc$Value.x > 10^4.6)[,c("CountryName.x", "IndicatorName.x","Value.x")]

#Let's single out the EU9
EU_9 = list("Austria", "Belgium", "Germany", "Finland", "France", "United Kingdom", "Ireland", "Iceland", "Netherlands")

EU9_indicators <- indicators[ indicators$CountryName %in% EU_9, ]

#Maybe only data after 1970 to allow recovery time from WWII
GDPPC_1970 <- EU9_indicators[ EU9_indicators$Year > "1970" & EU9_indicators$Year <= "2012" & EU9_indicators$IndicatorCode == "NY.GDP.PCAP.CD", ]

GDPPC_table <- split(GDPPC_1970, GDPPC_1970$CountryCode)

#There is probably a better way to do this
plot(GDPPC_table$AUT$Year, GDPPC_table$AUT$Value, col=2, pch=19, cex=0.1, xlab="Year", ylab="Per Capita GDP (2015 USD)")
lines(GDPPC_table$AUT$Year, GDPPC_table$AUT$Value, col=2)
points(GDPPC_table$BEL$Year, GDPPC_table$BEL$Value, col=3, pch=19, cex=0.1)
lines(GDPPC_table$BEL$Year, GDPPC_table$BEL$Value, col=3)
points(GDPPC_table$FIN$Year, GDPPC_table$FIN$Value, col=4, pch=19, cex=0.1)
lines(GDPPC_table$FIN$Year, GDPPC_table$FIN$Value, col=4)
points(GDPPC_table$FRA$Year, GDPPC_table$FRA$Value, col=5, pch=19, cex=0.1)
lines(GDPPC_table$FRA$Year, GDPPC_table$FRA$Value, col=5)
points(GDPPC_table$DEU$Year, GDPPC_table$DEU$Value, col=6, pch=19, cex=0.1)
lines(GDPPC_table$DEU$Year, GDPPC_table$DEU$Value, col=6)
points(GDPPC_table$ISL$Year, GDPPC_table$ISL$Value, col=7, pch=19, cex=0.1)
lines(GDPPC_table$ISL$Year, GDPPC_table$ISL$Value, col=7)
points(GDPPC_table$IRL$Year, GDPPC_table$IRL$Value, col=8, pch=19, cex=0.1)
lines(GDPPC_table$IRL$Year, GDPPC_table$IRL$Value, col=8)
points(GDPPC_table$NLD$Year, GDPPC_table$NLD$Value, col=9, pch=19, cex=0.1)
lines(GDPPC_table$NLD$Year, GDPPC_table$NLD$Value, col=9)
points(GDPPC_table$GBR$Year, GDPPC_table$GBR$Value, col=10, pch=19, cex=0.1)
lines(GDPPC_table$GBR$Year, GDPPC_table$GBR$Value, col=10)
legend(1975, 54000, c("AUT", "BEL", "FIN", "FRA", "DEU", "ISL", "IRL", "NLD", "GBR"), lty=c(1,1), lwd=c(2.5,2.5), col=c(2,3,4,5,6,7,8,9,10), bty="n", y.intersp = 1.5)

#Plot points for the world PCGDP for reference
world_indicators <- indicators[ indicators$CountryName == "World", ]
world_gdp <- world_indicators[ world_indicators$Year > "1970" & world_indicators$Year <= "2012" & world_indicators$IndicatorCode == "NY.GDP.PCAP.CD", ]
points(world_gdp$Year, world_gdp$Value)