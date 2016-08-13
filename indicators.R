library(dplyr)
library(readr)

setwd("~/Desktop/world-development-indicators-2")

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
log_elec = lapply(elec_gdppc$Value.y, log10)

log_gdp = lapply(elec_gdppc$Value.x, log10)

logelec_gdp = data.frame( logelec = unlist(log_elec), loggdp = unlist(log_gdp))

plot(log_elec, log_gdp, xlab="log Per Capita Electricity Consumption (kWh)", ylab ="log Per Capita GDP (2015 USD)", pch=19)

log_regression <- lm(logelec_gdp$loggdp ~ logelec_gdp$logelec)

abline(log_regression, col="red")

summary(log_regression)

#What about a linear plot?
plot(elec_gdppc$Value.y, elec_gdppc$Value.x, xlab="Per Capita Electricity Consumption (kWh)", ylab ="Per Capita GDP (2015 USD)", pch=19)

regression <- lm(elec_gdppc$Value.x ~ elec_gdppc$Value.y)

abline(regression, col="red")

summary(regression)

#What happens if we throw out Iceland?
no_iceland <- filter(elec_gdppc, CountryCode != "ISL")

plot(no_iceland$Value.y, no_iceland$Value.x, xlab="Per Capita Electricity Consumption (kWh)", ylab ="Per Capita GDP (2015 USD)", pch=19)

noice_regress <- lm(no_iceland$Value.x ~ no_iceland$Value.y)

abline(noice_regress, col="red")

summary(noice_regress)

noice_logelec = lapply(no_iceland$Value.y, log10)

noice_loggdp = lapply(no_iceland$Value.x, log10)

noice_logelecgdp <- data.frame( logelec = unlist(noice_logelec), loggdp = unlist(noice_loggdp))

plot(noice_logelecgdp$logelec, noice_logelecgdp$loggdp, xlab="log Per Capita Electricity Consumption (kWh)", ylab ="log Per Capita GDP (2015 USD)", pch=19)

noice_log_regress <- lm(noice_logelecgdp$loggdp ~ noice_logelecgdp$logelec)

abline(noice_log_regress, col="red")

summary(noice_log_regress)

#histograms
hist(elec_gdppc$Value.y, main="", xlab="Per Capita Electricity Consumption (kWh)", col="green", breaks=10)

hist(unlist(log_elec), main="", xlab="log Per Capita Electricity Consumption (kWh)", col="blue", breaks=10)

hist(elec_gdppc$Value.x, main="", xlab="Per Capita GDP (USD)", col="green", breaks=10)

hist(unlist(log_gdp), main="", xlab="log Per Capita GDP (USD)", col="blue", breaks=10)
