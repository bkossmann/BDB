library(dplyr)
library(readr)
library(mclust)

setwd("~/Desktop/world-development-indicators")

indicators <- read_csv("Indicators.csv")

#The dataset includes regional data, which is not of interest here since we're curious about specific countries
not_countries <- list("Arab World", "Caribbean small states", "Central Europe and the Baltics", "Channel Islands", "Dominica", "East Asia & Pacific (all income levels)", "East Asia & Pacific (developing only)", "Europe & Central Asia (all income levels)", "Europe & Central Asia (developing only)", "European Union", "Fragile and conflict affected situations", "Heavily indebted poor countries (HIPC)", "High income", "High income: nonOECD", "High income: OECD", "Latin America & Caribbean (all income levels)", "Latin America & Caribbean (developing only)", "Least developed countries: UN classification", "Low & middle income", "Low income", "Lower middle income", "Middle East & North Africa (all income levels)", "Middle East & North Africa (developing only)", "Middle income", "OECD members", "Other small states", "Sub-Saharan Africa (all income levels)", "Sub-Saharan Africa (developing only)", "West Bank and Gaza", "World", "Euro area", "North America", "Pacific island small states", "Small states", "South Asia", "Upper middle income")

country_indicators <- indicators[ ! indicators$CountryName %in% not_countries, ]

#2012 was the most recent year that per capita electricity use was reported
country_indicators_2012 <- filter(country_indicators, Year == "2012")

country_idc_table_2012 <- split(country_indicators_2012, country_indicators_2012$IndicatorCode)

#EG.USE.ELEC.KH.PC electric power consumption per capita
#NY.GDP.PCAP.CD GDP per capita
elec_gdppc <- merge(country_idc_table_2012$NY.GDP.PCAP.CD, country_idc_table_2012$EG.USE.ELEC.KH.PC, by="CountryCode")

egdf <- data.frame(elec = elec_gdppc$Value.y, gdp = elec_gdppc$Value.x, CountryName = elec_gdppc$CountryName.x)

elecgdp_clust <- Mclust(data.frame(egdf$elec, egdf$gdp))

country_classification <- data.frame(CountryName = elec_gdppc$CountryName.x, class = elecgdp_clust$classification)

elecgdp_classified <- merge(egdf, country_classification, by = "CountryName")
elecgdp_classified[elecgdp_classified$class == "1", c("elec", "gdp")]

#cluster_means <- data.frame(elec = elecgdp_clust$parameters$mean[1,], gdp = elecgdp_clust$parameters$mean[2,])

#Check linearity of underdeveloped -> developing cluster means

dev_lin <- lm(elecgdp_classified[elecgdp_classified$class %in% c(2,3,6), "gdp"]~
                elecgdp_classified[elecgdp_classified$class %in% c(2,3,6), "elec"])
underdev_lin <- lm(elecgdp_classified[elecgdp_classified$class %in% c(1,3,4), "gdp"]~
                elecgdp_classified[elecgdp_classified$class %in% c(1,3,4), "elec"])
all_lin <- lm(elecgdp_classified[!elecgdp_classified$class == "5", "gdp"]~
                elecgdp_classified[!elecgdp_classified$class == "5", "elec"])

bk_rmse <- function(slope, intercept, x, y) {
  se <- list()
  for (i in seq(1,length(x))) {
    e = ((x[i]*slope + intercept) - y[i]) ^ 2
    se <- c(se, e)
  }
  result = (mean(unlist(se))) ^ (1/2)
  return(result)
}

rmse_dev<- bk_rmse(dev_lin$coefficients[2], dev_lin$coefficients[1],
                     elecgdp_classified[elecgdp_classified$class %in% c(2,3,6), c("elec")],
                     elecgdp_classified[elecgdp_classified$class %in% c(2,3,6), c("gdp")])


rmse_underdev <- bk_rmse(underdev_lin$coefficients[2], underdev_lin$coefficients[1],
                        elecgdp_classified[elecgdp_classified$class %in% c(1,3,4), c("elec")],
                        elecgdp_classified[elecgdp_classified$class %in% c(1,3,4), c("gdp")])

rmse_all_dev <- bk_rmse(all_lin$coefficients[2], all_lin$coefficients[1],
                    elecgdp_classified[elecgdp_classified$class %in% c(2,3,6), "elec"],
                    elecgdp_classified[elecgdp_classified$class %in% c(2,3,6), "gdp"])

rmse_all_underdev <- bk_rmse(all_lin$coefficients[2], all_lin$coefficients[1],
                        elecgdp_classified[elecgdp_classified$class %in% c(1,3,4), "elec"],
                        elecgdp_classified[elecgdp_classified$class %in% c(1,3,4), "gdp"])

#plot(elecgdp_clust, xlab="Per Capita Electricity Consumption (kWh)", ylab="Per Capita GDP (2015 USD)")
#abline(dev_lin, lty=5)
#abline(underdev_lin, lty=6)
