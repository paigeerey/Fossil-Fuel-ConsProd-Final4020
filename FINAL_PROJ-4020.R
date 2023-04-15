# Paige Reynolds
# MATH 4020
# FINAL PROJECT

library(MASS)
library(ISLR)
library(ggplot2)
#install.packages("ggplot")
# Load the dataset and observe first rows
data <- read.csv(file.choose(), check.names = FALSE)
head(data)

# Changing column names for convenience. The original column names
# consisted of spaces, slashes and parenthesis which made them unusable.
colnames(data)[3] <- "gas_prod"
colnames(data)[4] <- "gas_cons"
colnames(data)[5] <- "coal_prod"
colnames(data)[6] <- "coal_cons"
colnames(data)[7] <- "oil_prod"
colnames(data)[8] <- "oil_cons"
colnames(data)[9] <- "gas_prod_percap"
colnames(data)[10] <- "gas_cons_percap"
colnames(data)[11] <- "coal_prod_percap"
colnames(data)[12] <- "coal_cons_percap"
colnames(data)[13] <- "oil_prod_percap"
colnames(data)[14] <- "oil_cons_percap"

colnames(data)

# Creating a separate dataset for only rows relating to the United States
US_data <- data[data$Entity == "United States", ]
na.omit(US_data)

# Using eia.gov, creating a dataset using the top 5 producers
China_data <- data[data$Entity == "China", ]
SA_data <- data[data$Entity == "Saudi Arabia", ]
Russia_data <- data[data$Entity == "Russia", ]
Canada_data <- data[data$Entity == "Canada", ]
Iraq_data <- data[data$Entity == "Iraq", ]
Iran_data <- data[data$Entity == "Iran", ]
Qatar_data <- data[data$Entity == "Qatar", ]
Australia_data <- data[data$Entity == "Australia", ]
Indonesia_data <- data[data$Entity == "Indonesia", ]
India_data <- data[data$Entity == "India", ]
Japan_data <- data[data$Entity == "Japan", ]
Germany_data <- data[data$Entity == "Germany", ]

# Using data from worldometer.info, I created separate datasets to look at
# the top 5 countries in which the specific fossil fuel is produced/consumed 
# at the highest rate

gas_top_prod <- rbind(US_data, Russia_data, Iran_data, Qatar_data,
                            Canada_data)
na.omit(gas_top_prod)

coal_top_prod <- rbind(China_data, US_data, India_data, Australia_data,
                            Indonesia_data)
na.omit(coal_top_prod)

oil_top_prod <- rbind(US_data, Russia_data, SA_data, Canada_data,
                           Iraq_data)
na.omit(oil_top_prod)

gas_top_cons <- rbind(US_data, Russia_data, China_data, Iran_data,
                          Japan_data)
na.omit(gas_top_cons)

coal_top_cons <- rbind(China_data, India_data, US_data, Germany_data,
                           Russia_data)
na.omit(coal_top_cons)

oil_top_cons <- rbind(US_data, China_data, India_data, Japan_data,
                            Russia_data)
na.omit(oil_top_cons)

# Investigating data by constructing scatter plots that show
# production/consumption of fossil fuels over the years, and highlighting
# each individual country
ggplot(gas_top_prod, aes(x= Year, y= gas_prod, group= Entity)) + 
  geom_point(aes(color=Entity)) + ylab("Gas Production per cubic meter") +
  ggtitle("Natural Gas Production 1980-2021")

ggplot(coal_top_prod, aes(x= Year, y= coal_prod, group= Entity)) + 
  geom_point(aes(color=Entity)) + ylab("Coal Production per ton") +
  ggtitle("Coal Production 1980-2021")

ggplot(oil_top_prod, aes(x= Year, y= oil_prod, group= Entity)) + 
  geom_point(aes(color=Entity)) + ylab("Oil Production per cubic meter") +
  ggtitle("Oil Production 1980-2021")

ggplot(gas_top_cons, aes(x= Year, y= gas_cons, group= Entity)) + 
  geom_point(aes(color=Entity)) +
  ylab("Natural Gas Consumption per cubic meter") + 
  ggtitle("Natural Gas Consumption 1980-2021")

ggplot(coal_top_cons, aes(x= Year, y= coal_cons, group= Entity)) + 
  geom_point(aes(color=Entity)) +
  ylab("Coal Consumption per ton") + ggtitle("Coal Consumption 1980-2021")

ggplot(oil_top_cons, aes(x= Year, y= oil_cons, group= Entity)) + 
  geom_point(aes(color=Entity)) +
  ylab("Oil Consumption per cubic meter") + 
  ggtitle("Oil Consumption 1980-2021")

# Investigating United States Consumption/Production data
pairs(~ Year + gas_cons + coal_cons + oil_cons, data= US_data, pch= 20,
      labels= c("Year", "Natural Gas", "Coal", "Oil"), 
      main= "Consumption in the US(1980-2021)")

pairs(~ Year + gas_prod + coal_prod + oil_prod, data= US_data, pch= 20,
      labels= c("Year", "Natural Gas", "Coal", "Oil"), 
      main= "Production in the US(1980-2021)")

#pairs(~ Year + gas_prod + gas_prod_percap, data= US_data, pch= 20,
#      labels= c("Year", "Natural Gas", "NG Per Cap"))


########################## PREDICT MODELS ##################################

# GAS #

# Check which polynomial degree is appropriate
US_mods <- vector("list", length= 7)
for(i in 1:7){
  
  US_mods[[i]] <- lm(gas_prod ~ poly(Year, i), data= US_data)
}

anova(US_mods[[1]], US_mods[[2]], US_mods[[3]], US_mods[[4]], US_mods[[5]],
      US_mods[[6]], US_mods[[7]])


# 5th Degree polynomial
fit <- lm(gas_prod ~ poly(Year, 5), data= US_data)
summary(fit)

g_year_grid <- seq(from= 1980, to= 2021, length= 100)
preds <- predict(fit, newdata= list(Year= g_year_grid), se= TRUE)
se.bands <- cbind(preds$fit + 2*preds$se, preds$fit - 2*preds$se)
plot(gas_prod ~ Year, data= US_data, 
     xlim= c(min(g_year_grid), max(g_year_grid)), xlab= "Year",
     ylab= "Natural Gas Production", cex= .75)
title("Natural Gas Production Predictive Model (United States)",
      outer= FALSE)
lines(g_year_grid, preds$fit, lwd= 2)



US_mods <- vector("list", length= 10)
for(i in 1:10){
  
  US_mods[[i]] <- lm(gas_cons ~ poly(Year, i), data= US_data)
}

anova(US_mods[[1]], US_mods[[2]], US_mods[[3]], US_mods[[4]], US_mods[[5]],
      US_mods[[6]], US_mods[[7]], US_mods[[8]], US_mods[[9]], US_mods[[10]])


# 4th Degree polynomial
fit <- lm(gas_cons ~ poly(Year, 4), data= US_data)
summary(fit)

g_year_grid <- seq(from= 1980, to= 2025, length= 100)
preds <- predict(fit, newdata= list(Year= g_year_grid), se= TRUE)
se.bands <- cbind(preds$fit + 2*preds$se, preds$fit - 2*preds$se)
plot(gas_cons ~ Year, data= US_data, 
     xlim= c(min(g_year_grid), max(g_year_grid)), xlab= "Year",
     ylab= "Natural Gas Consumption", cex= .75)
title("Natural Gas Consumption Predictive Model (United States)",
      outer= FALSE)
lines(g_year_grid, preds$fit, lwd= 2)

# COAL #


US_mods <- vector("list", length= 10)
for(i in 1:10){
  
  US_mods[[i]] <- lm(coal_prod ~ poly(Year, i), data= US_data)
}

anova(US_mods[[1]], US_mods[[2]], US_mods[[3]], US_mods[[4]], US_mods[[5]],
      US_mods[[6]], US_mods[[7]], US_mods[[8]], US_mods[[9]], US_mods[[10]])

# 3rd Degree Polynomial was the best fit
fit <- lm(coal_prod ~ poly(Year, 3), data= US_data)
summary(fit)

c_year_grid <- seq(from= 1980, to= 2025, length= 100)
preds <- predict(fit, newdata= list(Year= c_year_grid), se= TRUE)
se.bands <- cbind(preds$fit + 2*preds$se, preds$fit - 2*preds$se)
plot(coal_prod ~ Year, data= US_data, 
     xlim= c(min(c_year_grid), max(c_year_grid)), xlab= "Year",
     ylab= "Coal Production", cex= .75)
title("Coal Production Predictive Model (United States)",
      outer= FALSE)
lines(c_year_grid, preds$fit, lwd= 2)


US_mods <- vector("list", length= 10)
for(i in 1:10){
  
  US_mods[[i]] <- lm(coal_cons ~ poly(Year, i), data= US_data)
}

anova(US_mods[[1]], US_mods[[2]], US_mods[[3]], US_mods[[4]], US_mods[[5]],
      US_mods[[6]], US_mods[[7]], US_mods[[8]], US_mods[[9]], US_mods[[10]])

# 5th Degree Polynomial
fit <- lm(coal_cons ~ poly(Year, 5), data= US_data)
summary(fit)

c_year_grid <- seq(from= 1980, to= 2025, length= 100)
preds <- predict(fit, newdata= list(Year= c_year_grid), se= TRUE)
se.bands <- cbind(preds$fit + 2*preds$se, preds$fit - 2*preds$se)
plot(coal_cons ~ Year, data= US_data, 
     xlim= c(min(c_year_grid), max(c_year_grid)), xlab= "Year",
     ylab= "Coal Consumption", cex= .75)
title("Coal Consumption Predictive Model (United States)",
      outer= FALSE)
lines(c_year_grid, preds$fit, lwd= 2)

# OIL #

US_mods <- vector("list", length= 10)
for(i in 1:10){
  
  US_mods[[i]] <- lm(oil_prod ~ poly(Year, i), data= US_data)
}

anova(US_mods[[1]], US_mods[[2]], US_mods[[3]], US_mods[[4]], US_mods[[5]],
      US_mods[[6]], US_mods[[7]], US_mods[[8]], US_mods[[9]], US_mods[[10]])

# 4th degree polynomial
fit <- lm(oil_prod ~ poly(Year, 4), data= US_data)
summary(fit)

o_year_grid <- seq(from= 1980, to= 2025, length= 100)
preds <- predict(fit, newdata= list(Year= o_year_grid), se= TRUE)
se.bands <- cbind(preds$fit + 2*preds$se, preds$fit - 2*preds$se)
plot(oil_prod ~ Year, data= US_data, 
     xlim= c(min(o_year_grid), max(o_year_grid)), xlab= "Year",
     ylab= "Oil Production", cex= .75)
title("Oil Production Predictive Model (United States)",
      outer= FALSE)
lines(o_year_grid, preds$fit, lwd= 2)


US_mods <- vector("list", length= 10)
for(i in 1:10){
  
  US_mods[[i]] <- lm(oil_cons ~ poly(Year, i), data= US_data)
}

anova(US_mods[[1]], US_mods[[2]], US_mods[[3]], US_mods[[4]], US_mods[[5]],
      US_mods[[6]], US_mods[[7]], US_mods[[8]], US_mods[[9]], US_mods[[10]])

# 5th degree polynomial
fit <- lm(oil_cons ~ poly(Year, 5), data= US_data)
summary(fit)

o_year_grid <- seq(from= 1980, to= 2025, length= 100)
preds <- predict(fit, newdata= list(Year= o_year_grid), se= TRUE)
se.bands <- cbind(preds$fit + 2*preds$se, preds$fit - 2*preds$se)
plot(oil_cons ~ Year, data= US_data, 
     xlim= c(min(o_year_grid), max(o_year_grid)), xlab= "Year",
     ylab= "Oil Consumption", cex= .75)
title("Oil Consumption Predictive Model (United States)",
      outer= FALSE)
lines(o_year_grid, preds$fit, lwd= 2)






