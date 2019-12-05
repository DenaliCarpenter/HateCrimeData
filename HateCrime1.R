install.packages("gganimate")
install.packages("ggforce")
install.packages("gifski")
install.packages("png")
install.packages("lubridate")
install.packages("zoo")

library(gganimate)
library(ggforce)
library(readr)
library(ggplot2)
library(gifski)
library(png)
library(tidyr)
library(dplyr)
library(caret)
library(tidyverse)
library(lubridate)
library(zoo)

data <- read.csv("Hate Crime Data.csv")
data$INCIDENT_DATE <- as.Date(data$INCIDENT_DATE, "%d-%b-%y")
data <- data %>%
  dplyr::mutate(year = lubridate::year(INCIDENT_DATE), 
                month = lubridate::month(INCIDENT_DATE), 
                day = lubridate::day(INCIDENT_DATE))

data <- mutate(data, Month_Year = paste(data$month, "-", data$year))
data$Month_Year <- str_replace(data$Month_Year, " ", "")
data$Month_Year <- str_replace(data$Month_Year, " ", "")
data$Month_Year <- as.yearmon(data$Month_Year, "%m-%Y")
View(data)


anti_black <- as.data.frame(table(data$Month_Year[grep("Anti-Black or African American", data$BIAS_DESC)]))
anti_black <- mutate(anti_black, race_ethnicity = "Black")

anti_latinx <- as.data.frame(table(data$Month_Year[grep("Anti-Hispanic or Latino", data$BIAS_DESC)]))
anti_latinx <- mutate(anti_latinx, race_ethnicity = "Latinx")

anti_white <- as.data.frame(table(data$Month_Year[grep("Anti-White", data$BIAS_DESC)]))
anti_white <- mutate(anti_white, race_ethnicity = "White")

anti_islamic <- as.data.frame(table(data$Month_Year[grep("Anti-Islamic", data$BIAS_DESC)]))
anti_islamic <- mutate(anti_islamic, race_ethnicity = "Muslim")

anti_jewish <- as.data.frame(table(data$Month_Year[grep("Anti-Jewish", data$BIAS_DESC)]))
anti_jewish <- mutate(anti_jewish, race_ethnicity = "Jewish")

anti_arab <- as.data.frame(table(data$Month_Year[grep("Anti-Arab", data$BIAS_DESC)]))
anti_arab <- mutate(anti_arab, race_ethnicity = "Arab")

anti_asian <- as.data.frame(table(data$Month_Year[grep("Anti-Asian", data$BIAS_DESC)]))
anti_asian <- mutate(anti_asian, race_ethnicity = "Asian")

hate <- full_join(anti_arab, anti_asian)

hate <- full_join(hate, anti_black)

hate <- full_join(hate, anti_islamic)

hate <- full_join(hate, anti_jewish)

hate <- full_join(hate, anti_latinx)

hate <- full_join(hate, anti_white)

View(hate)


hate$Var1 <- as.Date(as.yearmon(hate$Var1, "%b %Y"))

p <- ggplot(data = hate,
            aes(x = Var1, y = Freq, group = race_ethnicity, color = factor(race_ethnicity))) +
  geom_line() +
  geom_vline(xintercept = as.Date("2015-6-01")) +
  labs(x = "Date", y = "Frequency of Hate Crimes") +
  theme(legend.position = "top", legend.title = element_blank())





