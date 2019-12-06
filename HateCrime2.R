install.packages("Rtools")
install.packages("gganimate")
install.packages("ggforce")
install.packages("gifski")
install.packages("png")
install.packages("lubridate")
install.packages("zoo")
install.packages("git")

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

#downloading zipfile containing data, extracting, and downloading
#data from https://crime-data-explorer.fr.cloud.gov/downloads-and-docs
temp <- tempfile()
download.file("http://s3-us-gov-west-1.amazonaws.com/cg-d4b776d0-d898-4153-90c8-8336f86bdfec/hate_crime_2017.zip", temp)
unzip("http://s3-us-gov-west-1.amazonaws.com/cg-d4b776d0-d898-4153-90c8-8336f86bdfec/hate_crime_2017.zip")
data <- read.csv(unz(temp, "hate_crime_2017.csv"))


#Mutating and Cleaning the INCIDENT_DATE variable
data$INCIDENT_DATE <- as.Date(data$INCIDENT_DATE, "%d-%b-%y")
data <- data %>%
  dplyr::mutate(year = lubridate::year(INCIDENT_DATE), 
                month = lubridate::month(INCIDENT_DATE), 
                day = lubridate::day(INCIDENT_DATE))

data <- mutate(data, Month_Year = paste(data$month, "-", data$year))


data$Month_Year <- str_replace(data$Month_Year, " ", "")
data$Month_Year <- str_replace(data$Month_Year, " ", "")
data$Month_Year <- as.yearmon(data$Month_Year, "%m-%Y")




#creating new variables
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

hate <- full_join(anti_white, anti_asian)

hate <- full_join(hate, anti_black)


hate_full <- full_join(anti_white, anti_asian)

hate_full <- full_join(hate, anti_black)

hate_full <- full_join(hate, anti_islamic)

hate_full <- full_join(hate, anti_jewish)

hate_full <- full_join(hate, anti_latinx)

hate_full <- full_join(hate, anti_arab)

#more cleaining of date
hate$Var1 <- as.Date(as.yearmon(hate$Var1, "%b %Y"))

hate_full$Var1 <- as.Date(as.yearmon(hate_full$Var1, "%b %y"))

staticLinePlot <- ggplot(data = hate,
       aes(x = Var1, y = Freq, group = race_ethnicity, color = factor(race_ethnicity))) +
  geom_line() +
  labs(x = "Date", y = "Frequency of Hate Crimes") +
  scale_color_manual(values = c("green3", "darkmagenta", "dodgerblue")) +
  theme(axis.text.y = element_blank(), axis.ticks = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), legend.title = element_blank())

animateLinePlot <- staticLinePlot +
  geom_point() +
  transition_reveal(Var1)

animate(animateLinePlot, renderer = gifski_renderer("HateCrimeLine.gif"))

data$STATE_ABBR <- as.factor(data$STATE_ABBR)


hate_new <- hate_full %>% 
  dplyr::mutate(month = lubridate::month(Var1), 
                year = lubridate::year(Var1))

agg_sum <- aggregate(hate_new[,2], by = list(hate_new$race_ethnicity, hate_new$year), sum)
hate_final <- as.data.frame(agg_sum)
colnames(hate_final) <- c("Race_Ethnicity", "Year", "Total")

hate_adjust <- filter(hate_final, Race_Ethnicity == "Black" | Race_Ethnicity == "White" | Race_Ethnicity == "Asian")

hate_adjust$std_total <- hate_adjust$Total

hate_adjust$std_total[hate_adjust$Year <= 1994 & hate_adjust$Race_Ethnicity == "Black"] <- hate_adjust$std_total/29.986060

hate_adjust$std_total[hate_adjust$Year <= 1994 & hate_adjust$Race_Ethnicity == "White"] <- hate_adjust$std_total/199.686070

hate_adjust$std_total[hate_adjust$Year <= 1994 & hate_adjust$Race_Ethnicity == "Asian"] <- hate_adjust$std_total/7.273662

hate_adjust$std_total[hate_adjust$Year > 1994 & hate_adjust$Year <= 2000 & hate_adjust$Race_Ethnicity == "Black"] <- hate_adjust$std_total/34.658190

hate_adjust$std_total[hate_adjust$Year > 1994 & hate_adjust$Year <= 2000 & hate_adjust$Race_Ethnicity == "White"] <- hate_adjust$std_total/211.460626	

hate_adjust$std_total[hate_adjust$Year > 1994 & hate_adjust$Year <= 2000 & hate_adjust$Race_Ethnicity == "Asian"] <- hate_adjust$std_total/10.641833

hate_adjust$std_total[hate_adjust$Year > 2000 & hate_adjust$Year <= 2004 & hate_adjust$Race_Ethnicity == "Black"] <- hate_adjust$std_total/34.658190

hate_adjust$std_total[hate_adjust$Year > 2000 & hate_adjust$Year <= 2004 & hate_adjust$Race_Ethnicity == "White"] <- hate_adjust$std_total/211.460626

hate_adjust$std_total[hate_adjust$Year > 2000 & hate_adjust$Year <= 2004 & hate_adjust$Race_Ethnicity == "Asian"] <- hate_adjust$std_total/10.641833

hate_adjust$std_total[hate_adjust$Year > 2004 & hate_adjust$Year <= 2010 & hate_adjust$Race_Ethnicity == "Black"] <- hate_adjust$std_total/38.929319

hate_adjust$std_total[hate_adjust$Year > 2004 & hate_adjust$Year <= 2010 & hate_adjust$Race_Ethnicity == "White"] <- hate_adjust$std_total/223.553265

hate_adjust$std_total[hate_adjust$Year > 2004 & hate_adjust$Year <= 2010 & hate_adjust$Race_Ethnicity == "Asian"] <- hate_adjust$std_total/15.214265

# hate_adjust$log_std_total <- log(hate_adjust$std_total)

hate_adjust$std_total <- format(hate_adjust$std_total, scientific = FALSE)


staticBar <- ggplot(hate_adjust, aes(Race_Ethnicity, std_total, 
                                     fill = as.factor(Race_Ethnicity))) +
  geom_col(show.legend = FALSE) + 
  scale_fill_manual(values = c("darkmagenta", "dodgerblue", "green3")) +
  theme(axis.text.y = element_blank(), axis.ticks = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) + 
  xlab("Race/Ethnicity") +
  ylab("Hate Crimes by Population Size")
  

animateBar <- staticBar + 
  transition_states(Year, transition_length = 1, state_length = 1) +
  labs(title = 'Hate Crimes Per Year : {closest_state}')


animate(animateBar, renderer = gifski_renderer("HateCrime.gif"))
