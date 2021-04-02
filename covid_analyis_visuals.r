# Load libraries:
library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(plotly)
library(scales)
library(RSelenium)
library(streamgraph)


setwd("~/Documents")

# Download the updated COVID-19 data:
driver <- rsDriver(browser = c("firefox"))
remDr <- driver[["client"]]
remDr$navigate("https://ourworldindata.org/coronavirus/country/spain")

cOVID_buTTON <- remDr$findElement(
  using = "xpath",
  "//a[@style='display: inline-block; margin-left: 0.5rem;']")

# Set to download every day at 0900hrs
cOVID_buTTON$clickElement()
# This downloads whole world data and stores it in a local folder.


# close
remDr$close()
driver[["server"]]$stop()
rm(driver, remDr)
gc()


# Since the data files have the same name R selects the latest downloaded data by:
# Where my data is downloaded: setwd("~/RProjects/COVID_ANALYSIS/DAta")
# View all files in the working directory:
data_files <- file.info(Sys.glob("*.csv"))

daILY_rEGIONS_rEPORTS <- read.csv(
  row.names(file.info(Sys.glob("*.csv")))[which.max(file.info(Sys.glob("*.csv"))[["ctime"]])],
  header = TRUE, na.strings = ""
)

# Summary
summary(daILY_rEGIONS_rEPORTS)
# Some columns have missing values, and this is due to values not recoreded earlier.
# For analysis fill the values with 0:
daILY_rEGIONS_rEPORTS <- daILY_rEGIONS_rEPORTS %>%
  mutate_all(funs(replace_na(., 0)))

# Set data as date:
daILY_rEGIONS_rEPORTS$date <- as.Date(daILY_rEGIONS_rEPORTS$date)

# Africa Visuals:
aFRICa_cOVID_dATA <- daILY_rEGIONS_rEPORTS %>% 
  filter(location == "Africa")

# Africa daily trend to date Line Graph
ggplot() +
  geom_line(
    data = aFRICa_cOVID_dATA,
    aes(y = total_cases, x = date, colour = "Confirmed.Cases"), group =  1
  ) +
  geom_line(
    data = aFRICa_cOVID_dATA,
    aes(y = new_cases, x = date, colour = "New.Cases"),
    linetype = "twodash", group =  1
  ) +
  geom_line(
    data = aFRICa_cOVID_dATA,
    aes(y = total_deaths, x = date, colour = "Death.Cases"),
    linetype = "dotdash", group = 1
  ) +
  geom_line(
    data = aFRICa_cOVID_dATA,
    aes(y = people_fully_vaccinated, x = date, colour = "Vaccinated"), group =  1
  ) +
  scale_color_manual(values = c(
    "Confirmed.Cases" = "darkred",
    "New.Cases" = "orange",
    "Death.Cases" = "steelblue",
    "Vaccinated" = "darkgreen"
  )) +
  labs(color = "COVID-19 CASES") +
  labs(
    title = "COVID-19 Cases February 2018 - March 2021",
    subtitle = "New, Recovered & Death Cases",
    caption = " Source: WHO"
  ) +
  xlab("Date") +
  ylab("CovidCase Count") +
  theme_minimal()

# For an interactive plot:  
ggplotly(p)

# Africa Daily_new Cases
covid_daily <- gather(
  aFRICa_cOVID_dATA[, c("date", "new_cases")],
  variable, value, -date)

# Plot  
ggplot(data = covid_daily, aes(x = date, y = value, fill = variable)) +
  geom_col(position = position_dodge()) +
  labs(
    title = "COVID-19 Daily Registered Cases February 2018 - March 2021",
    subtitle = "New Cases",
    caption = " Source: WHO"
  ) +
  xlab("Date") +
  ylab("Daily Count") +
  scale_x_date(breaks = date_breaks("2 month"), labels = date_format("%b %y")) +
  theme_minimal()

# Compare Regional(continental) cases:
Visualisation
# Compare Regions
# 1. Sum by Regions:
# 2. Graph the sums
reGION_cOUNT <- daILY_rEGIONS_rEPORTS%>%
  group_by(location) %>%
  summarise(Count = sum(new_cases)) %>% 
  filter(location %in% 
           c("Africa","Asia","Europe","North America","South America", "Oceania")
         )

ggplot(
  data = reGION_cOUNT, aes(x = reorder(location, -Count), y = Count)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  geom_col(alpha = 1 / 10) +
  ggplot2::labs(
    title = "Worldwide Cumulative COVID cases",
    x = "Regions",
    y = "Cumulated Cases"
  ) +
  expand_limits(x = c(0, NA), y = c(0, NA)) +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
  hrbrthemes::theme_ft_rc()

# South Africa Plots
# ______________________________________________________________________________
SaFRICa_cOVID_dATA <- daILY_rEGIONS_rEPORTS %>% 
  filter(location == "South Africa")

ggplot(data = SaFRICa_cOVID_dATA, aes(
  x = as.Date(date),
  y = total_cases
)) +
  geom_line(aes(
    x = as.Date(date),
    y = new_deaths_smoothed
  )) +
  labs(
    title = "SAfrica COVID-19 Daily Cumulatives",
    subtitle = "Rolling average between 2020-02-01 and 2021-028-26",
    y = "Count",
    x = "Month"
  ) +
  hrbrthemes::theme_modern_rc()


#  Moving averages
ggplot(data = SaFRICa_cOVID_dATA, aes(x = as.Date(date), y = new_cases)) +
  geom_col(position = position_dodge(),  colour = "white") +
  geom_line(aes(x = as.Date(date), y = new_cases_smoothed), colour = "red") +
  labs(
    title = "COVID-19 Daily Registered Cases February 2018 - March 2021",
    subtitle = "New Cases",
    caption = " Source: WHO"
  ) +
  xlab("Date") +
  ylab("Daily Count") +
  scale_x_date(breaks = date_breaks("2 month"), labels = date_format("%b %y")) +
  hrbrthemes::theme_modern_rc()

# Installation
devtools::install_github("hrbrmstr/streamgraph")
daILY_rEGIONS_rEPORTS %>%
  select(date, new_cases,new_tests) %>%
  tidyr::gather(groups, value, -date) %>%
  group_by(month(ymd(date)), groups) %>%
  tally(wt = value) %>%
  streamgraph("groups", "n", "date") %>%
  sg_axis_x(20) %>%
  sg_fill_brewer("PuOr") %>%
  sg_legend(show = TRUE, label = "Groups: ")


# Daily vaccine Doses given
# Seven Day average
ggplotly(ggplot(data = SaFRICa_cOVID_dATA[SaFRICa_cOVID_dATA$date > ymd(20210215), ],
                aes(x = as.Date(date), y = new_vaccinations)) +
  geom_col(position = position_dodge()) +
  geom_line(aes(x = as.Date(date), y = new_vaccinations_smoothed), colour = "red") +
  labs(
    title = "Vaccinations",
    caption = " Source: WHO"
  ) +
  xlab("Date") +
  ylab("Daily Count") +
  scale_x_date(breaks = date_breaks("7 days"), labels = date_format("%b %d")) +
  theme_minimal())

# Deaths
ggplotly(ggplot(data = SaFRICa_cOVID_dATA, aes(x = as.Date(date), y = new_deaths)) +
  geom_col(position = position_dodge()) +
  geom_line(aes(x = as.Date(date), y = new_deaths_smoothed), colour = "red") +
  labs(
    title = "Deaths",
    caption = " Source: WHO"
  ) +
  xlab("Date") +
  ylab("Daily Count") +
  scale_x_date(breaks = date_breaks("3 months"), labels = date_format("%b %y")) +
  theme_minimal())



# Daily Tests Conducted.
ggplotly(ggplot(data = SaFRICa_cOVID_dATA, aes(x = as.Date(date), y = new_tests)) +
  geom_col(position = position_dodge()) +
  geom_line(aes(x = as.Date(date), y = new_tests_smoothed), colour = "red") +
  labs(
    title = "Tests Conducted",
    caption = " Source: WHO"
  ) +
  xlab("Date") +
  ylab("Daily Count") +
  scale_x_date(breaks = date_breaks("3 months"), labels = date_format("%b %y")) +
  theme_minimal())

# BAseline
# Compared to base line graphs
curr <- daILY_rEGIONS_rEPORTS$new_cases[-(1:3)]
prev <- daILY_rEGIONS_rEPORTS$new_cases[1:(length(daILY_rEGIONS_rEPORTS$new_cases)-3)]
daily_change <- 100 * round( (curr-prev) / prev, 2 )
monChange <- 100 * round( (curr-prev) / prev, 2 )

daily_change[is.na(daily_change)] <- 0


barCols <- sapply(daily_change,
                  function(x) {
                    if ( x < 200) {
                      return("steelblue")
                    } else {
                      return("red")
                    }
                  })
barplot(daily_change, border=NA, space=, las=0, col=barCols,
        main="COVID-19 DAILY CHANGE",
        ylim=c(-200,3500),
        ylab = "DAILY CHANGE")

# End