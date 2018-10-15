rm(list = ls())

#Load packages
suppressPackageStartupMessages(library(readxl))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(scales))
suppressPackageStartupMessages(library(DBI))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(reshape2))
suppressPackageStartupMessages(library(ggmap))
suppressPackageStartupMessages(library(choroplethr))
suppressPackageStartupMessages(library(choroplethrMaps))
suppressPackageStartupMessages(library(rvest))
suppressPackageStartupMessages(library(RSQLite))

#Import the csv files
df <- read.csv('googleplaystore.csv', na.strings = c("NaN", "NA"))
revs <- read.csv('googleplaystore_user_reviews.csv', na.strings = c("NaN", "NA"))

#Clean out empty data
df <- df[complete.cases(df), ]
revs <- revs[complete.cases(revs), ]

#What are the average scores for each app category?
AvgSc <- select(df, Category, Rating) %>%
  group_by(Category) %>%
  summarize(AvgScore = mean(Rating)) 

#What are the top 3 apps (in terms of downloads) in each category?
levels(df$Installs)
df$Installs <- gsub('\\+','', df$Installs)
df$Installs <- gsub(',','', df$Installs)
df$Installs <- as.numeric(df$Installs)
top_insts <- select(df, App, Category, Installs) %>%
  group_by(Category) %>%
  top_n(n = 3, wt = Installs) %>%
  arrange(Category, desc(Installs))

qplot(Category, Installs, data = top_insts, geom = "point") +
  theme(axis.text.x = element_text(angle = 90)) + 
  scale_y_continuous(trans = "log10")

#Which app type is the most popular? (most downloads)
#How many games have > 1 genre label
#Which app has the most downloads, and has the highest rating?
#What is the ratio of free:paid apps?


#What are some of the most used words for reviews of 3 or above