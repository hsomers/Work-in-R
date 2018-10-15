rm(list = ls())

#Load packages
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(dplyr))

#-------------------------------------------------------------------
#Analysis Below
#-------------------------------------------------------------------

#Import the csv file
df <- read.csv('googleplaystore.csv', na.strings = c("NaN", "NA"))

#Clean out empty data
df <- df[complete.cases(df), ]

#What are the average scores for each app category?
AvgSc <- select(df, Category, Rating) %>%
  group_by(Category) %>%
  summarize(AvgScore = mean(Rating)) 

#What are the top 3 apps (in terms of downloads) for each category?
levels(df$Installs)
df$Installs <- gsub('\\+','', df$Installs)
df$Installs <- gsub(',','', df$Installs)
df$Installs <- as.numeric(df$Installs)
top_insts <- select(df, App, Category, Installs) %>%
  group_by(Category) %>%
  top_n(n = 3, wt = Installs) %>%
  arrange(Category, desc(Installs))

#Make a plot of the top apps installed for each category
qplot(Category, Installs, data = top_insts, geom = "point") +
  theme(axis.text.x = element_text(angle = 90)) + 
  scale_y_continuous(trans = "log10")

#Which app type is the most popular? (most total installs)
print(select(df, Category, Installs) %>%
  group_by(Category) %>%
  summarise(TotalInst = sum(Installs)) %>%
  top_n(n=1, wt = TotalInst))

#Which app has the most downloads, and has the highest rating?
temp <- select(df, App, Installs, Rating) %>%
        distinct(App, Installs, Rating) %>%
        top_n(n=1, wt = Installs) %>%
        top_n(n=1, wt = Rating)