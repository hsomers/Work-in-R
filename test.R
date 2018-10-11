rm(list = ls())
library(readxl)
library(ggplot2)
library(scales)
suppressPackageStartupMessages(library(DBI))
suppressPackageStartupMessages(library(dplyr))
library(reshape2)
library(ggmap)
suppressPackageStartupMessages(library(choroplethr))
library(choroplethrMaps)
library(rvest)
library(RSQLite)


#1
df <- read.csv('data.csv', na.strings = c("Unknown", "NA", "-1"))
df$Sex <- as.factor(df$Sex)
df$Appt_Date <- as.Date(df$Appt_Date)
df$Appt_Status <- as.factor(df$Appt_Status)

#2
df <- df[complete.cases(df), ]

#3
print(levels(df$Sex))
levels(df$Sex) <- c("Female", "Male")

#4
df <- subset(df, Age <= 10)

#5
df$Age <- df$Age*12

#6
st <- group_by(df, Appt_Status, Sex)
st <- summarize(st, num_apts = n())
st <- dcast(st, Appt_Status ~ Sex, value.var = 'num_apts')

#7
p <- qplot(Sex, Age, data = df, geom = "boxplot", fill = Sex)
ggsave(filename = "graphic.png", plot = p, width = 6, height = 4,
       dpi = 600)

#8
row_indices <- which(df$Sex == "Male" & df$Age <= 24 & df$Age >=12 & df$Appt_Status == "NoShow")

#9
lvls <- c("Oct", "Nov", "Dec", "Jan", "Feb")
df$Appt_Month <- format(df$Appt_Date, '%m')
df$Appt_Month[df$Appt_Month == "10"] <- "Oct"
df$Appt_Month[df$Appt_Month == "11"] <- "Nov"
df$Appt_Month[df$Appt_Month == "12"] <- "Dec"
df$Appt_Month[df$Appt_Month == "01"] <- "Jan"
df$Appt_Month[df$Appt_Month == "02"] <- "Feb"

df$Appt_Month <- as.factor(df$Appt_Month)
levels(df$Appt_Month) <- lvls
levels(df$Appt_Month)

#10
df_small <- df[order(df$Age), ]
tenthAge <- df_small[10, 1]
df_small <- subset(df_small, Age <= tenthAge)
