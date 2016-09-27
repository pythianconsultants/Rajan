library(dplyr)
library(ggplot2)
library(lubridate)
library(survival)
library(plotly)

#Read in the data
BMT <- read.csv("BMT.csv")

#Convert to date format
BMT$Transplant.Date <- dmy(BMT$Transplant.Date)
BMT$Eng.Date.1..........500.ANC <- dmy(BMT$Eng.Date.1..........500.ANC)
BMT$LFU.Date <- dmy(BMT$LFU.Date)
BMT$Last.Follow.up <- dmy(BMT$Last.Follow.up)
BMT$Death.date <- dmy(BMT$Death.date)

#Exploratory data anaylsis
#Type of MM with Patient's sex

ggplot(BMT,aes(TypeOfMM, fill=Patient.s.Sex))+geom_bar()

#Renal Failure
ggplot(BMT,aes(x=RenalFailure, fill=Patient.s.Sex))+geom_bar()

#Plasmacytoma
ggplot(BMT,aes(x=Plasmacytoma, fill=Patient.s.Sex))+geom_bar()

#Scatter between age and protein
p <- ggplot(BMT,aes(x=Pts.age,y=M.protein..))+geom_point()
p+geom_smooth()
p+geom_hline(yintercept = mean(BMT$M.protein.., na.rm=T))+geom_smooth(method = "loess")+geom_vline(xintercept = mean(BMT$Pts.age,na.rm = T))

#Scatter between MNC dose and age
p <- ggplot(BMT,aes(x=Pts.age,y=MNC.dose))+geom_point()
p+geom_smooth()
p+geom_hline(yintercept = mean(BMT$MNC.dose, na.rm=T))+geom_smooth(method = "loess")+geom_vline(xintercept = mean(BMT$Pts.age,na.rm = T))

#Scatter between MNC dose and protein
p <- ggplot(BMT,aes(x=M.protein..,y=MNC.dose))+geom_point()
p+geom_smooth()
p+geom_hline(yintercept = mean(BMT$MNC.dose, na.rm=T))+geom_smooth(method = "loess")+geom_vline(xintercept = mean(BMT$M.protein..,na.rm = T))
