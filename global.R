library(dplyr)
library(ggplot2)
library(survival)
library(lubridate)
library(tidyr)
library(broom)
library(survminer)
library(plotly)

#Read in the data
BMT <- read.csv("BMT_29Sep.csv",na.strings=c("", "NA"))

#Select variables of interest
BMT1 <- select(BMT, TypeOfMM, Transplant.Date, C.Status,Death.date, Last.Follow.up,M.protein..,RenalFailure,Plasmacytoma,RelapseDate,ISS,TotalNoTherapy,RemissionStatusPreTx,RemissionStatusPostTx)
BMT1$Transplant.Date <- dmy(BMT1$Transplant.Date)
BMT1$Last.Follow.up <- dmy(BMT1$Last.Follow.up)
BMT1$Death.date <- dmy(BMT1$Death.date)
BMT1$RelapseDate <- dmy(BMT1$RelapseDate)

#Create censoring indicator variable for death
cenin_dead <- rep(1, nrow(BMT1))
for (i in 1:nrow(BMT1)) ifelse(BMT1$C.Status[i]!='Dead',cenin_dead[i] <- 0,cenin_dead[i] <- 1)

#Replace 'NA' death dates with LFU dates
index <- which(is.na(BMT1$Death.date))
BMT1$Death.date[index] <- BMT1$Last.Follow.up[index]

#Find time difference
BMT1 <- mutate(BMT1, TD=as.numeric(Death.date-Transplant.Date))
surv_data <- cbind(BMT1,cenin_dead)

#Analysis on relapse date
#Create censoring indicator variable for relapse date
relapse <- rep(1, nrow(surv_data))
for (i in 1:nrow(surv_data)) ifelse(is.na(surv_data$RelapseDate[i]),relapse[i] <- 0,relapse[i] <- 1)

#Replace 'NA' death dates with LFU dates
index <- which(is.na(surv_data$RelapseDate))
surv_data$RelapseDate[index] <- surv_data$Last.Follow.up[index]

#time difference for transplant - relapse
surv_data <- mutate(surv_data, TD_R=as.numeric(RelapseDate-Transplant.Date))
rlp_data <- cbind(surv_data,relapse)
rlp_data$ISS[rlp_data$ISS=="II"] <- "I"
rlp_data$ISS <- factor((rlp_data$ISS))

########################################################
#Clinical Trials

dat <- read.csv("DBP.csv")
library(xtable)
print(xtable(dat, digits=0, align=rep("c", 1+ncol(dat)),
             caption="Diastolic Blood Pressure Trial Data.",label = "ANOVA.data.DBP"),
      table.placement = "htbp",caption.placement = "top",include.rownames=FALSE)
