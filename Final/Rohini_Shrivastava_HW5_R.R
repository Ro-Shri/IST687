########################################
#Course: IST687
#Assignment: HW #5
#Name: Rohni Shrivastava
#Date: 8/05/20
#Notes: Rev 1.0
########################################

install.packages("jsonlite")
library(jsonlite)
install.packages("RJSONIO")
library(RJSONIO)
install.packages("sqldf")
library(sqldf)
#STEP 1

filepath <- "C:\\Users\\shriv\\Downloads\\pdvhtf2u_2.JSON"

########################################
#STEP 2

acc_analysis <- as.data.frame(read_json(filepath, simplifyVector= TRUE))

acc_analysis

#is.data.frame(acc)
ac_analysisc_vec <- as.vector(acc_analysis['DAY_OF_WEEK'])

#########################################
#STEP 3

sunday_acc <- sqldf("SELECT COUNT(TRIM(DAY_OF_WEEK)) FROM acc_analysis WHERE DAY_OF_WEEK LIKE '%SUNDAY%'")
sunday_acc #95

omit_na <- na.omit(acc_analysis)

inj_acc <- sqldf("SELECT COUNT(TRIM(INJURY)) FROM omit_na WHERE INJURY LIKE '%YES%'")
inj_acc #272

inj_by_day <- sqldf("SELECT COUNT(TRIM(INJURY)), DAY_OF_WEEK FROM omit_na WHERE INJURY LIKE '%YES%' GROUP BY DAY_OF_WEEK")
inj_by_day
#                  45   FRIDAY   
#                  34   MONDAY   
#                  38   SATURDAY 
#                  22   SUNDAY   
#                  45   THURSDAY 
#                  37   TUESDAY  
#                  51   WEDNESDAY

###########################################
#STEP 4

sunday_tappy <- tapply(acc_analysis$day_of_week, acc_analysis$day_of_week, length)
sunday_tappy

injuries_day <- tapply(omit_na$injury, omit_na$injury, function(x) length(which(x=='YES')))
injuries_day

injuries_day <- tapply(omit_na$injury, omit_na$day_of_week, function(x) length(which(x=='YES')))
injuries_day
