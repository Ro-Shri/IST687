########################################
#Course: IST687
#Assignment: HW #8
#Name: Rohni Shrivastava
#Date: 08/27/20
#Notes: Rev 1.0
########################################
#STEP 1

df <-read.csv("C:/Users/shriv/Downloads/HW8mlr01.csv")

colnames(df)<-c("Fawn","Population","Precipitation", "Winter")

str(df)
#######################################
#Step 3
plot(df$Population, df$Fawn)
plot(df$Precipitation, df$Fawn)
plot(df$Winter, df$Fawn)

########################################
#STEP 4
Pop <- lm(Fawn ~ Population, data=df)
summary(Pop)
#This model has an R-quared value of 0.86- so it is a strong relationship. The P value is 0.0005
#so population is a statistically significant variable.

PopPrec <- lm(Fawn ~ Population + Precipitation, data=df)
summary(PopPrec)
#This model has a R-squared value of 0.87. THis also had a p-value of 0.0022 so it is statistically
#significant.

PopWin <- lm(Fawn ~ Population + Precipitation + Winter, data=df)
summary(PopWin)
#This model has an R-squared value of 0.955 which is the higest value. This is the model that is the 
#model that works best and is the most parsimonious. The p value here is also found to be 0.0012.
