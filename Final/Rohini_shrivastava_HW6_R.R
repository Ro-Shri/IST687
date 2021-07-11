########################################
#Course: IST687
#Assignment: HW #6
#Name: Rohni Shrivastava
#Date: 8/13/20
#Notes: Rev 1.0
########################################

install.packages("ggplot2")
library(ggplot2)

install.packages("reshape2")
library(reshape2)

#STEP 1
airdata <- airquality
airdata

#########################################
#STEP 2

airdta = na.omit(airdata)
airdta

########################################
#STEP 3

#Histograms
#OZONE
ozone_hist <- ggplot(airdta, aes(x=ozone)) + geom_histogram(bins = 25)
ozone_hist

#wind
wind_hist <- ggplot(airdta, aes(x=wind)) + geom_histogram(bins = 25)
wind_hist

#temp
temp_hist <- ggplot(airdta, aes(x=temp)) + geom_histogram(bins = 25)
temp_hist

#solar
solar_hist <- ggplot(airdta, aes(x=Solar.R)) + geom_histogram(bins = 25)
solar_hist

#boxplot
ozone_box <- ggplot(airdta, aes(x=factor(0), ozone)) + geom_boxplot()
ozone_box


wind_box <- ggplot(airdta, aes(x=factor(0), wind)) + geom_boxplot()
wind_box

#########################################
#STEP 4

airdta$dates <- paste0(airdta$Month, '/', airdta$Day, '/', '1973')
airdta
airdta$dates <-as.Date(airdta$dates, "%m/%d/%y")
#str(airdta)

ozone_line <- ggplot(airdta, aes(x=dates, y=ozone, group=1))+geom_line()
ozone_line

solar_line <- ggplot(airdta, aes(x=dates, y=Solar.R, group=1))+geom_line()
solar_line

temp_line <- ggplot(airdta, aes(x=dates, y=temp, group=1))+geom_line()
temp_line

wind_line <- ggplot(airdta, aes(x=dates, y=wind, group=1))+geom_line()
wind_line

x <- melt(data=airdta, id.vars=c('dates','Month','Day'), measured.vars=c('ozone','Solar.R','temp','wind'))
x

all_four <-ggplot(x, aes(x=dates, group=variable, color=variable)) +geom_line(aes(y=value))
all_four

########################################
#STEP 5
heat_tile <-ggplot(x, aes(x=dates, y=variable))+ geom_tile(aes(fill=value))
heat_tile

########################################
#STEP 6
scatter_plots <- ggplot(airdta, aes(x=wind, y=temp))+geom_point(aes(size=ozone, color=Solar.R))
scatter_plots

########################################
#STEP 7
#a. There doesn't seem to be a correlation. From looking at the line plots we can see that the data 
#   just randomly goes up and down without following a pattern. The heat tiles all show similar colors per section.

#b. The most useful tool for me was split between the histogram and the line plot. The histogram because it was nice
#   seeing the most common value bucket. It helped show if the data was skewed in any direction. The line plot because
#   it showed the pattern of the data for each individual point without too much complication of understanding what
#   it is showing. 