########################################
#Course: IST687
#Assignment: HW #4
#Name: Rohni Shrivastava
#Date: 7/27/20
#Notes: Rev 1.0
########################################
#STEP 1

install.packages("moments")
library(moments)

printVecInfo <- function(inputvector)
{vector_mean <- mean(inputvector)
 vector_median <- median(inputvector)
 vector_min <- min(inputvector)
 vector_max <- max(inputvector)
 vector_standard <- sd(inputvector)
 vector_quantile <- quantile(inputvector, prob = c(0.05, 0.95))
 vector_skew <- skewness(inputvector)
 cat("mean: ", vector_mean, "\n")
 cat("median: ", vector_median,"\n")
 cat("min: ", vector_min,"\n")
 cat("max: ", vector_max,"\n")
 cat("standard deviation: ", vector_standard,"\n")
 cat("quantile (0.05, 0.95): ", vector_quantile,"\n")
 cat("skewness: ", vector_skew,"\n")
}

test_vec <- c(1,2,3,4,5,6,7,8,9,10,50)
printVecInfo(test_vec)

########################################
#STEP 2

redM <- rep("red", 50)
blueM <- rep("blue", 50)

jar <- c(redM,blueM)

countred <- length(grep("red", jar))
countred
#returns 50

jarsamp <- sample(jar, size =10, replace=TRUE)
jarsamp
#returns "blue" "blue" "red"  "blue" "blue" "red"  "blue" "blue" "blue"
#"blue"
red_samp1_mean <- (length(grep("red", jarsamp)))/length(jarsamp)
red_samp1_mean
#returns 0.2

sample10 <-function(samplename){
  output <- sample(samplename, 10, replace = TRUE)
  return(output)
}

numberofred <- function(samplename){
  meanreds <- (length(grep("red", samplename)))/10
  return(meanreds)
}

replicate_js_20 <- replicate(20, (length(grep("red",(sample(jar, 10, replace = TRUE))))/10))
replicate_js_20
# [1] 0.5 0.5 0.7 0.2 0.7 0.5 0.4 0.6 0.1 0.6 0.7 0.5 0.3 0.3 0.4 0.6 0.7 0.6 0.5 0.5

printVecInfo(replicate_js_20)
hist(replicate_js_20)

replicate_js_100 <- replicate(20, (length(grep("red",(sample(jar, 100, replace = TRUE))))/10))
replicate_js_100

printVecInfo(replicate_js_100)
hist(replicate_js_100)

replicate_jar100 <- replicate(100, (length(grep("red",(sample(jar, 100, replace = TRUE))))/10))
replicate_jar100

printVecInfo(replicate_jar100)
hist(replicate_jar100)

###############################################################################
#Step 3

air_var <- airquality
air_var

clean_airquality <- na.omit(air_var)
clean_airquality

ozone <- as.vector(clean_airquality$Ozone)
printVecInfo(ozone)
hist(ozone)

wind <- as.vector(clean_airquality$Wind)
printVecInfo(wind)
hist(wind)

temp <- as.vector(clean_airquality$Temp)
printVecInfo(temp)
hist(temp)