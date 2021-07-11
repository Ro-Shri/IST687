########################################
#Course: IST687
#Assignment: HW #3
#Name: Rohini Shrivastava
#Date: 7/21/20
#Notes: Rev 1.0
########################################

#Step 1:
readStates <- function(URLs)
{
  readURL <- read.csv(url(URLs))
  return(readURL)
}
testFrame <- readStates("http://www2.census.gov/programs-surveys/popest/tables/2010-2011/state/totals/nst-est2011-01.csv")

#########################################
#Step 2
#Remove first 9 rows
testFrame <- testFrame[-1:-8,]

#remove last rows
testFrame <- testFrame[-62:-66,]
testFrame <- testFrame[-54:-58,]
testFrame <- testFrame[-52:-53,]

#keep first 5 columns

testFrame <- testFrame[,1:5]

#rownames(testFrame)

#rename the columns
names(testFrame)[1] <- "stateName"
names(testFrame)[2] <- "base2010"
names(testFrame)[3] <- "base2011"
names(testFrame)[4] <- "Jul2010"
names(testFrame)[5] <- "Jul2011"
#testFrame

#remove the '.' in the state names
testFrame$stateName <- gsub("\\.","", testFrame$stateName)

#create a function to make a value a number
numberize <- function(inputColumn)
{
  inputColumn<- gsub(",","",inputColumn)
  return(as.numeric(inputColumn))
}

#make the last four columns numbers
testFrame$base2010 <- numberize(testFrame$base2010)
testFrame$base2011 <- numberize(testFrame$base2011)
testFrame$Jul2010 <- numberize(testFrame$Jul2010)
testFrame$Jul2011 <- numberize(testFrame$Jul2011)
#testFrame

##########################################
#Step 3

dfStates <- testFrame
df2011mean<-mean(dfStates$Jul2011)
#dfStates

#########################################
#Step 4

max(dfStates$Jul2011)
columnnum <- rownames(dfStates[dfStates$Jul2011 == max(dfStates$Jul2011),])
numericnum <- as.numeric(columnnum)
dfStates$stateName[(numericnum-8)]
#colnames(testFrame)

#Max population is 37691912 for California

increasingpop <- dfStates[order(dfStates$Jul2011),]
increasingpop

##########################################
#Step 5

percentofstates <- function(vectorname, inputnum)
{
  numbelow <- 0
  for (val in vectorname){
    if (val <= inputnum) {numbelow = numbelow+1}
    #print(val)
  }
  #print(numbelow)
  percentage <- 100*(numbelow/51)
  return (percentage)
}
Jul2011Vector <- as.vector(dfStates[['Jul2011']])
#is.vector(Jul2011Vector)
percentofstates(Jul2011Vector, df2011mean)
#retunrs 66.6667% (34 states are below the mean)