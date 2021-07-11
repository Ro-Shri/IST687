myCars <- mtcars

max(myCars$hp)
#returns 335
rownames(myCars[myCars$hp == max(myCars$hp),])
#returns Masati Bora

max(myCars$mpg)
#returns 33.9
rownames(myCars[myCars$mpg == max(myCars$mpg),])
#returns Toyota Corolla
lowTohighMPG <- myCars[order(myCars$mpg),]
lowTohighMPG

#to find the car with the "best" combination of mpg and hp
#we are calculating the difference of the two
# ex. hp-mpg
#this number is stored into a new column bestcarhpmpg
#we are returning the value of the car with the greatest number in this new column
myCars$bestcarhpmpg <- myCars$hp - myCars$mpg
rownames(myCars[myCars$bestcarhpmpg == max(myCars$bestcarhpmpg),])
#returns a Maserati Bora

myCars$scalempg <- scale(myCars$mpg)
myCars
myCars$scalehp <- scale(myCars$hp)
myCars
#we have scaled both mpg and hp to find the best car now that we have placed equal weight on the two columns
#we will be taking the absolute value of the difference between the two 
#the maximum number will be the best car

myCars$scaledDiff <- abs(myCars$scalehp - myCars$scalempg)
myCars
rownames(myCars[myCars$scaledDiff == max(myCars$scaledDiff),])
#returns the Maserati Bora
