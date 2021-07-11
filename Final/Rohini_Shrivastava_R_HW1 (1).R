height <- c(59,60,61,58,67,72,70)
weight <- c(150,140,180,220,160,140,130)

MeanHeight = mean(height)
MeanWeight = mean(weight)
MeanWeight
#returns 160
MeanHeight
#returns 63.85714
LengthHeight = length(height)
LengthHeight
#returns 7
LengthWeight = length(weight)
LengthWeight
#returns 7
SumHeight = sum(height)
SumHeight
#returns 447
SumWeight = sum(weight)
SumWeight
#returns 1120
AvgWeight = SumWeight/LengthWeight
AvgWeight
#returns 160
AvgHeight = SumHeight/LengthHeight
AvgHeight
#returns 63.85714

#both mean and average for weight and height are the same
maxH = max(height)
maxH
#returns 72
minW = min(weight)
minW
#returns 130

Plus5Weight = weight + 5
Plus5Weight
#returns 155 145 185 225 165 145 135
HeightByWeight = Plus5Weight/height
HeightByWeight
#returns 2.627119 2.416667 3.032787 3.879310 2.462687 2.013889 1.928571

if (maxH > 60) "yes" else "no"
#returns "yes"

if (minW > a) "yes" else "no"
#returns "no"