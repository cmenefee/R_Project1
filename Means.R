SmallTreeData <- read.csv("/home/aggitan/Projects/Data_Science/Project_1/SampleTreeDataExport.csv")
FullTreeData <- read.csv("/home/aggitan/Projects/Data_Science/Project_1/covtype.csv")
#means

mean(SmallTreeData$Elevation)

########################################
#Average elevation for each area       #
########################################
#mean(SmallTreeData$Elevation[SmallTreeData$Wilderness_Area4==1])
for (e in seq(1:4))
  {
	  assign("x", paste("SmallTreeData$Wilderness_Area", e, sep=""))
    z <- eval(parse(text=x))
		print(mean(SmallTreeData$Elevation[z==1]))
	}






########################################
#Average elevation for each Cover Type:#
########################################
#elevation, Aspect, Slope, H and V distance to hydrology, h distance to roads and firepoints, hillside shade 9, noon, and 3

for (i in seq(1:7))
  {
    print(paste(round((mean(SmallTreeData$Hillshade_3pm[SmallTreeData$Cover_Type==i])))))
  }


range(FullTreeData$Aspect)





################################################
#Average elevation for each cover type by area:#
################################################
for (a in seq(1:4))
  {
    assign("x", paste("SmallTreeData$Wilderness_Area", a, sep=""))
    z <- eval(parse(text=x))

    for (c in seq(1:7))
      {
        print(paste(c, round(mean(SmallTreeData$Elevation[SmallTreeData$Cover_Type==c&z==1]))))
      }
  }




#######################
#distance to firepoint#
#######################
mean(SmallTreeData$Horizontal_Distance_To_Fire_Points)




##########################################
#mean distance to firepoint for each area#
##########################################
mean(SmallTreeData$Horizontal_Distance_To_Fire_Points[SmallTreeData$Wilderness_Area1==1])
mean(SmallTreeData$Horizontal_Distance_To_Fire_Points[SmallTreeData$Wilderness_Area2==1])
mean(SmallTreeData$Horizontal_Distance_To_Fire_Points[SmallTreeData$Wilderness_Area3==1])
mean(SmallTreeData$Horizontal_Distance_To_Fire_Points[SmallTreeData$Wilderness_Area4==1])

for (i in seq(1:7))
  {
    print(paste((mean(SmallTreeData$Horizontal_Distance_To_Fire_Points[SmallTreeData$Cover_Type==i&SmallTreeData$Wilderness_Area4==1]))))
  }


#Rounded
for (i in seq(1:7))
  {
    print(paste(round((mean(SmallTreeData$Horizontal_Distance_To_Fire_Points[SmallTreeData$Cover_Type==i])))))
  }




##################################
#Slope Average across all regions#
##################################
for (i in seq(1:4))
  {
    assign("Fragment1", paste("SmallTreeData$Wilderness_Area", i, sep=""))
    z <- eval(parse(text=Fragment1))
    print(paste(mean(SmallTreeData$Slope&z==1)))
  }




##################################################
#Slope Average for all species across all regions#
##################################################
  for (e in seq(1:4))
	  {
	    assign("Fragment1", paste("SmallTreeData$Wilderness_Area", e, sep=""))
	    x <- eval(parse(text=Fragment1))
	    for (i in seq(1:7))
	         {
	           assign("Fragment2", paste("SmallTreeData$Cover_Type==", i, sep=""))
	           y <- eval(parse(text=Fragment2))
	           print(paste(sep = "", mean(SmallTreeData$Slope&x==1&y)))
	         }
	    cat("\n")
	  }




####################
#Averages in aspect#
####################
mean(SmallTreeData$Aspect)
range(FullTreeData$Aspect)




####################################################
#Average aspect across all regions for all species #
####################################################
for (e in seq(1:4))
	{
	  assign("Fragment1", paste("SmallTreeData$Wilderness_Area", e, sep=""))
	  x <- eval(parse(text=Fragment1))
	  for (i in seq(1:7))
	  {
	    assign("Fragment2", paste("SmallTreeData$Cover_Type==", i, sep=""))
	    y <- eval(parse(text=Fragment2))
	    print(paste(sep = "", mean(SmallTreeData$Aspect&x==1&y)*1000))
	  }
	  cat("\n")
	}




####################################################
#Average aspect across a region for each species   #
####################################################
par(mfrow=c(2,4))
plot(table(FullTreeData$Aspect[FullTreeData$Wilderness_Area1==1&FullTreeData$Cover_Type==1]), xlim=c(0, 360), xlab=c("Cover Type 1"), ylab=c("Occurances"))
plot(table(FullTreeData$Aspect[FullTreeData$Wilderness_Area1==1&FullTreeData$Cover_Type==2]), xlim=c(0, 360), xlab=c("Cover Type 2"), ylab=c("Occurances"))
plot(table(FullTreeData$Aspect[FullTreeData$Wilderness_Area1==1&FullTreeData$Cover_Type==3]), xlim=c(0, 360), xlab=c("Cover Type 3"), ylab=c("Occurances"))
plot(table(FullTreeData$Aspect[FullTreeData$Wilderness_Area1==1&FullTreeData$Cover_Type==4]), xlim=c(0, 360), xlab=c("Cover Type 4"), ylab=c("Occurances"))
plot(table(FullTreeData$Aspect[FullTreeData$Wilderness_Area1==1&FullTreeData$Cover_Type==5]), xlim=c(0, 360), xlab=c("Cover Type 5"), ylab=c("Occurances"))
plot(table(FullTreeData$Aspect[FullTreeData$Wilderness_Area1==1&FullTreeData$Cover_Type==6]), xlim=c(0, 360), xlab=c("Cover Type 6"), ylab=c("Occurances"))
plot(table(FullTreeData$Aspect[FullTreeData$Wilderness_Area1==1&FullTreeData$Cover_Type==7]), xlim=c(0, 360), xlab=c("Cover Type 7"), ylab=c("Occurances"))
title("Area 1", line = -1, outer = TRUE)
