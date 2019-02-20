SmallTreeData <- read.csv("/home/aggitan/Projects/Data_Science/Project_1/SampleTreeDataExport.csv")
#means

mean(SmallTreeData$Elevation)

#Average elevation for area 1
mean(SmallTreeData$Elevation[SmallTreeData$Wilderness_Area4==1])

########################################
#Average elevation for each Cover Type:#
########################################

for (i in seq(1:7))
  {
    print(paste("Cover Type:", i, " - Average Elevation: ", (mean(SmallTreeData$Elevation[SmallTreeData$Cover_Type==i]))))
  }




################################################
#Average elevation for each cover type by area:#
################################################
for (i in seq(1:7))
{
  print(paste((mean(SmallTreeData$Elevation[SmallTreeData$Cover_Type==i&SmallTreeData$Wilderness_Area4==1]))))
}




#######################
#distance to firepoint#
#######################
mean(SmallTreeData$Horizontal_Distance_To_Fire_Points)

mean(SmallTreeData$Horizontal_Distance_To_Fire_Points[SmallTreeData$Wilderness_Area1==1])
mean(SmallTreeData$Horizontal_Distance_To_Fire_Points[SmallTreeData$Wilderness_Area2==1])
mean(SmallTreeData$Horizontal_Distance_To_Fire_Points[SmallTreeData$Wilderness_Area3==1])
mean(SmallTreeData$Horizontal_Distance_To_Fire_Points[SmallTreeData$Wilderness_Area4==1])

for (i in seq(1:7))
{
  print(paste((mean(SmallTreeData$Horizontal_Distance_To_Fire_Points[SmallTreeData$Cover_Type==i&SmallTreeData$Wilderness_Area4==1]))))
}


for (a in seq(1:4))
{
  for (c in seq(1:7))
    {
      assign("x", paste("SmallTreeData$Wilderness_Area", a, sep=""))
      z <- eval(parse(text=x))    
      print(paste("Cover type:", c, sum(SmallTreeData$Cover_Type==c&z==1), "Area:", a ))
    }
}

countvectors <- function()
  {
    array <- c(sum(SmallTreeData$Cover_Type==1), sum(SmallTreeData$Cover_Type==2),
                sum(SmallTreeData$Cover_Type==3), sum(SmallTreeData$Cover_Type==4)
            )
    return(ct)
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




####################################################
#Average aspect across all regions for each species#
####################################################
for (e in seq(1:4))
{
  assign("Fragment1", paste("SmallTreeData$Wilderness_Area", e, sep=""))
  x <- eval(parse(text=Fragment1))
  for (i in seq(1:7))
  {
    assign("Fragment2", paste("SmallTreeData$Cover_Type==", i, sep=""))
    y <- eval(parse(text=Fragment2))         
    print(paste(sep = "", mean(SmallTreeData$Aspect&x==1&y)))
  }
  cat("\n")
}
