SmallTreeData <- read.csv("/home/aggitan/Projects/Data_Science/Project_1/SampleTreeDataExport.csv")
#FullTreeData <- read.csv("/home/aggitan/Projects/Data_Science/Project_1/covtype.csv")



#############################
#How many columns are there?#
#############################
length(SmallTreeData)




##################################
#How many of each tree are there?#
##################################
for (i in seq(1:7))
  {
    print(sum(SmallTreeData$Cover_Type==i))
  }




#####################################
#How many of each is in each region?#
#####################################
SumByArea <- function(a)
  {
    #Let's turn a into a index reference we can use called z
    assign("Fragment1", paste("SmallTreeData$Wilderness_Area", a, "==1", sep=""))
    x <- eval(parse(text=Fragment1))

    #Let's iterate through the cover types
    for (obv in seq(1:7))
      {
        ct1 <- sum(SmallTreeData$Cover_Type==obv&x==1)
        print(ct1)
      }
  }

#Let's have SumByArea create a printout for each of the 4 areas
for (i in seq(1:4))
  {
    SumByArea(i)
    cat("\n")
  }




######################################
#How much of each soil type is there?#
######################################
for(e in seq(1:4))
  {
    assign("Fragment2", paste("SmallTreeData$Wilderness_Area", e, "==1", sep=""))
    y <- eval(parse(text=Fragment2))
    print(paste("Area:", e))
    for(i in seq(1:40))
      {
        assign("Fragment1", paste("SmallTreeData$Soil_Type", i, "==1", sep=""))
        x <- eval(parse(text=Fragment1))
        print(paste(sep = "", sum(x&y)))
      }
  }

for (i in seq(1:40))
  {
    print(paste(sep = "", "Soil_Type", i))
  }




#############################################################
#Howmany soil observations were there per area per soil type#
#############################################################
All_Soil_Types  <- data.frame(SmallTreeData$Soil_Type1, SmallTreeData$Soil_Type2,
                              SmallTreeData$Soil_Type3, SmallTreeData$Soil_Type4,
                              SmallTreeData$Soil_Type5, SmallTreeData$Soil_Type6,
                              SmallTreeData$Soil_Type7, SmallTreeData$Soil_Type8,
                              SmallTreeData$Soil_Type9, SmallTreeData$Soil_Type10,
                              SmallTreeData$Soil_Type11, SmallTreeData$Soil_Type12,
                              SmallTreeData$Soil_Type13, SmallTreeData$Soil_Type14,
                              SmallTreeData$Soil_Type15, SmallTreeData$Soil_Type16,
                              SmallTreeData$Soil_Type17, SmallTreeData$Soil_Type18,
                              SmallTreeData$Soil_Type19, SmallTreeData$Soil_Type20,
                              SmallTreeData$Soil_Type21, SmallTreeData$Soil_Type22,
                              SmallTreeData$Soil_Type23, SmallTreeData$Soil_Type24,
                              SmallTreeData$Soil_Type25, SmallTreeData$Soil_Type26,
                              SmallTreeData$Soil_Type27, SmallTreeData$Soil_Type28,
                              SmallTreeData$Soil_Type29, SmallTreeData$Soil_Type30,
                              SmallTreeData$Soil_Type31, SmallTreeData$Soil_Type32,
                              SmallTreeData$Soil_Type33, SmallTreeData$Soil_Type34,
                              SmallTreeData$Soil_Type35, SmallTreeData$Soil_Type36,
                              SmallTreeData$Soil_Type37, SmallTreeData$Soil_Type38,
                              SmallTreeData$Soil_Type39, SmallTreeData$Soil_Type40)

SoilDistributionMatrix <- matrix(0, ncol = 4, nrow = length(All_Soil_Types))
colnames(SoilDistributionMatrix) <- c("Area 1", "Area 2", "Area 3", "Area 4")
SoilDistributionMatrix_FullNames <-unlist(strsplit(colnames(All_Soil_Types), split = ".", fixed = TRUE))
SoilDistributionMatrix_Remove <- c("SmallTreeData")
rownames(SoilDistributionMatrix) <- setdiff(SoilDistributionMatrix_FullNames, SoilDistributionMatrix_Remove)

for (obv in seq(1:length(All_Soil_Types)))
  {

    Fragment1 <- paste("as.matrix(subset(SmallTreeData, Soil_Type", obv,
                       '=="1", select = Soil_Type', obv, "))", sep="")

    x <- eval(parse(text=Fragment1))
    x
    for (er in 1:nrow(x))
      {
        if(length(x == "0") > 0)
          {
            if (SmallTreeData$Wilderness_Area1[er] == "1")
              {
                SoilDistributionMatrix[obv,1] = SoilDistributionMatrix[obv,1]++1
              }
            else if (SmallTreeData$Wilderness_Area2[er] == "1")
              {
                SoilDistributionMatrix[obv,2] = SoilDistributionMatrix[obv,2]++1
              }
            else if (SmallTreeData$Wilderness_Area3[er] == "1")
              {
                SoilDistributionMatrix[obv,3] = SoilDistributionMatrix[obv,3]++1
              }
            else if (SmallTreeData$Wilderness_Area4[er] == "1")
              {
                SoilDistributionMatrix[obv,4] = SoilDistributionMatrix[obv,4]++1
              }
          }
      }
  }
write.table(
  SoilDistributionMatrix,
  sep=",",
  append=FALSE,
  file ="wet_table.csv"
)




#############################################################
#Howmany tree observations were there per area per soil type#
#############################################################
RainFallMatrix_Maker <- function()
  {
    #SmallTreeData <- read.csv("FullTreeDataExport.csv")
    #FullTreeData <- read.csv("/home/aggitan/Projects/Data_Science/Project_1/SampleTreeDataExport.csv")
    #FullTreeData <- read.csv("/home/aggitan/Projects/Data_Science/Project_1/covtype.csv")
    FullTreeData <- read.csv("/home/aggitan/Projects/Data_Science/Project_1/SampleTreeDataExport.csv")

    #################################################################################
    #Let's make a box to put all the soil types in along with their noted cover type#
    #Let's make it a simple vector                                                  #
    #################################################################################
    Soil_Type_Presense <-c(FullTreeData$Soil_Type1, FullTreeData$Soil_Type2,
                           FullTreeData$Soil_Type3, FullTreeData$Soil_Type4,
                           FullTreeData$Soil_Type5, FullTreeData$Soil_Type6,
                           FullTreeData$Soil_Type7, FullTreeData$Soil_Type8,
                           FullTreeData$Soil_Type9, FullTreeData$Soil_Type10,
                           FullTreeData$Soil_Type11, FullTreeData$Soil_Type12,
                           FullTreeData$Soil_Type13, FullTreeData$Soil_Type14,
                           FullTreeData$Soil_Type15, FullTreeData$Soil_Type16,
                           FullTreeData$Soil_Type17, FullTreeData$Soil_Type18,
                           FullTreeData$Soil_Type19, FullTreeData$Soil_Type20,
                           FullTreeData$Soil_Type21, FullTreeData$Soil_Type22,
                           FullTreeData$Soil_Type23, FullTreeData$Soil_Type24,
                           FullTreeData$Soil_Type25, FullTreeData$Soil_Type26,
                           FullTreeData$Soil_Type27, FullTreeData$Soil_Type28,
                           FullTreeData$Soil_Type29, FullTreeData$Soil_Type30,
                           FullTreeData$Soil_Type31, FullTreeData$Soil_Type32,
                           FullTreeData$Soil_Type33, FullTreeData$Soil_Type34,
                           FullTreeData$Soil_Type35, FullTreeData$Soil_Type36,
                           FullTreeData$Soil_Type37, FullTreeData$Soil_Type38,
                           FullTreeData$Soil_Type39, FullTreeData$Soil_Type40)

    #################################################################################
    #Let's make a box to put all the soil types in along with their noted cover type#
    #Couldn't get the length, let's make it a data frame                            #
    #################################################################################

    Soil_Type_Presense_df  <- data.frame(FullTreeData$Soil_Type1, FullTreeData$Soil_Type2,
                                         FullTreeData$Soil_Type3, FullTreeData$Soil_Type4,
                                         FullTreeData$Soil_Type5, FullTreeData$Soil_Type6,
                                         FullTreeData$Soil_Type7, FullTreeData$Soil_Type8,
                                         FullTreeData$Soil_Type9, FullTreeData$Soil_Type10,
                                         FullTreeData$Soil_Type11, FullTreeData$Soil_Type12,
                                         FullTreeData$Soil_Type13, FullTreeData$Soil_Type14,
                                         FullTreeData$Soil_Type15, FullTreeData$Soil_Type16,
                                         FullTreeData$Soil_Type17, FullTreeData$Soil_Type18,
                                         FullTreeData$Soil_Type19, FullTreeData$Soil_Type20,
                                         FullTreeData$Soil_Type21, FullTreeData$Soil_Type22,
                                         FullTreeData$Soil_Type23, FullTreeData$Soil_Type24,
                                         FullTreeData$Soil_Type25, FullTreeData$Soil_Type26,
                                         FullTreeData$Soil_Type27, FullTreeData$Soil_Type28,
                                         FullTreeData$Soil_Type29, FullTreeData$Soil_Type30,
                                         FullTreeData$Soil_Type31, FullTreeData$Soil_Type32,
                                         FullTreeData$Soil_Type33, FullTreeData$Soil_Type34,
                                         FullTreeData$Soil_Type35, FullTreeData$Soil_Type36,
                                         FullTreeData$Soil_Type37, FullTreeData$Soil_Type38,
                                         FullTreeData$Soil_Type39, FullTreeData$Soil_Type40)

    ###########################################################################################
    #The box is going to be a matrix                                                          #
    #This box is too huge, columns exceed 500,000 observations                                #
    ###########################################################################################
    RainFallMatrix <- matrix(Soil_Type_Presense, byrow = FALSE,
                             ncol = length(array(FullTreeData$Cover_Type)),
                             nrow = ncol(Soil_Type_Presense_df)
    )
    dimnames(RainFallMatrix) <- list(gsub(".*\\.(.*)", "\\1", (colnames(Soil_Type_Presense_df))), FullTreeData$Cover_Type)

    #######################################################################
    #Let's see if we can put it in a smaller box, programatically         #
    #Let's create the frequency table to put everything in                #
    #######################################################################

    #The X coordinates; Range is the column names,
    #The min/max Range of the column names will be the x axis
    SortedRainMatrix_x <- c(range(FullTreeData$Cover_Type))
    SortedRainMatrix_x_Start <- SortedRainMatrix_x[1]
    SortedRainMatrix_x_End <- SortedRainMatrix_x[2]
    SortedRainMatrix_x_Range <- seq(SortedRainMatrix_x[1]:SortedRainMatrix_x[2])

    #The Y coordinates, A list of the column names
    SortedRainMatrix_y_Range <- gsub(".*\\.(.*)", "\\1", (colnames(Soil_Type_Presense_df)))

    #Rows are made, columns are made, let's put the box together:
    SortedRainMatrix=data.frame(matrix(0, nrow=length(SortedRainMatrix_y_Range), ncol=SortedRainMatrix_x_End))
    colnames(SortedRainMatrix) <- SortedRainMatrix_x_Range
    rownames(SortedRainMatrix) <- SortedRainMatrix_y_Range

    #Let's itterate through the data and add it to the dataframe
    for (r in 1:nrow(RainFallMatrix))
      {
        for (c in 1:ncol(RainFallMatrix))
          {
            if (RainFallMatrix[r,c] == 1)
              {
                RainMatrix_Soil_Type = rownames(RainFallMatrix)[r]
                RainMatrix_Cover_Type = FullTreeData$Cover_Type[c]
                SortedRainMatrix[RainMatrix_Soil_Type,RainMatrix_Cover_Type] = SortedRainMatrix[RainMatrix_Soil_Type,RainMatrix_Cover_Type]++1
              }
          }
      }

    write.table(
      SortedRainMatrix,
      sep=",",
      append=FALSE,
      col.names = TRUE,
      row.names = TRUE,
      file ="/home/aggitan/Projects/Data_Science/Project_1/Refinement/RainFallMatrix2.csv"
    )

    return(SortedRainMatrix)
  }

RainFallMatrix_Maker()