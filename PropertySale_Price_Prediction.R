getwd()
setwd("C:\\Users\\Pranita\\Desktop\\property_project")
getwd()
Test_Data = read.csv("Property_Price_Test.csv")
View(Test_Data)

getwd()
Train_Data = read.csv("Property_Price_Train.csv")

View(Train_Data)

dim(Train_Data)
dim(Test_Data)


Train_Data$Source = "Train"
View(Train_Data)
dim(Train_Data)

Test_Data$Source = "Test"
View(Test_Data)
dim(Test_Data)



Train_Data1 = subset(Train_Data, select = -c(Sale_Price))
View(Train_Data1)
dim(Train_Data1)

Full_Data = rbind(Train_Data1,Test_Data)
View(Full_Data)
dim(Full_Data)

colSums(is.na(Full_Data))
summary(Full_Data)
summary(Train_Data$Sale_Price)
########################################
#NA values detection and treatment
########################################

#NA values for Dep vari
View(Train_Data$Sale_Price)
length(which(is.na("Sale_Price")))

#NA values for continuous variables : Here we have 11 continuous variables with NA values

#1 Lot_Extent
#S1 : subset and find median
x=median(Full_Data$Lot_Extent,na.rm = TRUE)

#S2 : Impute
Full_Data$Lot_Extent[is.na(Full_Data$Lot_Extent)] = x

#S3 : Validate
summary(Full_Data)

#2 Brick_Veneer_Area
#S1 : subset and find median
x=median(Full_Data$Brick_Veneer_Area,na.rm = TRUE)

#S2 : Impute
Full_Data$Brick_Veneer_Area[is.na(Full_Data$Brick_Veneer_Area)] = x

#S3 : Validate
summary(Full_Data)

#3 BsmtFinSF1
#S1 : subset and find median
x=median(Full_Data$BsmtFinSF1,na.rm = TRUE)

#S2 : Impute
Full_Data$BsmtFinSF1[is.na(Full_Data$BsmtFinSF1)] = x

#S3 : Validate
summary(Full_Data)

#4 BsmtFinSF2
#S1 : subset and find median
x=median(Full_Data$BsmtFinSF2,na.rm = TRUE)

#S2 : Impute
Full_Data$BsmtFinSF2[is.na(Full_Data$BsmtFinSF2)] = x

#S3 : Validate
summary(Full_Data)


#5 BsmtUnfSF
#S1 : subset and find median
x=median(Full_Data$BsmtUnfSF,na.rm = TRUE)

#S2 : Impute
Full_Data$BsmtUnfSF[is.na(Full_Data$BsmtUnfSF)] = x

#S3 : Validate
summary(Full_Data)

#6 Total_Basement_Area
#S1 : subset and find median
x=median(Full_Data$Total_Basement_Area,na.rm = TRUE)

#S2 : Impute
Full_Data$Total_Basement_Area[is.na(Full_Data$Total_Basement_Area)] = x

#S3 : Validate
summary(Full_Data)


#7 Underground_Full_Bathroom
#S1 : subset and find median
x=median(Full_Data$Underground_Full_Bathroom,na.rm = TRUE)

#S2 : Impute
Full_Data$Underground_Full_Bathroom[is.na(Full_Data$Underground_Full_Bathroom)] = x

#S3 : Validate
summary(Full_Data)



#8 Underground_Half_Bathroom
#S1 : subset and find median
x=median(Full_Data$Underground_Half_Bathroom,na.rm = TRUE)

#S2 : Impute
Full_Data$Underground_Half_Bathroom[is.na(Full_Data$Underground_Half_Bathroom)] = x

#S3 : Validate
summary(Full_Data)


#9 Garage_Built_Year
#S1 : subset and find median
x=median(Full_Data$Garage_Built_Year,na.rm = TRUE)

#S2 : Impute
Full_Data$Garage_Built_Year[is.na(Full_Data$Garage_Built_Year)] = x

#S3 : Validate
summary(Full_Data)


#10 Garage_Size
#S1 : subset and find median
x=median(Full_Data$Garage_Size,na.rm = TRUE)

#S2 : Impute
Full_Data$Garage_Size[is.na(Full_Data$Garage_Size)] = x

#S3 : Validate
summary(Full_Data)


#11 Garage_Area
#S1 : subset and find median
x=median(Full_Data$Garage_Area,na.rm = TRUE)

#S2 : Impute
Full_Data$Garage_Area[is.na(Full_Data$Garage_Area)] = x

#S3 : Validate
summary(Full_Data)

#NA values detection and treatment for categorical variables: Here we have 23 categorical variables wih NA values

#1 Zoning_Class
#Step1: Subset the traindata from the full data.
a=data.frame(table(Full_Data$Zoning_Class))

#step2 : Find the mode for categorical value with max value
#Find the category with the heighest freq
b=a$Var1[a$Freq == max(a$Freq)]

#Step 3: Impute na values with b whc we got in step 2
Full_Data$Zoning_Class[is.na(Full_Data$Zoning_Class)] = b

#Validate as to no na values 
summary(Full_Data)


#2 Lane_Type
#Step1: Subset the traindata from the full data.
a=data.frame(table(Full_Data$Lane_Type))

#step2 : Find the mode for categorical value with max value
#Find the category with the heighest freq
b=a$Var1[a$Freq == max(a$Freq)]

#Step 3: Impute na values with b whc we got in step 2
Full_Data$Lane_Type[is.na(Full_Data$Lane_Type)] = b

#Validate as to no na values 
summary(Full_Data)


#3 Utility_Type
#Step1: Subset the traindata from the full data.
a=data.frame(table(Full_Data$Utility_Type))

#step2 : Find the mode for categorical value with max value
#Find the category with the heighest freq
b=a$Var1[a$Freq == max(a$Freq)]

#Step 3: Impute na values with b whc we got in step 2
Full_Data$Utility_Type[is.na(Full_Data$Utility_Type)] = b

#Validate as to no na values 
summary(Full_Data)


#4 Exterior1st
#Step1: Subset the traindata from the full data.
a=data.frame(table(Full_Data$Exterior1st))

#step2 : Find the mode for categorical value with max value
#Find the category with the heighest freq
b=a$Var1[a$Freq == max(a$Freq)]

#Step 3: Impute na values with b whc we got in step 2
Full_Data$Exterior1st[is.na(Full_Data$Exterior1st)] = b

#Validate as to no na values 
summary(Full_Data)


#5 Exterior2nd
#Step1: Subset the traindata from the full data.
a=data.frame(table(Full_Data$Exterior2nd))

#step2 : Find the mode for categorical value with max value
#Find the category with the heighest freq
b=a$Var1[a$Freq == max(a$Freq)]

#Step 3: Impute na values with b whc we got in step 2
Full_Data$Exterior2nd[is.na(Full_Data$Exterior2nd)] = b

#Validate as to no na values 
summary(Full_Data)

#6 Brick_Veneer_Type
#Step1: Subset the traindata from the full data.
a=data.frame(table(Full_Data$Brick_Veneer_Type))

#step2 : Find the mode for categorical value with max value
#Find the category with the heighest freq
b=a$Var1[a$Freq == max(a$Freq)]

#Step 3: Impute na values with b whc we got in step 2
Full_Data$Brick_Veneer_Type[is.na(Full_Data$Brick_Veneer_Type)] = b


#Validate as to no na values 
summary(Full_Data)




#7 Basement_Height
#Step1: Subset the traindata from the full data.
a=data.frame(table(Full_Data$Basement_Height))

#step2 : Find the mode for categorical value with max value
#Find the category with the heighest freq
b=a$Var1[a$Freq == max(a$Freq)]

#Step 3: Impute na values with b whc we got in step 2
Full_Data$Basement_Height[is.na(Full_Data$Basement_Height)] = b

#Validate as to no na values 
summary(Full_Data)


#8 Basement_Condition
#Step1: Subset the traindata from the full data.
a=data.frame(table(Full_Data$Basement_Condition))

#step2 : Find the mode for categorical value with max value
#Find the category with the heighest freq
b=a$Var1[a$Freq == max(a$Freq)]

#Step 3: Impute na values with b whc we got in step 2
Full_Data$Basement_Condition[is.na(Full_Data$Basement_Condition)] = b

#Validate as to no na values 
summary(Full_Data)


#9 Exposure_Level
#Step1: Subset the traindata from the full data.
a=data.frame(table(Full_Data$Exposure_Level))

#step2 : Find the mode for categorical value with max value
#Find the category with the heighest freq
b=a$Var1[a$Freq == max(a$Freq)]

#Step 3: Impute na values with b whc we got in step 2
Full_Data$Exposure_Level[is.na(Full_Data$Exposure_Level)] = b

#Validate as to no na values 
summary(Full_Data)


#10 BsmtFinType1
#Step1: Subset the traindata from the full data.
a=data.frame(table(Full_Data$BsmtFinType1))

#step2 : Find the mode for categorical value with max value
#Find the category with the heighest freq
b=a$Var1[a$Freq == max(a$Freq)]

#Step 3: Impute na values with b whc we got in step 2
Full_Data$BsmtFinType1[is.na(Full_Data$BsmtFinType1)] = b

#Validate as to no na values 
summary(Full_Data)

#11 BsmtFinType2
#Step1: Subset the traindata from the full data.
a=data.frame(table(Full_Data$BsmtFinType2))

#step2 : Find the mode for categorical value with max value
#Find the category with the heighest freq
b=a$Var1[a$Freq == max(a$Freq)]

#Step 3: Impute na values with b whc we got in step 2
Full_Data$BsmtFinType2[is.na(Full_Data$BsmtFinType2)] = b

#Validate as to no na values 
summary(Full_Data)


#12 Electrical_System
#Step1: Subset the traindata from the full data.
a=data.frame(table(Full_Data$Electrical_System))

#step2 : Find the mode for categorical value with max value
#Find the category with the heighest freq
b=a$Var1[a$Freq == max(a$Freq)]

#Step 3: Impute na values with b whc we got in step 2
Full_Data$Electrical_System[is.na(Full_Data$Electrical_System)] = b

#Validate as to no na values 
summary(Full_Data)



#13 Kitchen_Quality
#Step1: Subset the traindata from the full data.
a=data.frame(table(Full_Data$Kitchen_Quality))

#step2 : Find the mode for categorical value with max value
#Find the category with the heighest freq
b=a$Var1[a$Freq == max(a$Freq)]

#Step 3: Impute na values with b whc we got in step 2
Full_Data$Kitchen_Quality[is.na(Full_Data$Kitchen_Quality)] = b

#Validate as to no na values 
summary(Full_Data)


#14 Functional_Rate
#Step1: Subset the traindata from the full data.
a=data.frame(table(Full_Data$Functional_Rate))

#step2 : Find the mode for categorical value with max value
#Find the category with the heighest freq
b=a$Var1[a$Freq == max(a$Freq)]

#Step 3: Impute na values with b whc we got in step 2
Full_Data$Functional_Rate[is.na(Full_Data$Functional_Rate)] = b

#Validate as to no na values 
summary(Full_Data)


#15 Fireplace_Quality
#Step1: Subset the traindata from the full data.
a=data.frame(table(Full_Data$Fireplace_Quality))

#step2 : Find the mode for categorical value with max value
#Find the category with the heighest freq
b=a$Var1[a$Freq == max(a$Freq)]

#Step 3: Impute na values with b whc we got in step 2
Full_Data$Fireplace_Quality[is.na(Full_Data$Fireplace_Quality)] = b

#Validate as to no na values 
summary(Full_Data)


#16 Garage
#Step1: Subset the traindata from the full data.
a=data.frame(table(Full_Data$Garage))

#step2 : Find the mode for categorical value with max value
#Find the category with the heighest freq
b=a$Var1[a$Freq == max(a$Freq)]

#Step 3: Impute na values with b whc we got in step 2
Full_Data$Garage[is.na(Full_Data$Garage)] = b

#Validate as to no na values 
summary(Full_Data)


#17 Garage_Finish_Year
#Step1: Subset the traindata from the full data.
a=data.frame(table(Full_Data$Garage_Finish_Year))

#step2 : Find the mode for categorical value with max value
#Find the category with the heighest freq
b=a$Var1[a$Freq == max(a$Freq)]

#Step 3: Impute na values with b whc we got in step 2
Full_Data$Garage_Finish_Year[is.na(Full_Data$Garage_Finish_Year)] = b

#Validate as to no na values 
summary(Full_Data)


#18 Garage_Quality
#Step1: Subset the traindata from the full data.
a=data.frame(table(Full_Data$Garage_Quality))

#step2 : Find the mode for categorical value with max value
#Find the category with the heighest freq
b=a$Var1[a$Freq == max(a$Freq)]

#Step 3: Impute na values with b whc we got in step 2
Full_Data$Garage_Quality[is.na(Full_Data$Garage_Quality)] = b

#Validate as to no na values 
summary(Full_Data)

#19 Garage_Condition
#Step1: Subset the traindata from the full data.
a=data.frame(table(Full_Data$Garage_Condition))

#step2 : Find the mode for categorical value with max value
#Find the category with the heighest freq
b=a$Var1[a$Freq == max(a$Freq)]

#Step 3: Impute na values with b whc we got in step 2
Full_Data$Garage_Condition[is.na(Full_Data$Garage_Condition)] = b

#Validate as to no na values 
summary(Full_Data)


#20 Pool_Quality
#Step1: Subset the traindata from the full data.
a=data.frame(table(Full_Data$Pool_Quality))

#step2 : Find the mode for categorical value with max value
#Find the category with the heighest freq
b=a$Var1[a$Freq == max(a$Freq)]

#Step 3: Impute na values with b whc we got in step 2
Full_Data$Pool_Quality[is.na(Full_Data$Pool_Quality)] = b

#Validate as to no na values 
summary(Full_Data)

#21 Fence_Quality
#Step1: Subset the traindata from the full data.
a=data.frame(table(Full_Data$Fence_Quality))

#step2 : Find the mode for categorical value with max value
#Find the category with the heighest freq
b=a$Var1[a$Freq == max(a$Freq)]

#Step 3: Impute na values with b whc we got in step 2
Full_Data$Fence_Quality[is.na(Full_Data$Fence_Quality)] = b

#Validate as to no na values 
summary(Full_Data)


#22 Miscellaneous_Feature
#Step1: Subset the traindata from the full data.
a=data.frame(table(Full_Data$Miscellaneous_Feature))

#step2 : Find the mode for categorical value with max value
#Find the category with the heighest freq
b=a$Var1[a$Freq == max(a$Freq)]

#Step 3: Impute na values with b whc we got in step 2
Full_Data$Miscellaneous_Feature[is.na(Full_Data$Miscellaneous_Feature)] = b

#Validate as to no na values 
summary(Full_Data)


#23 Sale_Type
#Step1: Subset the traindata from the full data.
a=data.frame(table(Full_Data$Sale_Type))

#step2 : Find the mode for categorical value with max value
#Find the category with the heighest freq
b=a$Var1[a$Freq == max(a$Freq)]

#Step 3: Impute na values with b whc we got in step 2
Full_Data$Sale_Type[is.na(Full_Data$Sale_Type)] = b

#Validate as to no na values 
summary(Full_Data)


#Validate Full_Data so as to not have any NA value
colSums(is.na(Full_Data))
summary(Full_Data)

#################################
#Outlier detection and treatment
#################################
#Check the class of each variable
str(Full_Data)
#get the categorical variables
## converting categorical columns to factors
cat_variables = c("Zoning_Class","Road_Type","Lane_Type","Property_Shape","Land_Outline","Utility_Type",
                  "Lot_Configuration","Property_Slope","Neighborhood","Condition1","Condition2","House_Type",
                  "House_Design","Roof_Design","Roof_Quality","Exterior1st","Exterior2nd","Brick_Veneer_Type",
                  "Exterior_Material","Exterior_Condition","Foundation_Type","Basement_Height","Basement_Condition",
                  "Foundation_Type","Basement_Height","Basement_Condition","Exposure_Level","BsmtFinType1","BsmtFinType2",
                  "Heating_Type","Heating_Quality","Air_Conditioning","Electrical_System","Kitchen_Quality","Functional_Rate",
                  "Fireplace_Quality","Garage","Garage_Finish_Year","Garage_Quality","Garage_Condition","Pavedd_Drive",
                  "Pool_Quality","Fence_Quality","Miscellaneous_Feature","Sale_Type","Sale_Condition","Source")


factors1.df=Full_Data[,cat_variables]
View(factors1.df)
length(cat_variables)



#Creating dummy variables
Dummy_DF = model.matrix(~ Zoning_Class + Road_Type + Lane_Type + Property_Shape + Land_Outline + Utility_Type + 
                        Lot_Configuration + Property_Slope + Neighborhood + Condition1 + Condition2 + House_Type + 
                        House_Design + Roof_Design + Roof_Quality + Exterior1st + Exterior2nd + Brick_Veneer_Type + 
                        Exterior_Material + Exterior_Condition + Foundation_Type + Basement_Height + Basement_Condition + 
                        Foundation_Type + Basement_Height + Basement_Condition + Exposure_Level + BsmtFinType1 + BsmtFinType2 + 
                        Heating_Type + Heating_Quality + Air_Conditioning + Electrical_System + Kitchen_Quality + Functional_Rate + 
                        Fireplace_Quality + Garage + Garage_Finish_Year + Garage_Quality + Garage_Condition + Pavedd_Drive + 
                        Pool_Quality + Fence_Quality + Miscellaneous_Feature + Sale_Type + Sale_Condition, Full_Data)


View(Dummy_DF)
nrow(Dummy_DF)
ncol(Dummy_DF)
dim(Dummy_DF)
dim(Full_Data)

Full_Data2 = cbind(Full_Data,Dummy_DF[,-1])
View(Full_Data2)
dim(Full_Data2)


Full_Data3 = subset(Full_Data2, select = -c(Zoning_Class,Road_Type,Lane_Type,Property_Shape,Land_Outline,Utility_Type,
                                              Lot_Configuration,Property_Slope,Neighborhood,Condition1,Condition2,House_Type, 
                                              House_Design,Roof_Design,Roof_Quality,Exterior1st,Exterior2nd,Brick_Veneer_Type,
                                              Exterior_Material,Exterior_Condition,Foundation_Type,Basement_Height,Basement_Condition,
                                              Foundation_Type,Basement_Height,Basement_Condition,Exposure_Level,BsmtFinType1,BsmtFinType2,
                                              Heating_Type,Heating_Quality,Air_Conditioning,Electrical_System,Kitchen_Quality,Functional_Rate,
                                              Fireplace_Quality,Garage,Garage_Finish_Year,Garage_Quality,Garage_Condition,Pavedd_Drive, 
                                              Pool_Quality,Fence_Quality,Miscellaneous_Feature,Sale_Type,Sale_Condition))
                    
View(Full_Data3)
dim(Full_Data3)


#Divide the data into train and test data based on the Source column
#and make sure that the source column shd be dropped
Trainset = subset(Full_Data3,subset = (Source == "Train"), select = -Source)
Test = subset(Full_Data3,subset = (Source == "Test"), select = -Source)

View(Trainset)
dim(Trainset)


View(Test)
dim(Test)

Sales_Price = Train_Data$Sale_Price
View(Sales_Price)
dim(Sales_Price)

Train = cbind(Trainset,Sales_Price)
View(Train)
dim(Train)

#Model building
install.packages("tidyverse")
library(tidyverse)
install.packages("haven")
library(haven)
install.packages("car")
library(car)
library(forecast)

library(caTools)
M0 = lm(Sales_Price~.,data = Train,singular.ok = TRUE)
#vif(M0)
#alias(M0)
#car::vif(M0)
M0
summary(M0)

#Removing and updation of our model by rmvg the insignificant ind vari based on p-value
M1 = update(M0, . ~ . -Heating_QualityPo)
summary(M1)

M2 = update(M1, . ~ . -Electrical_SystemFuseP)
summary(M2)


M3 = update(M2, . ~ . -BsmtFinType1Rec)
summary(M3)


M4 = update(M3, . ~ . -NeighborhoodMeadowV)
summary(M4)

M5 = update(M4, . ~ . -`Exterior2ndWd Shng`)
summary(M5)

M6 = update(M5, . ~ . -Exterior1stBrkComm)
summary(M6)

M7 = update(M6, . ~ . -Lane_TypePaved)
summary(M7)

M8 = update(M7, . ~ . -Underground_Half_Bathroom)
summary(M8)

M9 = update(M8, . ~ . -Functional_RateMajD2)
summary(M9)

M10 = update(M9, . ~ . -Condition1RRNe)
summary(M10)

M11 = update(M10, . ~ . -Heating_QualityFa)
summary(M11)

M12 = update(M11, . ~ . -Condition2Feedr)
summary(M12)

M13 = update(M12, . ~ . -NeighborhoodSomerst)
summary(M13)

M14 = update(M13, . ~ . -House_Type2fmCon)
summary(M14)

M15 = update(M14, . ~ . -Heating_TypeGasA)
summary(M15)

M16 = update(M15, . ~ . -Basement_ConditionGd)
summary(M16)

M17 = update(M16, . ~ . -Exterior_ConditionFa)
summary(M17)

M18 = update(M17, . ~ . -Air_ConditioningY)
summary(M18)

M19 = update(M18, . ~ . -Fireplace_QualityTA)
summary(M19)

M20 = update(M19, . ~ . -Fireplace_QualityGd)
summary(M20)

M21 = update(M20, . ~ . -NeighborhoodVeenker)
summary(M21)

M22 = update(M21, . ~ . -Exterior_ConditionTA)
summary(M22)

M23 = update(M22, . ~ . -Exterior2ndStucco)
summary(M23)

M24 = update(M23, . ~ . -Garage_Finish_YearUnf)
summary(M24)

M25 = update(M24, . ~ . -Exterior1stStucco)
summary(M25)

M26 = update(M25, . ~ . -Exterior1stMetalSd)
summary(M26)

M27 = update(M26, . ~ . -Exterior2ndMetalSd)
summary(M27)

M28 = update(M27, . ~ . -Exterior2ndBrkFace)
summary(M28)

M29 = update(M28, . ~ . -House_DesignSFoyer)
summary(M29)

M30 = update(M29, . ~ . -BsmtFinType2GLQ)
summary(M30)

M31 = update(M30, . ~ . -`Exterior2ndBrk Cmn`)
summary(M31)

M32 = update(M31, . ~ . -Exterior2ndAsphShn)
summary(M32)

M33 = update(M32, . ~ . -Exterior1stAsphShn)
summary(M33)

M34 = update(M33, . ~ . -Roof_DesignHip)
summary(M34)

M35 = update(M34, . ~ . -Roof_DesignGable)
summary(M35)

M36 = update(M35, . ~ . -NeighborhoodBrDale)
summary(M36)

M37 = update(M36, . ~ . -Condition2RRNn)
summary(M37)

M38 = update(M37, . ~ . -Electrical_SystemFuseF)
summary(M38)

M39 = update(M38, . ~ . -Pool_QualityGd)
summary(M39)

M40 = update(M39, . ~ . -Property_ShapeIR3)
summary(M40)

M41 = update(M40, . ~ . -Garage_Area)
summary(M41)

M42 = update(M41, . ~ . -Miscellaneous_FeatureTenC)
summary(M42)

M43 = update(M42, . ~ . -LowQualFinSF)
summary(M43)

M44 = update(M43, . ~ . -Grade_Living_Area)
summary(M44)
   
M45 = update(M44, . ~ . -Pavedd_DriveY)
summary(M45)

M46 = update(M45, . ~ . -Sale_TypeConLw)
summary(M46)

M47 = update(M46, . ~ . -Id)
summary(M47)

M48 = update(M47, . ~ . -Heating_TypeGrav)
summary(M48)

M49 = update(M48, . ~ . -Exterior1stStone)
summary(M49)

M50 = update(M49, . ~ . -Sale_TypeWD)
summary(M50)

M51 = update(M50, . ~ . -Heating_TypeGasW)
summary(M51)

M52 = update(M51, . ~ . -Sale_ConditionAlloca)
summary(M52)

M53 = update(M52, . ~ . -Fence_QualityMnWw)
summary(M53)

M54 = update(M53, . ~ . -Sale_ConditionFamily)
summary(M54)

M55 = update(M54, . ~ . -NeighborhoodBlueste)
summary(M55)

M56 = update(M55, . ~ . -Sale_ConditionPartial)
summary(M56)

M57 = update(M56, . ~ . -House_DesignSLvl)
summary(M57)

M58 = update(M57, . ~ . -Total_Basement_Area)
summary(M58)

M59 = update(M58, . ~ . -Condition1NoRMD)
summary(M59)

M60 = update(M59, . ~ . -Exterior1stCB)
summary(M60)

M61 = update(M60, . ~ . -Condition2NoRMD)
summary(M61)

M62 = update(M61, . ~ . -Exterior2ndCBlock)
summary(M62)

M63 = update(M62, . ~ . -Functional_RateMod)
summary(M63)

M64 = update(M63, . ~ . -Functional_RateSev)
summary(M64)

M65 = update(M64, . ~ . -Sale_ConditionAbnoRMDl)
summary(M65)


M66 = update(M65, . ~ . -Sale_ConditionNoRMDal)
summary(M66)






