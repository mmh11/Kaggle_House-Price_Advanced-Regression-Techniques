library(ggplot2)
library(gridExtra)
train_data=read.csv("train.csv",header=T,stringsAsFactors = T)

test_data=read.csv("test.csv",header=T,stringsAsFactors = T)







formula=lm(data=train_data,formula=SalePrice~#MSSubClass+MSZoning+LotFrontage+LotArea#+Street+LotShape+LandContour+
             #LotConfig+LandSlope+Neighborhood+Condition1+Condition2+BldgType+HouseStyle+
             OverallQual+#OverallCond+
             YearBuilt+YearRemodAdd+#RoofStyle+RoofMatl+Exterior1st+Exterior2nd+MasVnrType+
             MasVnrArea+#ExterQual+ExterCond+Foundation+BsmtQual+BsmtCond+BsmtExposure+BsmtFinType1+
             #BsmtFinSF1+#BsmtFinType2+BsmtFinSF2+
             #BsmtUnfSF+
             TotalBsmtSF+#Heating+HeatingQC+CentralAir+Electrical+
             X1stFlrSF+#X2ndFlrSF+LowQualFinSF+
             GrLivArea+
             #BsmtFullBath+BsmtHalfBath+
             FullBath+#HalfBath+BedroomAbvGr+KitchenAbvGr+#KitchenQual+
             TotRmsAbvGrd+#Functional+
             Fireplaces+#FireplaceQu+GarageType+
             GarageYrBlt+#GarageFinish+
             GarageCars+GarageArea#+GarageQual+GarageCond+PavedDrive+WoodDeckSF+OpenPorchSF+
             #EnclosedPorch+X3SsnPorch+ScreenPorch+
             #PoolArea+MiscVal+MoSold+YrSold+SaleType+SaleCondition
)

result=c()

for (i in 2:1460) {
  
    result[i-1]=predict(formula,data.frame(#MSSubClass=test_data[i-1,2],#MSZoning=test_data[i-1,3],
    #LotFrontage=test_data[i-1,4],
    #LotArea=test_data[i-1,5]#,Street=test_data[i-1,6],LotShape=test_data[i-1,8],
    #LandContour=test_data[i-1,9],LotConfig=test_data[i-1,11],LandSlope=test_data[i-1,12],
    #Neighborhood=test_data[i-1,13],Condition1=test_data[i-1,14],Condition2=test_data[i-1,15],
    #BldgType=test_data[i-1,16],HouseStyle=test_data[i-1,17],
    OverallQual=test_data[i-1,18],#OverallCond=test_data[i-1,19],
    YearBuilt=test_data[i-1,20],YearRemodAdd=test_data[i-1,21],
    #RoofStyle=test_data[i-1,22],RoofMatl=test_data[i-1,23],Exterior1st=test_data[i-1,24],#Exterior2nd=test_data[i-1,25]#,
    #MasVnrType=test_data[i-1,26],
    MasVnrArea=test_data[i-1,27],
    #ExterQual=test_data[i-1,28],ExterCond=test_data[i-1,29],Foundation=test_data[i-1,30],BsmtQual=test_data[i-1,31],
    #BsmtCond=test_data[i-1,32],BsmtExposure=test_data[i-1,33],BsmtFinType1=test_data[i-1,34],
    #BsmtFinSF1=test_data[i-1,35],
    #BsmtFinType2=test_data[i-1,36],BsmtFinSF2=test_data[i-1,37],
    #BsmtUnfSF=test_data[i-1,38],
    TotalBsmtSF=test_data[i-1,39],
    #Heating=test_data[i-1,40],HeatingQC=test_data[i-1,41],CentralAir=test_data[i-1,42],Electrical=test_data[i-1,43],
    X1stFlrSF=test_data[i-1,44],
    #X2ndFlrSF=test_data[i-1,45],LowQualFinSF=test_data[i-1,46],
    GrLivArea=test_data[i-1,47],
    #BsmtFullBath=test_data[i-1,48],BsmtHalfBath=test_data[i-1,49],
    FullBath=test_data[i-1,50],#HalfBath=test_data[i-1,51],
    #BedroomAbvGr=test_data[i-1,52],KitchenAbvGr=test_data[i-1,53],KitchenQual=test_data[i-1,54],
    TotRmsAbvGrd=test_data[i-1,55],
    #Functional=test_data[i-1,56],
    Fireplaces=test_data[i-1,57],#FireplaceQu=test_data[i-1,58],
    #GarageType=test_data[i-1,59],
    GarageYrBlt=test_data[i-1,60],#GarageFinish=test_data[i-1,61],
    GarageCars=test_data[i-1,62],GarageArea=test_data[i-1,63]#,GarageQual=test_data[i-1,64],GarageCond=test_data[i-1,65],PavedDrive=test_data[i-1,66],
    #WoodDeckSF=test_data[i-1,67],OpenPorchSF=test_data[i-1,68],EnclosedPorch=test_data[i-1,69],
    #X3SsnPorch=test_data[i-1,70],ScreenPorch=test_data[i-1,71],PoolArea=test_data[i-1,72],MiscVal=test_data[i-1,76],
    #MoSold=test_data[i-1,77],YrSold=test_data[i-1,78],SaleType=test_data[i-1,79],SaleCondition=test_data[i-1,80]
)) }

    
data=with(test_data,data.frame(result,OverallQual,YearBuilt,YearRemodAdd,MasVnrArea,TotalBsmtSF,X1stFlrSF,GrLivArea,FullBath,TotRmsAbvGrd,Fireplaces,GarageYrBlt,GarageCars,GarageArea))



#==========================================================================================================================================================================#
submission = with(test_data,data.frame(Id,result))
# change the column name
colnames(submission)[which(names(submission) == "result")] <- "SalePrice"
# remove NA
submission$SalePrice[is.na(submission$SalePrice)] <- mean(submission$SalePrice, na.rm = TRUE) 

# write csv
write.table(submission,file = "submission.csv" ,row.names = FALSE, sep=",")
# result histogram
ggplot(data[!is.na(data$result),]) + geom_histogram(aes(x=result), color = 'red', fill = 'orange') + labs(x = 'Sale Price') + ggtitle("Predicted Sale Price Histogram")
#==========================================================================================================================================================================#



yearbuilt_box=ggplot(data,aes(x=YearBuilt,y=result,group=YearBuilt)) + geom_boxplot() + geom_abline(intercept=-2887670,slope=1556,color="red") + ggtitle("YearBuilt")
yearbuilt_point=ggplot(data,aes(x=YearBuilt,y=result)) + geom_point() + geom_abline(intercept=-2887670,slope=1556,color="red") + ggtitle("YearBuilt")
yearremodadd_box=ggplot(data,aes(x=YearRemodAdd,y=result,group=YearRemodAdd)) + geom_boxplot() + geom_abline(intercept=-3957328,slope=2086,color="red") + ggtitle("YearRemoAdd")
yearremodadd_point=ggplot(data,aes(x=YearRemodAdd,y=result)) + geom_point() + geom_abline(intercept=-3957328,slope=2086,color="red") + ggtitle("YearRemoAdd")
masvnrarea_box=ggplot(data,aes(x=MasVnrArea,y=result,group=MasVnrArea)) + geom_boxplot() + geom_abline(intercept = 157817.1,slope=224.3,color="red") + ggtitle("MasVnrArea")
masvnrarea_point=ggplot(data,aes(x=MasVnrArea,y=result)) + geom_point() + geom_abline(intercept = 157817.1,slope=224.3,color="red") + ggtitle("MasVnrArea")
overallqual_box=ggplot(data,aes(x=OverallQual,y=result,group=OverallQual)) + geom_boxplot() + geom_abline(intercept=-93774,slope=44772,color="red") + ggtitle("OverallQual")
overallqual_point=ggplot(data,aes(x=OverallQual,y=result)) + geom_point() + geom_abline(intercept=-93774,slope=44772,color="red") + ggtitle("OverallQual")
bsmt_box=ggplot(data,aes(x=TotalBsmtSF,y=result,group=TotalBsmtSF)) + geom_boxplot() + geom_abline(intercept=63102.1,slope=111.8,color="red") + ggtitle("TotalBsmtSF")
bsmt_point=ggplot(data,aes(x=TotalBsmtSF,y=result)) + geom_point() + geom_abline(intercept=63102.1,slope=111.8,color="red") + ggtitle("TotalBsmtSF")
x1st_box=ggplot(data,aes(x=X1stFlrSF,y=result,group=X1stFlrSF)) + geom_boxplot() + geom_abline(intercept=39489.2,slope=121.6,color="red") + ggtitle("1stFlrSF")
x1st_point=ggplot(data,aes(x=X1stFlrSF,y=result)) + geom_point() + geom_abline(intercept=39489.2,slope=121.6,color="red") + ggtitle("1stFlrSF")
grlivarea_box=ggplot(data,aes(x=GrLivArea,y=result,group=GrLivArea)) + geom_boxplot() + geom_abline(intercept = 6204.2,slope=117.1,color="red") + ggtitle("GrivArea")
grlivarea_point=ggplot(data,aes(x=GrLivArea,y=result)) + geom_point() + geom_abline(intercept = 6204.2,slope=117.1,color="red") + ggtitle("GrivArea")
bath_box=ggplot(data,aes(x=FullBath,y=result,group=FullBath)) + geom_boxplot() + geom_abline(intercept = 55324,slope=80077,color="red") + ggtitle("FullBath")
bath_point=ggplot(data,aes(x=FullBath,y=result)) + geom_point() + geom_abline(intercept = 55324,slope=80077,color="red") + ggtitle("FullBath")
TotRms_box=ggplot(data,aes(x=TotRmsAbvGrd,y=result,group=TotRmsAbvGrd)) + geom_boxplot() + geom_abline(intercept = 3044,slope=27029,color="red") + ggtitle("TotRmsAbvGrd")
TotRms_point=ggplot(data,aes(x=TotRmsAbvGrd,y=result)) + geom_point() + geom_abline(intercept = 3044,slope=27029,color="red") + ggtitle("TotRmsAbvGrd")
fire_box=ggplot(data,aes(x=Fireplaces,y=result,group=Fireplaces)) + geom_boxplot() + geom_abline(intercept = 147369,slope=56066,color="red") + ggtitle("Fireplaces")
fire_point=ggplot(data,aes(x=Fireplaces,y=result)) + geom_point() + geom_abline(intercept = 147369,slope=56066,color="red") + ggtitle("Fireplaces")
Garageyear_box=ggplot(data,aes(x=GarageYrBlt,y=result,group=GarageYrBlt)) + geom_boxplot() + geom_abline(intercept = -3219224,slope=1720,color="red") + ggtitle("GarageYrBlt")
Garageyear_point=ggplot(data,aes(x=GarageYrBlt,y=result)) + geom_point() + geom_abline(intercept = -3219224,slope=1720,color="red") + ggtitle("GarageYrBlt")
Garagecar_box=ggplot(data,aes(x=GarageCars,y=result,group=GarageCars)) + geom_boxplot() + geom_abline(intercept = 34463,slope=79001,color="red") + ggtitle("GarageCars")
Garagecar_point=ggplot(data,aes(x=GarageCars,y=result)) + geom_point() + geom_abline(intercept = 34463,slope=79001,color="red") + ggtitle("GarageCars")
Garagearea_box=ggplot(data,aes(x=GarageArea,y=result,group=GarageArea)) + geom_boxplot() + geom_abline(intercept = 48464,slope=266.9,color="red") + ggtitle("GarageArea")
Garagearea_point=ggplot(data,aes(x=GarageArea,y=result)) + geom_point() + geom_abline(intercept = 48464,slope=266.9,color="red") + ggtitle("GarageArea")
grid.arrange(yearbuilt_box,yearbuilt_point,nrow=2)
readline(prompt = "Enter a character:")
grid.arrange(yearremodadd_box,yearremodadd_point,nrow=2)
readline(prompt = "Enter a character:")
grid.arrange(masvnrarea_box,masvnrarea_point,nrow=2)
readline(prompt = "Enter a character:")
grid.arrange(masvnrarea_box,masvnrarea_point,nrow=2)
readline(prompt = "Enter a character:")
grid.arrange(overallqual_box,overallqual_point,nrow=2)
readline(prompt = "Enter a character:")
grid.arrange(bsmt_box,bsmt_point,nrow=2)
readline(prompt = "Enter a character:")
grid.arrange(x1st_box,x1st_point,nrow=2)
readline(prompt = "Enter a character:")
grid.arrange(grlivarea_box,grlivarea_point,nrow=2)
readline(prompt = "Enter a character:")
grid.arrange(bath_box,bath_point,nrow=2)
readline(prompt = "Enter a character:")
grid.arrange(TotRms_box,TotRms_point,nrow=2)
readline(prompt = "Enter a character:")
grid.arrange(fire_box,fire_point,nrow=2)
readline(prompt = "Enter a character:")
grid.arrange(Garageyear_box,Garageyear_point,nrow=2)
readline(prompt = "Enter a character:")
grid.arrange(Garagecar_box,Garagecar_point,nrow=2)
readline(prompt = "Enter a character:")
grid.arrange(Garagearea_box,Garagearea_point,nrow=2)
