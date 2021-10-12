# input 2 csv into 2 data frame first.
train_df <- read.csv("C:/Users/user/Desktop/online class/y2s2/data analytics/project/train.csv")
test_df <- read.csv("C:/Users/user/Desktop/online class/y2s2/data analytics/project/test.csv")


# Histogram for the sale price.
ggplot(train_df[!is.na(train_df$SalePrice),]) + geom_histogram(aes(x=SalePrice), color = 'red', fill = 'orange') + labs(x = 'Sale Price') + ggtitle("Sale Price Histogram")


# HeatMap for the sale price and other factors.
# (1)Here I used corrplot to do the heat map, reference:
#    https://stackoverflow.com/questions/15887212/heatmap-or-plot-for-a-correlation-matrix
#    https://www.youtube.com/watch?v=jxUiIFj2l-s
#    install.packages("corrplot")
# (2)Here I used select_if to select all numeric columns, reference:
#    https://stackoverflow.com/questions/5863097/selecting-only-numeric-columns-from-a-data-frame
#    install.packages("dplyr")
# (3)Here I also used na.omit to get the complete data set, reference:
#    https://www.youtube.com/watch?v=q8eR2suCyGk
All_nums_data <- select_if(train_df, is.numeric)
correlations <- cor(na.omit(All_nums_data))
corrplot(correlations, method = "ellipse")


# BoxPlot for non numeric factors
# (1)MSZoning & SalePrice
ggplot(train_df) + geom_boxplot(aes(x = MSZoning, y = SalePrice, fill = MSZoning)) + labs(x = 'MSZoning') + ggtitle("MSZoning & SalePrice")

# (2)BldgType & SalePrice
ggplot(train_df) + geom_boxplot(aes(x = BldgType, y = SalePrice, fill = BldgType)) + labs(x = 'BldgType') + ggtitle("BldgType & SalePrice")

# (3)Neighborhood & SalePrice
ggplot(train_df) + geom_boxplot(aes(x = Neighborhood, y = SalePrice, fill = Neighborhood)) + labs(x = 'Neighborhood') + ggtitle("Neighborhood & SalePrice")

# (4)GarageType & SalePrice
ggplot(train_df) + geom_boxplot(aes(x = GarageType, y = SalePrice, fill = GarageType)) + labs(x = 'GarageType') + ggtitle("GarageType & SalePrice")

# Reference for plots:
# https://towardsdatascience.com/generalized-pairs-plot-in-r-6bbfde2c98b8