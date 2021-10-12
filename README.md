# Kaggle_House-Price_Advanced-Regression-Techniques
Kaggle GettingStarted Competition, House Price Predicting Using R

This academic project is a house price prediction competition on Kaggle.
In this project, we used R language with R studio.

My contribution to this project (Responsible Parts) included:
- Data Cleaning
- Histograms
- Heat Map
- Box Plots

Our Idea:
(2) After finding correlations between factors, use heat map to find factors which are having stronger correlation with "SalePrice". 
(3) Then using those factors to do linear regression.
(4) Finally, generate a csv file of the predicting result.
(4) Data cleaning, replace all N/A value in the csv file using mean value.

Limitations:
(1) Non-numeric data have been ignored (Box Plots showed that there are strong correlations between some ignored non-numeric factors and "SalePrice").
(2) The data cleaning process, which is replacing N/A with mean value, may be improper.
    Considered method:
    - Removing rows with N/A result (not allowed in Kaggle).
    - Replacing N/A result with 0 (much worse than using mean value).
    
Final Kaggle Public Score (Root mean squared logarithmic error): 0.62059
