# ECON-5821-Final

This is the repo I collect the codes for the final project of data_science.

In model 2 I tested 18 models. They're the combination of different parameters:
PCA dimensions=8,9,10
Whether use CPI as variable
Lag=1,2,3

In model 3 I use all the data to train the global best lambda of Lasso Regression. The results are not as good as finding lambda separately.

In model 4 I use x_t-2 and y_t-1 to predict yt, which means Lag=2 for x and Lag=1 for CPI as an independent variable in Lasso Regression.
It seems model 4 has the best performance for CPI.

