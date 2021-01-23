# Nonprofit-classification
# This project is one of my work for my professor.
# Due to the private issue of the data. I cannot upload it on GitHub and
# the codes are uploaded after having permission from my professor.
# If you are interested in the data, please contact me. I will direct you
# to my professor.


# The task of this project is to build a or some model(s) that can
# predict whether a non-profit organization is important or not
# based on their mission statements and existed data (as the training set)

# In this project, I use different models to do the work, including
# Lasso Regression, Ridge Regression, Neural Network and Deep Learning.
# (There is also the Elastic Net, but I did not put the code here)

# Overall, all models predict correctly around 75%, but the main problem is that
# the Specificity is often too low (around 60 ~ 65%). In other words, the models
# predicts greatly with non-important organzation, but not that good while predicting
# important organzation.

# Another problem is that there are often overfitted the data. This happens perhaps
# because the data has around 5000 observations while variables (words after stemming
# removing) are roughly 9000. 
