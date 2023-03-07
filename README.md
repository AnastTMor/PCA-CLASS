# PCA-CLASS
Classification on NBA Players using PCA analysis

The R code provided is a script for performing principal component analysis (PCA) and k-means clustering on a dataset of basketball player statistics. Here is a breakdown of what each section of the code does:

Install and Load Required Packages:
This section installs and loads the necessary R packages, including stats, dplyr, ggplot2, readxl, and ggfortify.

Read in Data:
This section reads in the basketball player statistics data from an Excel file using the read_excel() function from the readxl package. It then selects specific columns of interest and creates a new column for player ID using the select() and nrow() functions from the dplyr package.

Check PCA Eligibility:
This section calculates the correlation matrix of the selected columns using the cor() function and checks the average correlation amongst variables. If the average correlation is greater than 0.3 or less than -0.3, the variables are eligible for PCA.

Perform PCA:
This section performs PCA using the princomp() function from the stats package and saves the results as an object called PCA.

Evaluate PCA Analysis:
This section examines the loadings (weights) of the principal components using the $loadings attribute of the PCA object. It also calculates the correlation matrix of the principal components to check for independence.

Perform K-Means Clustering:
This section performs k-means clustering on the principal components using the kmeans() function from the stats package. It then plots the results using the ggplot2 package.

Extract Cluster Information:
This section extracts information on the clusters, including which players are in each cluster and which cluster a specific player belongs to.

Perform PCA and K-Means Clustering with Additional Players:
This section repeats the PCA and k-means clustering steps using additional basketball player statistics data. It then merges this data with the original data using the merge() function and plots the results using ggplot2. It also extracts information on the clusters for the additional players.



