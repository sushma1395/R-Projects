# Load the data
data("iris")

# 1. PCA Algorithm
# Normalization
a = scale(iris[, 1:4], center = TRUE, scale = FALSE)
# Compute variance-covariance matrix
cov_a = cov(a)
# Spectral Decomposition to solve for Eigen values and Eigen vectors
lambda = eigen(cov_a)$values
load = eigen(cov_a)$vectors # loadings or weight matrix 
# calculate the percentage
lambda = lambda/sum(lambda)
# select first PC at the level of 90%
plot(lambda)

# 2. Apply PCA function directly
pca_iris = princomp(iris[1:4])
summary(pca_iris)
loadings(pca_iris)
plot(pca_iris,type="lines") # scree plot 
pca_iris$scores # principal components 
biplot(pca_iris) # new PC space


