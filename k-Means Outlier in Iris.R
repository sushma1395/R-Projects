# Run clustering for two clusters 
library(datasets)
head(iris)

library(ggplot2)
ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) + geom_point()

iris2 <- iris[, c(3,4)]
kmeans.result <- kmeans(iris2, centers=2) 


# Get the centers of clusters
centers <- kmeans.result$centers[kmeans.result$cluster, ] 

# Calculate distances between objects and cluster centers 
distances <- sqrt(rowSums((iris2 - centers)^2))

# Pick top 5 largest distances 
outliers <- order(distances, decreasing=T)[1:6] 

# Print the outliers 
print(outliers) 

print(iris2[outliers,]) 
ggplot(iris2, aes(Petal.Length, Petal.Width, color = kmeans.result$cluster)) +
  geom_point() +
  geom_point(data=mydata[outliers, ], aes(Petal.Length, Petal.Width), colour="red", size=3)
