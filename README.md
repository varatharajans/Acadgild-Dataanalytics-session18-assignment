# Acadgild-Dataanalytics-session18-assignment
DATA ANALYTICS WITH R, EXCEL AND TABLEAU SESSION 18 ASSIGNMENT 

Session 18 Segmentation Analysis
Weight Lifting Excercise




5. Problem Statement 
1. Use the below given data set 
DataSet 
2. Perform the below given activities: 
a. Create classification model using different decision trees. 
b. Verify model goodness of fit. 
c. Apply all the model validation techniques. 
d. Make conclusions


setwd("C:/Users/Seshan/Desktop/sv R related/acadgild/assignments/session17")
library(readr)
Weightlift <- read.csv("C:/Users/Seshan/Desktop/sv R related/acadgild/assignments/session 18 Assign/Weight lift.csv",sep=',',header=F)
#problem: find out natual grouping 
df<-Weightlift 
#df=Weightlift <- read.csv("C:/Users/Seshan/Desktop/sv R related/acadgild/assignments/session 18 Assign/Weightlift.csv",sep=',',header=F)
View(df)
#df = read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/00273/Example_WearableComputing_weight_lifting_exercises_biceps_curl_variations.csv",sep=',',header=F)
#problem: find out natual grouping 
#View(df)
head(df)

str(df)

set.seed(1234)
ind = sample(1:nrow(df),0.8*nrow(df),replace = F)
df_train =df[ind,-1]
df_test = df[-ind,-1]

summary(df)
dim(df)
apply(df,2,range)
apply(df,2,summary)
df[] <- lapply(df, function(x) as.numeric(as.character(x)))
# KMeans - comes from Rcmdr library
# Kmeans- from amap library
# kmeans- from stats library
# steps in k-means clustering
#1- preprocessing the data (impute missing values, remove outliers, feature trasnformation)
#2- scaling or standardization of data set
#3- decide the number of clusters (value of K)
#4- iterate over the samples to create clusters
#5- decide the distance measure
#6- calculate the group accuracy
# scaling of data
df_train1 <- scale(df_train)
head(df_train1)
class(df_train1)

# screeplot approach to decide the number of clusters
km = kmeans(df_train1,1)
km$withinss
km$tot.withinss

km = kmeans(df_train1,2)
km$withinss
km$tot.withinss

km = kmeans(df_train1,3)
km$withinss
km$tot.withinss

km = kmeans(df_train1,4)
km$withinss
km$tot.withinss

km = kmeans(df_train1,5)
km$withinss
km$tot.withinss

km = kmeans(df_train1,6)
km$withinss
km$tot.withinss

km = kmeans(df_train1,7)
km$withinss
km$tot.withinss

km = kmeans(df_train1,8)
km$withinss
km$tot.withinss

km = kmeans(df_train1,9)
km$withinss
km$tot.withinss

km = kmeans(df_train1,10)
km$withinss
km$tot.withinss

dev.off()
sumsq=NULL
for (i in 1:10)
  sumsq[i] = sum(kmeans(df_train,centers=i,
                        iter.max = 1000,
                        nstart=i,
                        algorithm='Forgy')$withinss)
plot(1:10,sumsq,type='b', main='Screeplot showing within group sum of squares')

km = kmeans(df_train1,3)
km$withinss
km$tot.withinss

class(km$cluster)

summary(km)

km$centers

as.numeric(km$cluster)

length(km$cluster)

dim(df_train)

class(df_train)

df_train$cl <- km$cluster

head(df_train)
# profiles of clusters
aggregate(df_train[,1:8],list(df_train[,9]),mean)

table(df$V1)

library(cluster)

clusplot(df_train,df_train$cl,cex=0.9,color=T,shade=T, labels=4,lines=0)

#HC clustering or Hierarchical Clustering
# distance (euclidean, manhattan, cosine distance)

# Divisive method (top down)
# Agglomorative method (bottom up)



df_train = df_train[,-9]
head(df_train)

# compute the distance metrix
d1 <- dist(df_train,method='euclidean')
summary(d1)


# HC
fit <- hclust(d1,method = 'ward.D2')
plot(fit)

# single, double, average, ward, ward.D2

# agglomorative method
fit <- agnes(d1,metric='euclidean',method = 'ward')
plot(fit)

# divisive method
fit <- diana(d1,metric='euclidean')
plot(fit)
library(ggdendro)
if(require(cluster)){
  fit<- agnes(d1, metric = "manhattan", stand = TRUE)
  dg <- as.dendrogram(fit)
  ggdendrogram(dg)
  
  fit <- diana(d1, metric = "manhattan", stand = TRUE)
  dg <- as.dendrogram(fit)
  ggdendrogram(dg)
}





A Scree Plot is a simple line segment plot that shows the fraction of total variance in the data as explained or represented by each PC. The PCs are ordered, and by definition are therefore assigned a number label, by decreasing order of contribution to total variance.






Conclusion:-
The term cluster analysis  encompasses a number of different algorithms and methods for grouping objects of similar kind into respective categories.  In other words cluster analysis is an exploratory data analysis tool which aims at sorting different objects into groups in a way that the degree of association between two objects is maximal if they belong to the same group and minimal otherwise. Given the above, cluster analysis can be used to discover structures in data without providing an explanation/interpretation. In other words, cluster analysis simply discovers structures in data without explaining why they exist.
k-Means Clustering

In general, the k-means method will produce exactly k different clusters of greatest possible distinction. It should be mentioned that the best number of clusters k leading to the greatest separation (distance) is not known as a priori and must be computed from the data
INTERPRETATION OF RESULTS
Usually, as the result of a k-means clustering analysis, we would examine the means for each cluster on each dimension to assess how distinct our k clusters are. Ideally, we would obtain very different means for most, if not all dimensions, used in the analysis. The magnitude of the F values from the analysis of variance performed on each dimension is another indication of how well the respective dimension discriminates between clusters.
Euclidean distance. This is probably the most commonly chosen type of distance. It simply is the geometric distance in the multidimensional space. 






A dendrogram or tree diagram allows to illustrate the hierarchical organisation of several entities. For example, we often use it to make family trees. It is constituted of a root node, which give birth to several nodes that end by giving leaf nodes (the
bottom of the tree). Dendrogram can be made with 2 types of dataset. i/ a numeric matrixwhere several variables describe the features of individuals. We can then calculate the distance between individuals and cluster them. ii/ A hierarchical

gg dendroplot given below provides a good output. As the data is large it needs lot of cleaning and removal of missing data


