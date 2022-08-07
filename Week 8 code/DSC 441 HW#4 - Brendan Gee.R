#Problem #1
#a.)
#read in both files
df1 = read.csv("winequality-red.csv", sep = ';')
df2 = read.csv("winequality-white.csv", sep=';')
head(df1)
head(df2)

#add type variable to both
df1$type = as.factor('red')
df2$type = as.factor('white')

#change quality to a factor
df1$quality = as.factor(df1$quality)
df2$quality = as.factor(df2$quality)
#merge both tables w/ full join
df = full_join(df1,df2)
head(df)

#b.)
p = prcomp(df[c(-12,-13)], scale = TRUE)
summary(p) #2 PCs only at .47 cumulative var
print(p)
plot(p) #4 components w/ >1 var criteria
df_pca = as.data.frame(p$x)
df_pca$type = df$type

ggplot(df_pca, aes(PC1, PC2, color = type)) +
  geom_point()

#d.)
#kNN
ctrl = trainControl(method="cv", number = 10)
knnFit = train(type ~., data = df,
               method = "knn",
               trControl = ctrl,
               preProcess = c("center","scale"))
knnFit

pred_knn = predict(knnFit, df)
confusionMatrix(df$type, pred_knn)

#SVM
grid = expand.grid(C = 10^seq(-5,2,.5))
svm_grid = train(type ~., data = df, method = "svmLinear",
                 trControl = ctrl, tuneGrid = grid)
svm_grid
pred_svm = predict(svm_grid, df)
confusionMatrix(df$type, pred_svm)

#Decision Tree
tree = train(type ~., data = df, method = "rpart",
             trControl = ctrl)
tree
pred_tree = predict(tree, df)
confusionMatrix(df$type, pred_tree)

#e.)
#Original PCA Plot
ggplot(df_pca, aes(PC1, PC2, color = type)) +
  geom_point()
#KNN Plot
df_pca$KNN = pred_knn
ggplot(df_pca, aes(PC1, PC2, color = KNN)) +
  geom_point()

#SVM Plot
df_pca$SVM = pred_svm
ggplot(df_pca, aes(PC1, PC2, color = SVM)) +
  geom_point()

#Decision Tree Plot
df_pca$Tree = pred_tree
ggplot(df_pca, aes(PC1, PC2, color = Tree)) +
  geom_point()

#Problem #2
#a.)
data("Sacramento")

#convert categorical cols to dummies
install.packages("fastDummies")
library(fastDummies)
Sacramento = dummy_cols(Sacramento,
                        select_columns = "city")
Sacramento = dummy_cols(Sacramento,
                        select_columns = "zip")
Sacramento = Sacramento[c(-1,-2)]

#c.)
library(kknn)
tuneGrid = expand.grid(kmax = 3:10,
                       kernel = c("cos"),
                       distance = 1:3)
kknn_fit <- train(type ~ ., data = Sacramento,
                  method = 'kknn',
                  trControl = ctrl,
                  preProcess = c('center', 'scale'),
                  tuneGrid = tuneGrid)
kknn_fit


#Problem #3
#a.)
head(df)
df_clust = df %>% select(-type)
df_clust$quality = as.integer(df_clust$quality)

#center & scale
preproc = preProcess(df_clust, method=c("center","scale"))
df_clust = predict(preproc,df_clust)

#Knee Method
fviz_nbclust(df_clust, kmeans, method = "wss") #3 or 5

#Silhouette Method
fviz_nbclust(df_clust, kmeans, method = "silhouette") #3 or 4

#Using 3 Clusters
fit = kmeans(df_clust, centers = 3, nstart = 25)
fviz_cluster(fit, data = df_clust)

#b.)
dist_mat = dist(df_clust, method = 'euclidean')
dist_mat2 = dist(df_clust, method = 'manhattan')

#each hfit combination
hfit1 = hclust(dist_mat, method = 'complete')
hfit2 = hclust(dist_mat, method = 'single')
hfit3 = hclust(dist_mat2, method = 'complete')
hfit4 = hclust(dist_mat2, method = 'single')

#knee plot
fviz_nbclust(df_clust, FUN = hcut, method = "wss") #3 or 4

#silhouette plot
fviz_nbclust(df_clust, FUN = hcut, method = "silhouette") #2 or 4

#4 clusters w/ each hfit
h1 <- cutree(hfit1, k=4)
h2 <- cutree(hfit2, k=4)
h3 <- cutree(hfit3, k=4)
h4 <- cutree(hfit4, k=4)

#4 cluster plots
fviz_cluster(list(data = df_clust, cluster = h1))
fviz_cluster(list(data = df_clust, cluster = h2))
fviz_cluster(list(data = df_clust, cluster = h3))
fviz_cluster(list(data = df_clust, cluster = h4))

#c.)
table = data_frame(h1,h2,h3,h4,fit = fit$cluster, type = df$type)
#h1 cross tab
result1 = table %>%
  group_by(h1) %>%
  select(h1, type) %>% table()
#h2 cross tab
result2 = table %>%
  group_by(h2) %>%
  select(h2, type) %>% table()
#h3 cross tab
result3 = table %>%
  group_by(h3) %>%
  select(h3, type) %>% table()
#h4 cross tab
result4 = table %>%
  group_by(h4) %>%
  select(h4, type) %>% table()
#fit cross tab
result5 = table %>%
  group_by(fit) %>%
  select(fit, type) %>% table()

result1
result2
result3
result4
result5

#d.)
p = prcomp(df_clust)
rotated_data = as.data.frame(p$x)
rotated_data$h1 = as.factor(h1)
rotated_data$fit = as.factor(fit$cluster)
rotated_data$type = df$type
ggplot(rotated_data, aes(PC1, PC2, color = h1)) + geom_point(alpha = .3)
ggplot(rotated_data, aes(PC1, PC2, color = fit)) + geom_point(alpha = .3)
ggplot(rotated_data, aes(PC1, PC2, color = type)) + geom_point(alpha = .3)

#Problem #4
rm(list = )
df = starwars
data = starwars
#get rid of unnecessary vars
df = df %>%
  select(-c(name,films,vehicles,starships))
#change chars to factors
df$hair_color = as.factor(df$hair_color)
df$skin_color = as.factor(df$skin_color)
df$eye_color = as.factor(df$eye_color)
df$sex = as.factor(df$sex)
df$gender = as.factor(df$gender)
df$homeworld = as.factor(df$homeworld)
df$species = as.factor(df$species)

#omit NAs
df = na.omit(df)

#center and scale int vars for clustering
preproc = preProcess(df[c(1,2,6)])
df = predict(preproc, df)
#a.)
library(cluster)
dist_mat = daisy(df, metric = "gower")
summary(dist_mat)


fviz_nbclust(df, FUN = hcut, method = "wss") #3 or 4

fviz_nbclust(df, FUN = hcut, method = "silhouette") #2 or 3

hfit = hclust(dist_mat, method = 'average')
h = cutree(hfit, k=3)
#b.)
plot(hfit)

#c.)
#dummies
df = dummy_cols(df,select_columns = "hair_color")
df = dummy_cols(df,select_columns = "skin_color")
df = dummy_cols(df,select_columns = "eye_color")
df = dummy_cols(df,select_columns = "sex")
df = dummy_cols(df,select_columns = "gender")
df = dummy_cols(df,select_columns = "homeworld")
df = dummy_cols(df,select_columns = "species")
summary(df)
df = df %>% 
  select(-c(hair_color,skin_color,eye_color,sex,gender,homeworld,species))
fviz_nbclust(df, kmeans, method = "wss") #4 or 6
fviz_nbclust(df, kmeans, method = "silhouette") #2 or 4

#4 clusters
fit = kmeans(df, centers = 4, nstart=25)

#d.)
df$fit = fit$cluster
df$h = h
result = df %>%
  group_by(h) %>%
  select(h, fit) %>% table()
result
