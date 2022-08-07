library(caret)
library(tidyverse)
library(ggplot2)
#a.)
#import dataset
df = read.csv("energy-usage-2010.csv")
head(df)
summary(df)

#b.)
summary(df)

df_nums = df[c(5:17,20:32,64:69)]
library(psych)

pairs.panels(df_nums[c(13,26:32)])

df_nums  = df_nums %>%
  select(TOTAL.KWH, TOTAL.THERMS : OCCUPIED.UNITS) %>%
  mutate(total.kwh_log = log2(TOTAL.KWH)) %>%
  mutate(total.therm_log = log2(TOTAL.THERMS))

ggplot(df_nums, aes(x=total.kwh_log)) + geom_histogram()

table(df['BUILDING_SUBTYPE'])
table(df['BUILDING.TYPE'])

df_bt = df %>%
  group_by(BUILDING.TYPE) %>%
  summarise(n = n()) %>%
  mutate(Freq = n/sum(n))
df_bst = df %>%
  group_by(BUILDING_SUBTYPE) %>%
  summarise(n = n()) %>%
  mutate(Freq = n/sum(n))

ggplot(df, aes(BUILDING.TYPE, fill = BUILDING_SUBTYPE)) + geom_bar(color = "black") +
  theme_classic() +
  xlab("Building Type") +
  labs(fill = "Building Subtype") +
  ggtitle("Building Type & Subtype Breakout") +
  theme(plot.title = element_text(hjust = .1, face = "bold",size = 15))

ggplot(df_nums, aes(x=AVERAGE.STORIES)) + geom_histogram(bins = 30)

#c.)
summary(df)
#15K+ NAs under therms.sqft.standard.deviation.2010
#15K+ NAs under kwh.sqft.standard.deviation.2010

#data without summary statistic calculations
df_cleaned = df %>%
  select(COMMUNITY.AREA.NAME:THERMS.TOTAL.SQFT,TOTAL.POPULATION:OCCUPIED.HOUSING.UNITS)

#replace NAs with median value per building subtype
df_cleaned = df_cleaned %>%
  group_by(BUILDING_SUBTYPE) %>%
  mutate_at(vars(KWH.JANUARY.2010:TOTAL.KWH,THERM.JANUARY.2010:TOTAL.THERMS,
                 KWH.TOTAL.SQFT, THERMS.TOTAL.SQFT),
            ~replace_na(., median(., na.rm = TRUE)))
summary(df_cleaned)
df_cleaned = na.omit(df_cleaned) #still retain 96% of the data

#fix column name
colnames(df_cleaned)[colnames(df_cleaned) == "TERM.APRIL.2010"] = "THERM.APRIL.2010"
summary(df_cleaned)

#changing building subtype/type to factors
df_cleaned$BUILDING.TYPE = as.factor(df_cleaned$BUILDING.TYPE)
df_cleaned$BUILDING_SUBTYPE = as.factor(df_cleaned$BUILDING_SUBTYPE)
df_cleaned$ZERO.KWH.ACCOUNTS = as.numeric(df_cleaned$ZERO.KWH.ACCOUNTS)
df_cleaned$TOTAL.POPULATION = as.numeric(df_cleaned$TOTAL.POPULATION)
df_cleaned$TOTAL.UNITS = as.numeric(df_cleaned$TOTAL.UNITS)
df_cleaned$OCCUPIED.UNITS = as.numeric(df_cleaned$OCCUPIED.UNITS)
df_cleaned$RENTER.OCCUPIED.HOUSING.UNITS = as.numeric(df_cleaned$RENTER.OCCUPIED.HOUSING.UNITS)
df_cleaned$OCCUPIED.HOUSING.UNITS = as.numeric(df_cleaned$OCCUPIED.HOUSING.UNITS)

ggplot(df_cleaned, aes(y=TOTAL.KWH,x=TOTAL.THERMS)) + geom_point() +
  scale_x_continuous(trans = "log2") +
  scale_y_continuous(trans = "log2")


#d.)
#binning heat/electricity accounts
df_cleaned$ELECTRICITY.ACCOUNTS = str_replace(df_cleaned$ELECTRICITY.ACCOUNTS,"Less than 4","3")
df_cleaned$GAS.ACCOUNTS = str_replace(df_cleaned$GAS.ACCOUNTS,"Less than 4","3")

df_cleaned[df_cleaned == ""] = NA
df_cleaned$GAS.ACCOUNTS = as.numeric(df_cleaned$GAS.ACCOUNTS)
df_cleaned$ELECTRICITY.ACCOUNTS = as.numeric(df_cleaned$ELECTRICITY.ACCOUNTS)
df_cleaned = df_cleaned %>%
  group_by(BUILDING_SUBTYPE) %>%
  mutate_at(vars(ELECTRICITY.ACCOUNTS,GAS.ACCOUNTS),
            ~replace_na(., median(., na.rm = TRUE)))

summary(df_cleaned)
df_cleaned = df_cleaned %>%
  mutate(eAccts = cut(ELECTRICITY.ACCOUNTS,
                      breaks = c(-Inf,5,20,Inf),
                      labels = c("0-5","5-20","20+"))) %>%
  mutate(gAccts = cut(GAS.ACCOUNTS,
                      breaks = c(-Inf,5,20,Inf),
                      labels = c("0-5","5-20","20+")))

df_cleaned = df_cleaned %>%
  select(-c(GAS.ACCOUNTS,ELECTRICITY.ACCOUNTS))

#dummy vars
library(fastDummies)
df_cleaned = dummy_cols(df_cleaned,select_columns = "COMMUNITY.AREA.NAME")

df_cleaned = df_cleaned %>%
  select(BUILDING_SUBTYPE:COMMUNITY.AREA.NAME_Woodlawn)
#log transformations
df_cleaned[2:13][df_cleaned[2:13] == 0] = 1
df_cleaned2 = df_cleaned %>%
  mutate_at(vars(KWH.JANUARY.2010:TOTAL.KWH,THERM.JANUARY.2010:TOTAL.THERMS,
                 KWH.TOTAL.SQFT, THERMS.TOTAL.SQFT,
                 OCCUPIED.UNITS), log2)
summary(df_cleaned2)
#center/scale
df_cleaned2 = df_cleaned2 %>%
  dplyr::mutate_if(is.numeric,"scale")

#e.)
#get rid of categoricals
predictors = df_cleaned2 %>%
  select(-c(BUILDING_SUBTYPE,RENTER.OCCUPIED.HOUSING.UNITS,eAccts,gAccts,`COMMUNITY.AREA.NAME_Albany Park`:COMMUNITY.AREA.NAME_Woodlawn))
#set seed
set.seed(13)

preproc = preProcess(predictors, method = c("center","scale"))
predictors = predict(preproc,predictors)

#knee/silhouette method
library(factoextra)
library(stats)
fviz_nbclust(predictors, kmeans, method = "wss") #4 clusters
fviz_nbclust(predictors, kmeans, method = "silhouette") #2 or 4 clusters

fit = kmeans(predictors, centers = 4, nstart = 25)
fviz_cluster(fit, data = predictors)

result = data.frame(SUBTYPE = df_cleaned2$BUILDING_SUBTYPE, fit = fit$cluster)

result %>% group_by(fit) %>% select(fit,SUBTYPE) %>% table() #hard time grouping by clusters

#HAC
library(cluster)
index = createDataPartition(y=df_cleaned2$BUILDING_SUBTYPE, p=.2, list=FALSE)
predictors = df_cleaned2[index,]
predictors = predictors %>%
  select(-c(BUILDING_SUBTYPE,RENTER.OCCUPIED.HOUSING.UNITS,eAccts,gAccts,`COMMUNITY.AREA.NAME_Albany Park`:COMMUNITY.AREA.NAME_Woodlawn))
dist_mat = dist(predictors, method = "euclidean")
hfit = hclust(dist_mat, method = 'median')
fviz_nbclust(predictors, FUN=hcut, method = "wss") #4 or 6
fviz_nbclust(predictors, FUN=hcut, method = "silhouette") #2 or 4

h = cutree(hfit, k=6)
fviz_cluster(list(data = predictors, cluster = h))

result2 = data.frame(SUBTYPE = df_cleaned2[index,]$BUILDING_SUBTYPE, hfit = h)

result2 %>% group_by(hfit) %>% select(hfit,SUBTYPE) %>% table() #hard time grouping by clusters
colnames(df_cleaned2) = make.names(colnames(df_cleaned2))
#f.)
set.seed(14)
index = createDataPartition(y = df_cleaned2$BUILDING_SUBTYPE, p = .7, list = FALSE)
train_set = df_cleaned2[index,]
test_set = df_cleaned2[-index,]

train_control = trainControl(method = "cv", number = 10)
#decision tree
tree1 = train(BUILDING_SUBTYPE ~., data = train_set, method = "rpart1SE",
              trControl = train_control)
library(caret)
library(rpart)
library(rattle)
library(RColorBrewer)
fancyRpartPlot(tree1$finalModel, caption="")
tree1
pred_tree = predict(tree1, test_set)
confusionMatrix(test_set$BUILDING_SUBTYPE, pred_tree)


#knn
df_clean_num = df_cleaned2 %>%
  select(-c(eAccts,gAccts))
index = createDataPartition(y = df_cleaned2$BUILDING_SUBTYPE, p = .2, list = FALSE)
knn_train = df_clean_num[index,]
knn_test = df_clean_num[-index,]

knn1 = train(BUILDING_SUBTYPE ~., data = knn_train, method = "knn",
             trControl = train_control, preProcess = c("center","scale"),
             tuneLength = 15)
knn1
plot(knn1)

pred_knn = predict(knn1, knn_test)
confusionMatrix(knn_test$BUILDING_SUBTYPE, pred_knn)

#g.)
#create binary classifier
df_cleaned3 = df_cleaned2 %>%
  mutate(bST = ifelse(BUILDING_SUBTYPE == "Single Family" | BUILDING_SUBTYPE == "Multi < 7",
                      "Small Building","Large Building"))
df_cleaned3 = df_cleaned3[2:120]

#update decision tree model
index = createDataPartition(y = df_cleaned3$bST, p = .7, list = FALSE)
train_set = df_cleaned3[index,]
test_set = df_cleaned3[-index,]
tree2 = train(bST ~., data = train_set, method = "rpart1SE",
              trControl = train_control)
tree2
fancyRpartPlot(tree2$finalModel, caption="")

pred_tree2 = predict(tree2, test_set)
cm = confusionMatrix(test_set$bST, pred_tree2)
cm
#store metrics in a data frame
metrics = as.data.frame(cm$byClass)
metrics

#precision
metrics[5,]

#recall
metrics[6,]

#ROC Curve
library(pROC)
pred_prob = predict(tree2, test_set, type = "prob")
roc_obj = roc((test_set$bST), pred_prob[,1])
plot(roc_obj, print.auc=TRUE)
