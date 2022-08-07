library(ggplot2)
install.packages("astsa")
library(astsa)

head(gdp)
df = as.data.frame(gdp)
df$quarter = seq(1947.25, 2018.75, by = .25)
head(df)
plt = ggplot(df,aes(x=quarter,y=x))
#add a line layer
plt + geom_line() +
  xlab("Time") +
  ylab("GDP") +
  ggtitle("GDP Time Series")

#bar chart
ggplot(diamonds, aes(x=clarity)) + geom_bar() #categorical version of histogram

p = ggplot(diamonds, aes(x=clarity, fill=cut))
p + geom_bar(position="stack")
#bar chart with y specificiation
p = ggplot(diamonds, aes(x=clarity, y = price, fill=cut))
p + geom_col()

#Histogram
head(mtcars)
ggplot(mtcars,aes(x=hp)) + geom_histogram(binwidth = 50)

p = ggplot(data = mtcars, aes(x=hp)) +
  geom_histogram(binwidth=30)+
  facet_wrap(~cyl)
p

#Boxplot
ggplot(mtcars, aes(x=as.factor(cyl),y=mpg)) + #as factor treats it as a categorical
  geom_boxplot() +
  xlab("cyl")

#Scatter plots
ggplot(mtcars, aes(mpg,hp)) + geom_point()

#with linear regression line
ggplot(mtcars, aes(mpg,hp)) + 
  geom_point() + 
  geom_smooth(method = lm)

#for overlapping points, make points transparent
ggplot(mtcars, aes(mpg,hp)) + 
  geom_point(alpha = .5) + 
  geom_smooth(method = lm)

#Contingency plots - multiple categoricals
ggplot(mtcars, aes(x=cyl, y=gear, fill=wt)) + geom_tile()

##QQ plot
ggplot(mtcars, aes(sample=hp)) +
  stat_qq() +
  stat_qq_line(col="red")

#Integration - Joining and Finding Errors
##Missing values
install.packages("hflights")
library(hflights)
data(hflights)
flights = hflights
summary(flights$DepTime)
sum(is.na(flights$DepTime))

##Joins
head(authors)
head(books)

authors %>% inner_join(books,by="name")
authors %>% left_join(books, by="name")

#Handling Missing Data
install.packages("dplyr")
library(dplyr)
summary(flights$DepTime)
clean_flights <- flights %>% drop_na(DepTime)
summary(clean_flights$DepTime)

#Mean replacement
install.packages("tidyverse")
library(tidyverse)
summary(flights$TaxiIn)
clean_flights$TaxiIn = clean_flights$TaxiIn %>%
  replace_na(mean(clean_flights$TaxiIn, na.rm=TRUE))
summary(clean_flights$TaxiIn)

#Nonsensical Values / Placeholders
grades = sample(1:100, 90, replace = TRUE)
grades[91:100] = 99999
grades = data.frame("ID" = 1001:1100, "Grades" = grades)
summary(grades$Grades)
nrow(grades)
clean_grades = grades %>% filter(Grades <= 100)
#after removal
nrow(clean_grades)

#Sampling
set.seed(19)
mtcars_indexed = mtcars %>% mutate(id=row_number())
#create training set
train = mtcars_indexed %>% sample_frac(.75)
#create test set
test = anti_join(mtcars_indexed, train, by='id')
nrow(mtcars)
nrow(test)

#Normalization
install.packages("caret")
library(caret)

preproc1 = preProcess(mtcars, method=c("center","scale"))
norm1 = predict(preproc1, mtcars)
summary(norm1)

#min-max scaler
preproc2 = preProcess(mtcars, method=c("range"))
norm2 = predict(preproc2,mtcars)
summary(norm2)

#Binning
mycars = mtcars
mycars %>%
  mutate(hpfactor = cut(hp,breaks=c(-Inf,120,200,Inf),labels=c("low","medium","high"))) %>%
  head()

#Smoothing
mycars = mtcars %>%
  mutate(hpfactor = cut(hp,breaks=3, labels=c("low","medium","high"))) %>%
  head()
head(mycars)

low = mycars %>% filter(hpfactor == 'low') %>%
  mutate(hp = mean(hp,na.rm=TRUE))
medium = mycars %>% filter(hpfactor == 'medium') %>%
  mutate(hp = mean(hp,na.rm = TRUE))
high = mycars %>% filter(hpfactor == 'high') %>%
  mutate(hp = mean(hp,na.rm = TRUE))
bind_rows(list(low,medium,high))

#Feature Extraction
head(storms)
ggplot(storms,aes(category)) + geom_bar()
storm = storms %>% select(-c("name"))
storm = na.omit(storm)
head(storm)
dummy = dummyVars(category ~ ., data = storm)
dummies = as.data.frame(predict(dummy, newdata = storm))
head(dummies)

nzv = nearZeroVar(dummies)
length(nzv)

storm.pca = prcomp(dummies)
summary(storm.pca)
screeplot(storm.pca, type="1") + title(xlab = "PCs")

target = storm %>% dplyr:: select(category)
preProc = preProcess(dummies, method="pca", pcaComp=2)
storm.pc = predict(preProc, dummies)
storm.pc$category = storm$category
head(storm.pc)

#SVM classifier
library(e1071)
storm_dummies = dummies
storm_dummies$category = storm$category
train_control = trainControl(method = "cv", number = 5)
svm_storm = train(category ~ ., data = storm_dummies, method = "svmLinear", 
                  trControl = train_control)
svm_storm

svm_storm2 = train(category ~., data = storm.pc, method = "svmLinear",
                   trControl = train_control)
svm_storm2
