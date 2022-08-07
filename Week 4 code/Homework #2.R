#Problem #1
#a.)
library(ggplot2)
df = read.csv("BankData.csv")
summary(df)
plot(df[c(2:4,7,9:10,12:13)])
ggplot(df,aes(x = factor(approval))) +
  geom_bar()

ggplot(df,aes(x = ages)) + 
  geom_histogram(binwidth = 10)

#b.)
##z-score normalization
m = mean(df$ages)
m
s = sd(df$ages)
s
df = df %>%
  rowwise() %>%
  mutate(zAge = (ages - m) / s)
##min-max normalization range 0 - 10
cont6min = min(df$cont6)
cont6min
cont6max = max(df$cont6)
cont6max
df = df %>%
  rowwise() %>%
  mutate(mmCont6 = (cont6 - cont6min) / (cont6max - cont6min) * (10 - 0) + 0)
##decimal scaling normalization
max(df$cont5,na.rm = TRUE)
d = .0001
df = df %>%
  rowwise() %>%
  mutate(dCont5 = cont5 * d)

#c.)
ggplot(df, aes(x = zAge)) +
  geom_histogram(bins = 25)

ggplot(df, aes(x = mmCont6)) +
  geom_histogram(bins = 25)

ggplot(df, aes(x = cont6)) +
  geom_histogram(bins = 25)

ggplot(df, aes(x = dCont5)) +
  geom_histogram(bins = 15)

#d.)

#binning
df = df %>%
  mutate(age_bins = cut(ages,breaks=c(0,20,40,60,80,100),
                           labels=c("Teenager","Young Adult","Adult","Senior","Elderly")))
head(df$age_bins)

#e.)
##smoothing
mean(df[df$age_bins == 'Senior','ages'], na.rm = TRUE)
df %>%
  group_by(age_bins) %>%
  summarise(avg = mean())
teen = df %>%
  filter(age_bins == "Teenager") %>%
  mutate(ages = 18.3)
ya = df %>%
  filter(age_bins == "Young Adult") %>%
  mutate(ages = 31.7)
a = df %>%
  filter(age_bins == "Adult") %>%
  mutate(ages = 48.9)
s = df %>%
  filter(age_bins == "Senior") %>%
  mutate(ages = mean(ages, na.rm = 66.2))
e = df %>%
  filter(age_bins == "Elderly") %>%
  mutate(ages = mean(ages, na.rm = 84))
df = bind_rows(list(teen, ya, a, s, e))

#Problem #2
#a.)
df = read.csv("BankData.csv")
summary(df)
df_num = df[c(2:4,7,9:11,12:13)]
df_num = na.omit(df_num)
summary(df_num)
library(caret)
library(e1071)
svm1 = train(approval ~ ., data =df_num,method = "svmLinear")
svm1
train_control_cv = trainControl(method = "cv", number = 10)
svm2 = train(approval ~., data=df_num, method = "svmLinear",
             trControl = train_control_cv)
svm2

#b.)
grid = expand.grid(C = 10^seq(-5,2,.5))
svm_grid = train(approval ~., data = df_num, method = "svmLinear",
                 trControl = train_control_cv, tuneGrid = grid)
svm_grid

#Problem #3
df = starwars
df = df[-c(1,12:14)]
summary(df)
df = na.omit(df)
head(df)

#a.)
#hair dummy variables
df = df %>%
  mutate(blackHair = ifelse(hair_color == 'black', 1,0)) %>%
  mutate(blondHair = ifelse(hair_color == 'blond',1,0)) %>%
  mutate(noneHair = ifelse(hair_color == 'none',1,0)) %>%
  mutate(brownHair = ifelse(hair_color == 'brown',1,0)) %>%
  mutate(whiteHair = ifelse(hair_color == 'white',1,0)) %>%
  mutate(auburn.whiteHair = ifelse(hair_color == 'auburn, white',1,0)) %>%
  mutate(greyHair = ifelse(hair_color == 'grey',1,0))
#skin color dummy variables
df = df %>%
  mutate(fairSkin = ifelse(skin_color == 'fair',1,0)) %>%
  mutate(whiteSkin = ifelse(skin_color == 'white',1,0)) %>%
  mutate(lightSkin = ifelse(skin_color == 'light',1,0)) %>%
  mutate(paleSkin = ifelse(skin_color == 'pale',1,0)) %>%
  mutate(greenSkin = ifelse(skin_color == 'green',1,0)) %>%
  mutate(redSkin = ifelse(skin_color == 'red',1,0)) %>%
  mutate(darkSkin = ifelse(skin_color == 'dark',1,0)) %>%
  mutate(brownmottleSkin = ifelse(skin_color == 'brown mottle',1,0)) %>%
  mutate(brownSkin = ifelse(skin_color == 'brown',1,0)) %>%
  mutate(blueSkin = ifelse(skin_color == 'blue',1,0)) %>%
  mutate(yellowSkin = ifelse(skin_color == 'yellow',1,0)) %>%
  mutate(tanSkin = ifelse(skin_color == 'tan',1,0)) %>%
  mutate(orangeSkin = ifelse(skin_color == 'orange',1,0))
#eye color dummy variables
df = df %>%
  mutate(blackEyes = ifelse(eye_color == 'black',1,0)) %>%
  mutate(blueEyes = ifelse(eye_color == 'blue',1,0)) %>%
  mutate(bluegrayEyes = ifelse(eye_color == 'blue-gray',1,0)) %>%
  mutate(yellowEyes = ifelse(eye_color == 'yellow',1,0)) %>%
  mutate(hazelEyes = ifelse(eye_color == 'hazel',1,0)) %>%
  mutate(orangeEyes = ifelse(eye_color == 'orange',1,0)) %>%
  mutate(redEyes = ifelse(eye_color == 'red',1,0))
#planet dummy variable
df = df %>%
  mutate(Alderaan = ifelse(homeworld == 'Alderaan',1,0)) %>%
  mutate(Bespin = ifelse(homeworld == 'Bespin',1,0)) %>%
  mutate(Cerea = ifelse(homeworld == 'Cerea',1,0)) %>%
  mutate(ConcordDawn = ifelse(homeworld == 'Concord Dawn',1,0)) %>%
  mutate(Corellia = ifelse(homeworld == 'Corellia',1,0)) %>%
  mutate(Dathomir = ifelse(homeworld == 'Dathomir',1,0)) %>%
  mutate(Dorin = ifelse(homeworld == 'Dorin',1,0)) %>%
  mutate(Endor = ifelse(homeworld == 'Endor',1,0)) %>%
  mutate(HaruunKal = ifelse(homeworld == 'Haruun Kal',1,0)) %>%
  mutate(Kamino = ifelse(homeworld == 'Kamino',1,0)) %>%
  mutate(Kashyyyk = ifelse(homeworld == 'Kashyyyk',1,0)) %>%
  mutate(Mirial = ifelse(homeworld == 'Mirial',1,0)) %>%
  mutate(MonCala = ifelse(homeworld == 'Mon Cala',1,0)) %>%
  mutate(Naboo = ifelse(homeworld == 'Naboo',1,0)) %>%
  mutate(Ryloth = ifelse(homeworld == 'Ryloth',1,0)) %>%
  mutate(Serenno = ifelse(homeworld == 'Serenno',1,0)) %>%
  mutate(Socorro = ifelse(homeworld == 'Socorro',1,0)) %>%
  mutate(Stewjon = ifelse(homeworld == 'Stewjon',1,0)) %>%
  mutate(Tatooine = ifelse(homeworld == 'Tatooine',1,0))
#dummy variable species
df = df %>%
  mutate(Cerean = ifelse(species == 'Cerean',1,0)) %>%
  mutate(Ewok = ifelse(species == 'Ewok',1,0)) %>%
  mutate(Cerean = ifelse(species == 'Cerean',1,0)) %>%
  mutate(Gungan = ifelse(species == 'Gungan',1,0)) %>%
  mutate(Human = ifelse(species == 'Human',1,0)) %>%
  mutate(KelDor = ifelse(species == 'Kel Dor',1,0)) %>%
  mutate(Mirialan = ifelse(species == 'Mirialan',1,0)) %>%
  mutate(MonCalamari = ifelse(species == 'Mon Calamari',1,0)) %>%
  mutate(Trandoshan = ifelse(species == 'Trandoshan',1,0)) %>%
  mutate(Wookiee = ifelse(species == 'Wookiee',1,0)) %>%
  mutate(Zabrak = ifelse(species == 'Zabrak',1,0))
df = df[c(1:2,6,8,11:66)]
head(df)

#b.)
svm1 = train(gender ~ ., data =df,method = "svmLinear")
svm1

#c.)
df2 = df[-c(4)]
p = prcomp(df2, scale = T)
summary(p)
print(p)
#scree plot
plot(p)
abline(1,0, col="red")

#d.)
df3 = as.data.frame(p$x)
df3 = df3[1:20]
df3$gender = df$gender
#divide data into train/test
s = sample(nrow(df3), nrow(df3) * .70)
starTrain = df3[s, ]
starTest = df3[-s, ]
svm2 = train(gender ~ ., data=starTrain,method="svmLinear")
predStar = predict(svm2,starTest)
starTest$gender = as.factor(starTest$gender)
confusionMatrix(starTest$gender, predStar)

#bootstrap
boot = trainControl(method = "boot", number = 100)
svm3 = train(gender ~ ., data = df3, method = "svmLinear",
             trControl = boot)
svm3

#10 fold cv
train_control_cv = trainControl(method = "cv", number = 10)
svm6 = train(gender ~., data=df3, method = "svmLinear",
             trControl = train_control_cv)
svm6
#gridsearch
svm4 = train(gender ~., data = df3, method = "svmLinear",
             trControl = boot, tuneGrid = grid)
svm4

svm5 = train(gender ~., data = df3, method = "svmLinear",
             trControl = train_control_cv, tuneGrid = grid)
svm5
