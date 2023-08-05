dataset <- read.csv("D:/11th Semester/Data Science/Mid Project/titanic.csv")
dataset

#Find the shape of the data set
nrow(dataset)

ncol(dataset)

length(dataset)

#Show the attributes names of the data set
names(dataset)

#Find the types of data for all attributes
str(dataset)
#View the structure of the data set
str(dataset)

#The first few rows of data set
head(dataset)

#Summary of the data set
summary(dataset)

#Find the type of attribute
sapply(dataset, class)

#Measure of spread range and standard deviation
#For Gender
gender_range <- range(dataset$gender, na.rm = TRUE)
print(gender_range)
gender_sd <- sd(dataset$gender, na.rm = TRUE)
print(gender_sd)

#For Age
age_range <- range(dataset$age, na.rm = TRUE)
print(age_range)
age_sd <- sd(dataset$age, na.rm = TRUE)
print(age_sd)

#For sibsp
sibsp_range <- range(dataset$sibsp, na.rm = TRUE)
print(sibsp_range)
sibsp_sd <- sd(dataset$sibsp, na.rm = TRUE)
print(sibsp_sd)

#For parch
parch_range <- range(dataset$parch, na.rm = TRUE)
print(parch_range)
parch_sd <- sd(dataset$parch, na.rm = TRUE)
print(parch_sd)

#For fare
fare_range <- range(dataset$fare, na.rm = TRUE)
print(fare_range)
fare_sd <- sd(dataset$fare, na.rm = TRUE)
print(fare_sd)

#For survived
survived_range <- range(dataset$survived, na.rm = TRUE)
print(survived_range)
survived_sd <- sd(dataset$fsurvived, na.rm = TRUE)
print(survived_sd)


#Find the missing value
number_of_missing_value=colSums(is.na(dataset))
number_of_missing_value

dataset[!complete.cases(dataset),]


#Missing Value
missing_gender=which(is.na(dataset$gender))
missing_gender
missing_age=which(is.na(dataset$age))
missing_age

#Most Frequent Value
find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}
most_frequent_gender=find_mode(dataset$gender)
most_frequent_gender

#Replacing missing value by most frequent value for gender attributes
dataset$gender[is.na(dataset$gender)]<-most_frequent_gender
print(dataset)


#Data Cleaning
#Deleting row for clean data
remove_missing<-na.omit(dataset)
print(remove_missing)

#.................................Using mean for data cleaning.......................................

dataset <- read.csv("D:/11th Semester/Data Science/Mid Project/titanic.csv")
dataset

#Find the missing value
number_of_missing_value=colSums(is.na(dataset))
number_of_missing_value

dataset[!complete.cases(dataset),]


#Missing Value
missing_gender=which(is.na(dataset$gender))
missing_gender
missing_age=which(is.na(dataset$age))
missing_age

#Most Frequent Value
find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}
most_frequent_gender=find_mode(dataset$gender)
most_frequent_gender

#Replacing missing value by most frequent value for gender attributes
dataset$gender[is.na(dataset$gender)]<-most_frequent_gender
print(dataset)


#..........................................Using mean................................
age_mean=mean(dataset$age,na.rm=T)
recover_missing_age_mean = dataset$age[is.na(dataset$age)]<-age_mean
recover_missing_age_mean
print(dataset)

#..........................................Using mode for data cleaning................................

dataset <- read.csv("D:/11th Semester/Data Science/Mid Project/titanic.csv")
dataset

#Find the missing value
number_of_missing_value=colSums(is.na(dataset))
number_of_missing_value

dataset[!complete.cases(dataset),]


#Missing Value
missing_gender=which(is.na(dataset$gender))
missing_gender
missing_age=which(is.na(dataset$age))
missing_age

#Most Frequent Value
find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}
most_frequent_gender=find_mode(dataset$gender)
most_frequent_gender

#Replacing missing value by most frequent value for gender attributes
dataset$gender[is.na(dataset$gender)]<-most_frequent_gender
print(dataset)

#..........................................Using Mode................................

age_mode=find_mode(dataset$age)
recover_missing_age_mode = dataset$age[is.na(dataset$age)]<-age_mode
recover_missing_age_mode
print(dataset)



#..................................................Annotate....................................
dataset <- read.csv("D:/11th Semester/Data Science/Mid Project/titanic.csv")
dataset

#For embarked
dataset$embarked<-factor(dataset$embarked,levels=c("S","C","Q"),labels=c(1,2,3))
print(dataset$embarked)
print(dataset)

#For class
dataset$class<-factor(dataset$class,levels=c("First","Second","Third"),labels=c(11,22,33))
print(dataset$class)
print(dataset)

#For who
dataset$who<-factor(dataset$who,levels=c("man","woman","child"),labels=c(44,55,66))
print(dataset$who)
print(dataset)


#For alone
dataset$alone<-factor(dataset$alone,levels=c("FALSE","FALL","TRUE"),labels=c(0,5,1))
print(dataset$alone)
print(dataset)



#....................................outlier for age..........................................
dataset <- read.csv("D:/11th Semester/Data Science/Mid Project/titanic.csv")
dataset

sort(dataset$age)

dataset_outlier=subset(dataset,age<=19)
dataset_outlier


dataset_outlier_location=which(dataset$age<19)
dataset_outlier_location

dataset$age[dataset_outlier_location]<-NA
print(dataset)


#....................................outlier for fare ..........................................
dataset <- read.csv("D:/11th Semester/Data Science/Mid Project/titanic.csv")
dataset

sort(dataset$fare)

dataset_outlier=subset(dataset,fare<=8.034)
dataset_outlier

dataset_outlier_location=which(dataset$fare<8.034)
dataset_outlier_location


dataset$fare[dataset_outlier_location]<-NA
print(dataset)


#.................................Data Transformation.............................
dataset <- read.csv("D:/11th Semester/Data Science/Mid Project/titanic.csv")
dataset

min_max_normalization<-function(x){(x-min(x))/(max(x)-min(x))}
dataset<-as.data.frame(lapply(dataset[1:5],min_max_normalization))
dataset

#........................................Visualization.....................................
#For age
dataset <- read.csv("D:/11th Semester/Data Science/Mid Project/titanic.csv")
dataset

mean_val <- 33.33
sd_val <- 45.7735
age_range <- c(0.83, 455)

age_data <- runif(1000, min = age_range[1], max = age_range[2])
hist(age_data,
     main = "Histogram of Age",
     xlab = "Age", ylab = "Frequency",
     col = "blue", border = "white")

#Sibsp
mean_val <- 0.656
sd_val <- 1.305558
sibsp_range <- c(0, 8)
sibsp_data <- runif(1000, min = sibsp_range[1], max = sibsp_range[2])

hist(sibsp_data,
     main = "Histogram of sibsp",
     xlab = "sibsp", ylab = "Frequency",
     col = "blue", border = "white")

#Parch
mean_val <- 0.392
sd_val <- 0.8252637
parch_range <- c(0, 5)
parch_data <- runif(1000, min = parch_range[1], max = parch_range[2])

hist(age_data,
     main = "Histogram of parch",
     xlab = "parch", ylab = "Frequency",
     col = "blue", border = "white")
#fare
mean_val <- 26.588
sd_val <- 34.82165
fare_range <- c(0, 263)
fare_data <- runif(1000, min =fare_range[1], max = fare_range[2])

hist(age_data,
     main = "Histogram of fare",
     xlab = "fare", ylab = "Frequency",
     col = "blue", border = "white")
#survived
mean_val <- 0.344
sd_val <- NA
survived_range <- c(0, 1)
survived_data <- runif(1000, min =survived_range[1], max = survived_range[2])

hist(age_data,
     main = "Histogram of survived",
     xlab = "survived", ylab = "Frequency",
     col = "blue", border = "white")





#............................Invalid Value........................

dataset <- read.csv("D:/11th Semester/Data Science/Mid Project/titanic.csv")
dataset


#For who
find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}
most_frequent_who=find_mode(dataset$who)
most_frequent_who

dataset$who[16]<-most_frequent_who
print(dataset)