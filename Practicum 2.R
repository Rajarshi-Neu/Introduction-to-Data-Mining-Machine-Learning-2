adult_data <- read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data",header = FALSE,stringsAsFactors = FALSE,
                       col.names=c("Age","Workclass","Fnlwgt","Education","Educationnum","MaritalStatus","Occupation","Relationship","Race","Sex","Capital-Gain","CapitalLoss","HoursPerWeek","NativeCountry", "Sal")) #Specifying column name based on description of data given in data set explanation.

adult_data[1:10,]

anyNA(adult_data)
#No NA values
#Lets go via taking a look at the table
#We see that data has ? in place of values which are unknown
#We will need to impute those values. Anyhoww, lets convert them to NA for now
adult_data[adult_data == " ?"] <- NA
colnames(adult_data)[colSums(is.na(adult_data)) > 0]
#We see that three columns contain NA values, which are categorical.
#Now we shall implement knn based on other features to impute the missing values.
#new_adult_DF_missing <- adult_data[rowSums(is.na(adult_data)) > 0,]
#str(new_adult_DF_missing)

original_adult_data <- adult_data


mode <- function(x) {
  uniqv <- unique(x)
  uniqv[which.max(tabulate(match(x, uniqv)))]
}


x <- adult_data$Workclass
wc.m <- mode(x)
x[is.na(x)] <- wc.m
wc.m
anyNA(x)


y <- adult_data$Occupation
occ.m <- mode(y)
y[is.na(y)] <- occ.m
occ.m
anyNA(y)


z <- adult_data$NativeCountry
nc.m <- mode(z)
z[is.na(z)] <- nc.m
nc.m
anyNA(z)


adult_data$Workclass[which(is.na(adult_data$Workclass))] <- wc.m
adult_data$Occupation[which(is.na(adult_data$Occupation))] <- occ.m
adult_data$NativeCountry[which(is.na(adult_data$NativeCountry))] <- nc.m

anyNA(adult_data)



library(psych)
pairs.panels(adult_data[c(1,3, 5, 11,12,13)])

#Frequency table
#install.packages('plyr')
library(plyr)
f1 <- count(adult_data, 'Workclass')
f2 <- count(adult_data, 'Education')
f3 <- count(adult_data, 'MaritalStatus')
f4 <- count(adult_data, 'Occupation')
f5 <- count(adult_data, 'Relationship')
f6 <- count(adult_data, 'Race')
f7 <- count(adult_data, 'Sex')
f8 <- count(adult_data, 'NativeCountry')


tol <- function(x)
{
  tem <- numeric(nrow(x))

  mold(x, tem)
}


mold <- function(x, tem)
{
  dof <- x
  j <- nrow(x)
  print(j)
  for (i in 1:j)
  {
    print(x$freq[i])
    print(sum(x$freq))
    tem[i] <- x$freq[i]/sum(x$freq)
  }
  dof$likelilihood <- tem
  print(dof)
}

tol(f1)
tol(f2)
tol(f3)
tol(f4)
tol(f5)
tol(f6)
tol(f7)
tol(f8)


l1 <- table(adult_data$Workclass, adult_data$Sal)
l2 <- table(adult_data$Education, adult_data$Sal)
l3 <- table(adult_data$MaritalStatus, adult_data$Sal)
l4 <- table(adult_data$Occupation, adult_data$Sal)
l5 <- table(adult_data$Relationship, adult_data$Sal)
l6 <- table(adult_data$Race, adult_data$Sal)
l7 <- table(adult_data$Sex, adult_data$Sal)
l8 <- table(adult_data$NativeCountry, adult_data$Sal)

summary(l1)

h1 <- l1

pol <- function(x)
{
  tem <- numeric(nrow(x))
  mold2(x, tem)
}

mold2 <- function(x, tem)
{
  dof <- x
  j <- nrow(x)
  for (i in 1:j)
  {
    tem[i] <- x[i,2]/sum(x[i,1],x[i,2])
  }
  dof <- cbind(dof, tem)
  colnames(dof)[3] <- c("Likelihood")
  print(dof)
}


pol(l1)


k1 <- unclass(h1)
summary(k1)

h1 <- k1

pol(h1)




#Q3:

jol <- function(x)
{
  tem <- numeric(nrow(x))
  lem <- numeric(nrow(x))
  kold2(x, tem, lem)
}

kold2 <- function(x, tem, lem)
{
  dof <- x
  j <- nrow(x)
  for (i in 1:j)
  {
    tem[i] <- x[i,2]/sum(x[i,1],x[i,2])
    lem[i] <- x[i,1]/sum(x[i,1],x[i,2])
  }
  dof <- cbind(dof, tem, lem)
  colnames(dof)[3] <- c("Likelihood_>50K")
  colnames(dof)[4] <- c("Likelihood_<50K")
  print(dof)
}




k1 <- unclass(l1)
k2 <- unclass(l2)
k3 <- unclass(l3)
k4 <- unclass(l4)
k5 <- unclass(l5)
k6 <- unclass(l6)
k7 <- unclass(l7)
k8 <- unclass(l8)

h1 <- k1

pol(h1)


Workclass1 <- jol(k1)
Education1 <- jol(k2)
MaritalStatus1 <- jol(k3)
Occupation1 <- jol(k4)
Relationship1 <- jol(k5)
Race1 <- jol(k6)
Sex1 <- jol(k7)
NativeCountry1 <- jol(k8)

#-------------------------------------
  

#Now first we need to calculate the probability of whether the instance is >50K or <50K

FreqOf50KTable <- count(adult_data, 'Sal')

ProbSal50KorMore <- FreqOf50KTable[1,2]/sum(FreqOf50KTable[1,2], FreqOf50KTable[2,2])
ProbSal50KorLess <- FreqOf50KTable[2,2]/sum(FreqOf50KTable[1,2], FreqOf50KTable[2,2])
ProbSal50KorMore #0.7591904
ProbSal50KorLess #0.2408096


Prob50KM_White <- as.numeric(Race1[5,3])
Prob50KM_Male <- as.numeric(Sex1[2,3])
Prob50KM_Fed <- as.numeric(Workclass1[1,3])
Prob50KM_Edu <- as.numeric(Education1[10,3])
Prob50KM_Native <- as.numeric(NativeCountry1[21,3])


Prob50KL_White <- as.numeric(Race1[5,4])
Prob50KL_Male <- as.numeric(Sex1[2,4])
Prob50KL_Fed <- as.numeric(Workclass1[1,4])
Prob50KL_Edu <- as.numeric(Education1[10,4])
Prob50KL_Native <- as.numeric(NativeCountry1[21,4])

Prob50KM <- ProbSal50KorMore*Prob50KM_White*Prob50KM_Male*Prob50KM_Fed*Prob50KM_Edu*Prob50KM_Native
Prob50KL <- ProbSal50KorLess*Prob50KL_White*Prob50KL_Male*Prob50KL_Fed*Prob50KL_Edu*Prob50KL_Native

Prob50M <- Prob50KM/sum(Prob50KM, Prob50KL)

Prob50L <- Prob50KL/sum(Prob50KM, Prob50KL)

#######Question5:
#first we will only the feature data
kfoldadultdata <- adult_data[,c(2,4,9,10, 14, 15)]
#Divide the data into 10 datasets
d1 <- kfoldadultdata[1:3256,]
d2 <- kfoldadultdata[3257:6512,]
d3 <- kfoldadultdata[6513:9768,]
d4 <- kfoldadultdata[9769:13024,]
d5 <- kfoldadultdata[13025:16280,]
d6 <- kfoldadultdata[16281:19536,]
d7 <- kfoldadultdata[19537:22792,]
d8 <- kfoldadultdata[22793:26048,]
d9 <- kfoldadultdata[26049:29304,]
d10 <- kfoldadultdata[29305:32561,]

#Likelihood tables for d1:
jol2 <- function(x)
{
  tem <- numeric(nrow(x))
  lem <- numeric(nrow(x))
  kold22(x, tem, lem)
}

kold22 <- function(x, tem, lem)
{
  dof <- x
  j <- nrow(x)
  for (i in 1:j)
  {
    tem[i] <- x[i,2]/sum(x[i,1],x[i,2])
    lem[i] <- x[i,1]/sum(x[i,1],x[i,2])
  }
  dof <- cbind(dof, tem, lem)
  colnames(dof)[3] <- c("Likelihood_>50K")
  colnames(dof)[4] <- c("Likelihood_<50K")
}


pikachu <- function(x)
{
 lt1 <- unclass(table(x$Workclass, x$Sal))
 ae1 <- jol2(lt1)
}

pikachu2 <- function(x)
{
  lt2 <- unclass(table(x$Education, x$Sal))
  ae2 <- jol2(lt2)
}

pikachu3 <- function(x)
{
 lt3 <- unclass(table(x$Race, x$Sal))
 ae3 <- jol2(lt3)
}

pikachu4 <- function(x)
{
  lt4 <- unclass(table(x$Sex, x$Sal))
  ae4 <- jol2(lt4)
}

pikachu5 <- function(x)
{
  lt5 <- unclass(table(x$NativeCountry, x$Sal))
  ae5 <- jol2(lt5)
}

Probability_check <- function(x)
{
  
 WC1 <- pikachu(x)
 ED1 <- pikachu2(x)
 RA1 <- pikachu3(x)
 SE1 <- pikachu4(x)
 NC1 <- pikachu5(x)

 T50kx <- count(x, 'Sal')
 P50M <- T50kx[1,2]/sum(T50kx[1,2], T50kx[2,2])
 P50L <- T50kx[2,2]/sum(T50kx[1,2], T50kx[2,2])

 Prob50KM_White <- as.numeric(RA1[5,3])
 Prob50KM_Male <- as.numeric(SE1[2,3])
 Prob50KM_Fed <- as.numeric(WC1[1,3])
 Prob50KM_Edu <- as.numeric(ED1[10,3])
 Prob50KM_Native <- as.numeric(NC1[21,3])


 Prob50KL_White <- as.numeric(RA1[5,4])
 Prob50KL_Male <- as.numeric(SE1[2,4])
 Prob50KL_Fed <- as.numeric(WC1[1,4])
 Prob50KL_Edu <- as.numeric(ED1[10,4])
 Prob50KL_Native <- as.numeric(NC1[21,4])

 Prob50KM <- P50M*Prob50KM_White*Prob50KM_Male*Prob50KM_Fed*Prob50KM_Edu*Prob50KM_Native
 Prob50KL <- P50L*Prob50KL_White*Prob50KL_Male*Prob50KL_Fed*Prob50KL_Edu*Prob50KL_Native

 Prob50M <- Prob50KM/sum(Prob50KM, Prob50KL)
 print("Probability that the person has > 50K is:")
 print(Prob50M)
 Prob50L <- Prob50KL/sum(Prob50KM, Prob50KL)
 print("Probability that the person has < 50K is:")
 print(Prob50L)
}


Probability_check(d1)
Probability_check(d2)
Probability_check(d3)
Probability_check(d4)
Probability_check(d5)
Probability_check(d6)
Probability_check(d7)
Probability_check(d8)
Probability_check(d9)
Probability_check(d10)





#Sources:
#https://www.r-bloggers.com/how-to-get-the-frequency-table-of-a-categorical-variable-as-a-data-frame-in-r/
