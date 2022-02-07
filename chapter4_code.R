#### R Code for chapter 4
# 1. Summary Statistics
# 2. Correlation
# 3. Using the table() and aggregate() functions
# 4. Principal component analysis

#### 1. Smmary Statistics

boston.housing.df <- read.csv("BostonHousing.csv", header = TRUE) 
head(boston.housing.df, 9)
summary(boston.housing.df) 

# compute mean, standard dev., min, max, median, length, and missing values of CRIM
mean(boston.housing.df$CRIM) 
sd(boston.housing.df$CRIM)
min(boston.housing.df$CRIM)
max(boston.housing.df$CRIM)
median(boston.housing.df$CRIM) 
length(boston.housing.df$CRIM) 

# find the number of missing values of variable CRIM
sum(is.na(boston.housing.df$CRIM)) 

# compute mean, standard dev., min, max, median, length, and missing values for all
# variables
data.frame(mean=sapply(boston.housing.df, mean), 
           sd=sapply(boston.housing.df, sd), 
           min=sapply(boston.housing.df, min), 
           max=sapply(boston.housing.df, max), 
           median=sapply(boston.housing.df, median), 
           length=sapply(boston.housing.df, length),
           miss.val=sapply(boston.housing.df, function(x) 
             sum(length(which(is.na(x))))))

#### 2. Correlations between all variables

round(cor(boston.housing.df),2)

#### 3. Using pivot tables and aggregates

# Using a table with one variable
table(boston.housing.df$CHAS)

# Using the aggregate() function

# first you have to create bins of size 1 for the RM variable
boston.housing.df$RM.bin <- .bincode(boston.housing.df$RM, c(1:9))

# compute the average of MEDV by (binned) RM and CHAS
# in aggregate() use the argument by= to define the list of aggregating variables, 
# and FUN= as an aggregating function.
aggregate(boston.housing.df$MEDV, by=list(RM=boston.housing.df$RM.bin, 
                                          CHAS=boston.housing.df$CHAS), FUN=mean) 

#### Principal Components Analysis (PCA)
#### Using the prcomp() function

cereals.df <- read.csv("Cereals.csv") 
# compute PCs on two dimensions
pcs <- prcomp(data.frame(cereals.df$calories, cereals.df$rating)) 
summary(pcs) 

# Compute PCs on all dimensions... omit the first three columns

pcs <- prcomp(na.omit(cereals.df[,-c(1:3)])) 
summary(pcs)

# Compute PCs on all dimensions... omit the first three columns
# this time we standardize all the values.

pcs.cor <- prcomp(na.omit(cereals.df[,-c(1:3)]), scale. = T)
summary(pcs.cor)









