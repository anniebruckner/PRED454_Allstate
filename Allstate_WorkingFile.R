#PRED 454 Advances Modeling
#Allstate Purchase Prediction Challenge

#Sherman Test Commit via RStudio
# Install Packages if they don't current exist
list.of.packages <- c("doBy"
                      ,"lazyeval"
                      ,"psych"
                      ,"lars"
                      ,"GGally"
                      ,"ggplot2"
                      ,"grid"
                      ,"gridExtra"
                      ,"corrgram"
                      ,"corrplot"
                      ,"leaps"
                      ,"glmnet"
                      ,"MASS"
                      ,"gbm"
                      ,"tree"
                      ,"rpart"
                      ,"rpart.plot"
                      ,"rattle"
                      ,"gam"
                      ,"class"
                      ,"e1071"
                      ,"randomForest"
                      ,"doParallel"
                      ,"iterators"
                      ,"foreach"
                      ,"parallel"
                      ,"lattice"
                      ,"caret"
                      ,"data.table"
                      ,"plyr")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Load all packages
lapply(list.of.packages, require, character.only = TRUE)

#library(lattice)
#library(data.table)
#data.table cheat sheet.
#https://s3.amazonaws.com/assets.datacamp.com/img/blog/data+table+cheat+sheet.pdf


# set to your local directory. We will each have to edit this line of code.
path <- "C:/Users/elfty/Desktop/Sherman/MSPA/P454/Project/" #shermanpath
#path <- "/Users/paulbertucci/Desktop/MSPA/PRED454_AdvancedModeling/FinalProject/AllState #paulpath

#load the train and the test data
train <- read.csv(file.path(path,"train.csv"), stringsAsFactors=TRUE)
test <- read.csv(file.path(path,"test_v2.csv"), stringsAsFactors=TRUE)

attach(train)
par(mfrow=c(3,3))

#freq table for each purchase option
apply(train[18:24],2,FUN = count)

#creating histogram for each purchase option
my_hist<-function(variable)
{
  x <- get(variable)
  h<-hist(x,breaks=seq(from=-.5,to=4.5,by=1),col="red",main=variable)
}
apply(X = array(names(train)[18:24]),MARGIN =1,FUN = my_hist)

# summary statistics
summary(train)
# risk_factor 240418 NAs
# C_previous & duration_previous - 1811 NAs

#Creating planCombo variable for plan total 
train$planCombo<-paste(train$A,train$B,train$C,train$D,train$E,train$F,train$G)

#creating a dataframe of purchased records
train.purchase<-train[which(train$record_type==1),]

#Adding last plan shopped to train.purchase data frame
train.purchase$lastQuotePoint<-train.purchase$shopping_pt-1
lookup<-train[c("customer_ID","shopping_pt","planCombo")]
train.purchase<-merge(x=train.purchase,y=lookup,by.x=c("lastQuotePoint","customer_ID"),by.y=c("shopping_pt","customer_ID"))
#cleaning up train.purchase dataframe
names(train.purchase)[names(train.purchase)=='planCombo.x']<-"planCombo"
names(train.purchase)[names(train.purchase)=='planCombo.y']<-"lastQuotedPlan"
train.purchase<-train.purchase[order(train.purchase$customer_ID),]
#table of who bought the last plan shopped
table(train.purchase$planCombo==train.purchase$lastQuotedPlan)


# of unique Customers in train set - All purchase a plan
length(unique(train$customer_ID))
#97009

# of unique plans shopped in train set
length(unique(train$planCombo))
#1809

# of unique plans purchased in train set
length(unique(train.purchase$planCombo))
#1522

#top 10 plans purchased.
top10plans<-data.frame("count"=(sort(table(train.purchase$planCombo),decreasing=TRUE)[1:10]))

# distribution of shopping point for purchased plans
hist(as.numeric((train.purchase$shopping_pt)),col="blue",breaks=20,main = "Purchase Shopping Point")

#Exploring the components of a plan against variables
# histogram(~ A+B+C +D+E+F+G| car_value, data = train.purchase)
# histogram(~ A+B+C +D+E+F+G| homeowner, data = train.purchase)

#can't get the below function to work, trying to plot hist for each a variable for each purchase option
my_hist2<-function(variable)
{
  x <- get(variable)
  ggplot(train.purchase,aes(x=day))+geom_histogram()+facet_grid(~x)
  #h<-hist(x,breaks=seq(from=-.5,to=4.5,by=1),col="red",main=variable)
}
apply(X = array(names(train.purchase)[18:24]),MARGIN =1,FUN = my_hist2)

#correlation matrix for numeric variables
cormat = cor(train[c(2:4,7:10,12:17)], use="na.or.complete")
cormat_table <- as.data.frame(as.table(cormat))
cormat_table[order(abs(cormat_table$Freq),decreasing = TRUE),]
