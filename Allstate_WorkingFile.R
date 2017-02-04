#PRED 454 Advanced Modeling
#Allstate Purchase Prediction Challenge

# Install Packages if they don't currently exist
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
                      ,"plyr"
                      ,"maps"
                      ,"reshape2")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Load all packages
lapply(list.of.packages, require, character.only = TRUE)

#data.table cheat sheet.
#https://s3.amazonaws.com/assets.datacamp.com/img/blog/data+table+cheat+sheet.pdf

#####################################
## LOADING DATA ##
#####################################

# set to your local directory. We will each have to edit this line of code.
#path <- "C:/Users/elfty/Desktop/Sherman/MSPA/P454/Project/" #shermanpath
#path <- "/Users/paulbertucci/Desktop/MSPA/PRED454_AdvancedModeling/FinalProject/AllState" #paulpath
path <- "/Users/annie/Desktop/Northwestern/PREDICT_454/Allstate" #anniepath
setwd("/Users/annie/Desktop/Northwestern/PREDICT_454/Allstate")

#load the train and the test data
train <- read.csv(file.path(path,"train.csv"), stringsAsFactors=TRUE)
test <- read.csv(file.path(path,"test_v2.csv"), stringsAsFactors=TRUE)

# Explore the training data -- how big is it, what types of variables included, distributions and missing values.
class(train) # data.frame
dim(train) # 665249     25
names(train)
str(train) # all integer except time, state, and car_value are factor
summary(train) # some missing data 

# Check for NAs
colSums(is.na(train))[colSums(is.na(train)) > 0]

# variables with missing data:
# risk_factor   C_previous    duration_previous 
# 240418        18711         18711

# setting variable types, please feel free to change if you think this is incorrect. #PB
# Shouldn't record_type, state, group_size, homeowner, risk_factor, married_couple, C_previous be factors too? #Annie
## state was read as a factor already because it is a string #PB
## added (record_type,homeowner,married_couple,C_previous) as factors.#PB

## I question whether group_size or risk_factor should be factors. #PB
## risk_factor should be a Factor, If we want to keep group_size as Factor then we should consider age_oldest and age_youngest as Factors as well.. #AM 

# Why did we make location and car_value factors? # Annie
## location is a location_ID , I am unsure how to handle this one. Please feel free to edit as neccessary. #PB
## car_value is a alpha character ("a","b","c",..:), I will keep this as a factor. #PB
## should we convert car_value, state, into numbers? #SC

# setting variable types, please feel free to change if you think this is incorrect. #PB
names <- c('day','location','car_value','A','B','C','D','E','F','G','record_type','homeowner','married_couple','C_previous')
train[,names] <- lapply(train[,names] , factor)
test[,names] <- lapply(test[,names] , factor)
str(train)
summary(train)
#Creating planCombo variable for plan total #PB
train$planCombo<-paste(train$A,train$B,train$C,train$D,train$E,train$F,train$G,sep="")

#creating a dataframe of purchased records #PB
train.purchase<-train[which(train$record_type==1),]
head(train.purchase)

#####################################
## EDA ##
#####################################

#freq table for each purchase option #PB
apply(train[18:24],2,FUN = table)

#####Begin Data Description - DT 1/22/17
#Graphing policy shoppers by state

#load state codes csv file
state_codes <- read.csv(file.choose())
head(state_codes)

state_df <- as.data.frame(table(state_codes$state)) #turn state data into DF for manipulating purposes -- AB: this didn't work --  object 'state' not found. (issue from removing attach()?)
state_df$postal_code <- state_df$state #rename column for easy merging below -- AB: only Var1 and Freq exist in state_df
colnames(state_df) <- c("state","Freq") # AB: changed Var1 to state
head(state_df)

#merge state data frame with frequency counts and postal codes to get state names
#state_combo <- merge(state_codes, state_df, by="postal_code", all=TRUE) # AB: postal code doesn't exist in state_df
state_combo <- merge(state_codes, state_df, by="state", all=TRUE) # AB: added because postal_code doesn't exist in both dfs
head(state_combo) # AB: This essentialy added a Freq column to state_codes

#merge state_combo with state plotting data
names(state_combo)[names(state_combo)=="state.x"] <- "region"
state_combo$region<-tolower(state_combo$region) #done for merging on region to work
# AB: I got this error when I ran the above line of cod:
# Error in `$<-.data.frame`(`*tmp*`, "region", value = character(0)) : 
# replacement has 0 rows, data has 59
state_total <- merge(all_states,state_combo, by="region", all=TRUE)
#head(state_total)

#construct map graph
state_total <- state_total[order(state_total$order),]
p <- ggplot()
p <- p + geom_polygon(data=state_total, aes(x=long, y=lat, group = group, fill=state_total$Freq),colour="grey70"
) + scale_fill_continuous(low = "aliceblue", high = "darkblue", na.value="ivory",guide="colorbar")
P1 <- p + theme_bw()  + labs(fill = "Number of Records" 
                             ,title = "Policy Shoppers by State", x="", y="")
P1 + scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank())

###
#Graphical Distributions of Variables
# Disable scientific notation for labeling plots
options(scipen = 10)
par.default <- par() #save in case needed later
par(mfrow=c(1,2)) #fit more graphs
#barplots for predictor variables # AB: Added train$ since none worked after we removed attach()
par(mfrow=c(3,3)); ylab.box <- "Number of Records"; col.bar = "dodgerblue4" 
barplot(table(train$car_value),main="Frequency by Car Values (New)",ylab=ylab.box,xlab="Car Value Categories",col=col.bar)
barplot(table(train$day),main="Frequency of Site Visits by Day",ylab=ylab.box,xlab="Day (0=Monday,...,6=Sunday)",col=col.bar)
barplot(table(train$C_previous),main="Frequency by Policy Option C",ylab=ylab.box,xlab="Policy Option C Choices (0=nothing)",col=col.bar)
barplot(table(train$record_type),main="Frequency of Record Types",ylab=ylab.box,xlab="0 = Shopping Point, 1 = Purchase Point",col=col.bar)
barplot(table(train$homeowner),main="Frequency by Homeowner", ylab=ylab.box,xlab="0 = Not Homeowner, 1 = Homeowner",col=col.bar)
barplot(table(train$married_couple),main="Frequency by Marital Status",ylab=ylab.box,xlab="0 = Married, 1 = Not Married",col=col.bar)
barplot(table(train$risk_factor),main="Frequency by Risk Factor",ylab=ylab.box,xlab="Risk Factor Levels",col=col.bar)
barplot(table(train$shopping_pt),main="Frequency by Shopping Point",ylab=ylab.box,xlab="Shopping Point Depth",col=col.bar)
barplot(table(train$time),main="Frequency of Site Visits by Time of Day",ylab=ylab.box,xlab="Time (HH:MM)",col=col.bar,border=NA)

#histograms for predictor variables
col.hist = "dodgerblue4"
par(mfrow=c(1,5))
hist(train$car_age,xlab="Age of Car (Years)", main="Frequency by Age of Car",col=col.hist)
hist(train$age_oldest,xlab="Age of Oldest Person in Group", main="Frequency by Oldest Age",col=col.hist)
hist(train$age_youngest,xlab="Age of Youngest Person in Group", main="Frequency by Youngest Age",col=col.hist)
hist(train$cost,xlab="Cost (Dollars)", main="Frequency by Cost of Coverage Options",col=col.hist)
hist(train$duration_previous,xlab="Duration (Years)", main="Previous Insurance Issuer Duration",col=col.hist)

#barplots for response variables 
xlab.policy = "Policy Options"; par(mfrow=c(1,4))

barplot(table(train$A),main="Frequency for Option A",ylab=ylab.box,xlab=xlab.policy,col=col.bar)
barplot(table(train$B),main="Frequency for Option B",ylab=ylab.box,xlab=xlab.policy,col=col.bar)
barplot(table(train$C),main="Frequency for Option C",ylab=ylab.box,xlab=xlab.policy,col=col.bar)
barplot(table(train$D),main="Frequency for Option D",ylab=ylab.box,xlab=xlab.policy,col=col.bar)
par(mfrow=c(1,3))
barplot(table(train$E),main="Frequency for Option E",ylab=ylab.box,xlab=xlab.policy,col=col.bar)
barplot(table(train$F),main="Frequency for Option F",ylab=ylab.box,xlab=xlab.policy,col=col.bar)
barplot(table(train$G),main="Frequency for Option G",ylab=ylab.box,xlab=xlab.policy,col=col.bar)


#look at 5 number summaries
lapply(train, summary)

#consider outliers in select variables
col.box = "dodgerblue4"
bxp.shopping_pt <- boxplot(train$shopping_pt,main="Box Plot of Shopping Points", ylab="Shopping Point", col=col.box)
bxp.car_age <- boxplot(train$car_age,main="Box Plot of Car Age", ylab="Car Age (Years)", col=col.box)
bxp.cost <- boxplot(train$cost,main="Box Plot of Policy Option Cost", ylab="Cost (Dollars)", col=col.box)
# Reset plot display
par(mfrow=c(1,1))

#####End Data Description - DT 1/22/17


# summary statistics
summary(train)
# risk_factor 240418 NAs
# C_previous & duration_previous - 18711 NAs

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
top10plans

# distribution of shopping point for purchased plans
hist(as.numeric((train.purchase$shopping_pt)),col="dodgerblue4",breaks=20,xlab = "Purchase Shopping Point",
     main = "Purchase Shopping Point - Training Set")
maxShoppingPoint.test<-aggregate(test$shopping_pt, by = list(test$customer_ID), max)
#head(maxShoppingPoint.test)
hist(as.numeric((maxShoppingPoint.test$x)),col="dodgerblue4",breaks=20,xlab = "Max Shopping Point",
     main = "Max Shopping Point - Test Set")


#Exploring the components of a plan against variables
mysettings <- trellis.par.get()
mysettings$strip.background$col <- c("lightgrey", "darkgrey")
trellis.par.set(mysettings)
histogram(~ C | car_value, data = train.purchase,
          col = "dodgerblue4", main = "Insurance Plan C v. Car Value", xlab = "Insurance Plan C")
histogram(~ A+B+C+D+E+F+G | car_value, data = train.purchase,
          col = "dodgerblue4", main = "Insurance Plan Options v. Car Value", xlab = "Insurance Options A, B, C, D, E, F, and G")

# histogram(~ A+B+C +D+E+F+G| homeowner, data = train.purchase)

#Frequency of policy Option by state
A_Freq<-prop.table(table(train.purchase$state,train.purchase$A),1)
B_Freq<-prop.table(table(train.purchase$state,train.purchase$B),1)
C_Freq<-prop.table(table(train.purchase$state,train.purchase$C),1)
D_Freq<-prop.table(table(train.purchase$state,train.purchase$D),1)
E_Freq<-prop.table(table(train.purchase$state,train.purchase$E),1)
F_Freq<-prop.table(table(train.purchase$state,train.purchase$F),1)
G_Freq<-prop.table(table(train.purchase$state,train.purchase$G),1)
#Stack bar graph for interesting relationships
plot(A_Freq,col=c("blue","red","yellow","green"))

plot(F_Freq,col=c("blue","red","yellow","green"))
plot(G_Freq,col=c("blue","red","yellow","green"))


#can't get the below function to work, trying to plot hist for each variable for each purchase option
my_hist2<-function(variable)
{
  #x <- get(variable)
  ggplot(train.purchase,aes(x=day))+geom_bar()+facet_grid(~variable)
  #h<-hist(x,breaks=seq(from=-.5,to=4.5,by=1),col="red",main=variable)
}
apply(X = array(names(train.purchase)[18:24]),MARGIN =1,FUN = my_hist2)

#find numeric columns #SC
nums <- sapply(train.purchase, is.numeric)
train.purchase[ , nums]
str(train.purchase)
ggplot(train.purchase,aes(x=day)) + theme_bw() + facet_grid(~A) + geom_bar(color =I("black"), fill = I("dodgerblue4")) + ggtitle("Insurance Option A") + theme(plot.title = element_text(hjust = 0.5))

#graph of predictor vs response
ggplot(train.purchase,aes(x=day))+geom_bar()+facet_grid(~A)
ggplot(train.purchase,aes(x=day))+geom_bar()+facet_grid(paste("~","A"))

forLoopGraph <- function(x) {
  #print(x)
  for (i in 1:7) {
    #print(myFunction2(train.purchase, names(train.purchase)[17+i], x))
    #df = melt(cast(train.purchase, paste(names(train.purchase)[17+i],x, sep = "~"), pctTot))
    ggplot(train.purchase,aes(x=paste(x)))+geom_bar()+facet_grid(paste("~",names(train.purchase)[17+1]))
    #df$col1 = names(df)[1]
    #df$col2 = names(df)[3]
    #names(df)[1] = "cat1"
    #names(df)[3] = "cat2"
    #t = rbind(t,df)
  }
  #return(t)
}

#histtable of each predictor for each response #SC
pctTot <- function(x) { 
  length(x) / nrow(train.purchase) * 100
}

#install.packages("reshape2")
#library(reshape2)
#sessionInfo()

forLoopFunc <- function(x) {
  print(x)
  for (i in 1:7) {
    #print(myFunction2(train.purchase, names(train.purchase)[17+i], x))
    df = melt(cast(train.purchase, paste(names(train.purchase)[17+i],x, sep = "~"), pctTot))
    df$col1 = names(df)[1]
    df$col2 = names(df)[3]
    names(df)[1] = "cat1"
    names(df)[3] = "cat2"
    t = rbind(t,df)
  }
  return(t)
}

df = apply(X=array(names(train.purchase)[c(4,8:17)]), MARGIN = 1, FUN = forLoopFunc)
# could not find function "cast" -- AB: though reshape2 is installed, we must use acast or dcast per ?cast
# Use ‘acast’ or ‘dcast’ depending on whether you want vector/matrix/array output or data frame output.
# AB: Neither acast nor dcast works for me.

##uniquechar
train.uniquechar = unique(train[c("customer_ID","state", "group_size","homeowner","car_age","car_value","risk_factor","age_oldest",
                                  "age_youngest","married_couple","C_previous","duration_previous")])

#add numeric factors in place of categorical for correlation analysis #SC
train_cp = train

state_factor = as.factor(train[,c("state")])
state_ranks <- rank(-table(state_factor), ties.method="first")
train_cp$state_num <- data.frame(category=state_factor, rank=state_ranks[as.character(state_factor)])$rank

car_value_factor = as.factor(train[,c("car_value")])
car_value_ranks <- rank(-table(car_value_factor), ties.method="first")
train_cp$car_value_num <- data.frame(category=car_value_factor, rank=car_value_ranks[as.character(car_value_factor)])$rank

#correlation matrix for numeric variables #SC
cormat = cor(train_cp[c(2:4,7:10,12:17,27:28)], use="na.or.complete")
cormat_table <- as.data.frame(as.table(cormat))
cormat_table <- cormat_table[order(abs(cormat_table$Freq),decreasing = TRUE),]
write.csv(cormat_table, "cormat_table.csv")

#PCA #SC
train.pca <- prcomp(na.omit(train_cp[c(2:4,7:10,12:17,27:28)]),center = TRUE,scale. = TRUE)
print(train.pca)
summary(train.pca)
plot(train.pca, type="l")

write.csv(train.pca$rotation,"pca.csv")

#####################################
## Data manipulation for Model Build ## 
#####################################
#PB

train.purchase.m<-train.purchase

#Adding previous quorted plans to train.purchase.m data frame #PB
train.purchase.m$purchaseMinus_1<-train.purchase.m$shopping_pt-1
train.purchase.m$purchaseMinus_2<-train.purchase.m$shopping_pt-2
train.purchase.m$purchaseMinus_3<-train.purchase.m$shopping_pt-3
train.purchase.m$purchaseMinus_4<-train.purchase.m$shopping_pt-4

#purchase minus 1 quote
lookup<-train[c("customer_ID","shopping_pt","planCombo")]
train.purchase.m<-merge(x=train.purchase.m,y=lookup,by.x=c("purchaseMinus_1","customer_ID"),by.y=c("shopping_pt","customer_ID"))
#cleaning up train.purchase.m dataframe
names(train.purchase.m)[names(train.purchase.m)=='planCombo.x']<-"planCombo"
names(train.purchase.m)[names(train.purchase.m)=='planCombo.y']<-"lastQuotedPlan"

#purchase minus 2 quote
train.purchase.m<-merge(x=train.purchase.m,y=lookup,by.x=c("purchaseMinus_2","customer_ID"),by.y=c("shopping_pt","customer_ID"),all.x=TRUE)
#cleaning up train.purchase.m dataframe
names(train.purchase.m)[names(train.purchase.m)=='planCombo.x']<-"planCombo"
names(train.purchase.m)[names(train.purchase.m)=='planCombo.y']<-"QuoteMinus_2"

#purchase minus 3 quote
train.purchase.m<-merge(x=train.purchase.m,y=lookup,by.x=c("purchaseMinus_3","customer_ID"),by.y=c("shopping_pt","customer_ID"),all.x=TRUE)
#cleaning up train.purchase.m dataframe
names(train.purchase.m)[names(train.purchase.m)=='planCombo.x']<-"planCombo"
names(train.purchase.m)[names(train.purchase.m)=='planCombo.y']<-"QuoteMinus_3"

#purchase minus 4 quote
train.purchase.m<-merge(x=train.purchase.m,y=lookup,by.x=c("purchaseMinus_4","customer_ID"),by.y=c("shopping_pt","customer_ID"),all.x=TRUE)
#cleaning up train.purchase.m dataframe
names(train.purchase.m)[names(train.purchase.m)=='planCombo.x']<-"planCombo"
names(train.purchase.m)[names(train.purchase.m)=='planCombo.y']<-"QuoteMinus_4"

#Cleaning up the train.purchsase data frame
train.purchase.m$QuoteMinus_3[is.na(train.purchase.m$QuoteMinus_3)]<-"XXXXXXX"
train.purchase.m$QuoteMinus_4[is.na(train.purchase.m$QuoteMinus_4)]<-"XXXXXXX"
train.purchase.m<-train.purchase.m[order(train.purchase.m$customer_ID),]
#removing unneccessary variables
train.purchase.m$purchaseMinus_1 <- NULL
train.purchase.m$purchaseMinus_2 <- NULL
train.purchase.m$purchaseMinus_3 <- NULL
train.purchase.m$purchaseMinus_4 <- NULL

#table of who bought the last plan shopped
table(train.purchase.m$planCombo==train.purchase.m$lastQuotedPlan)

#adding quoting history to model data frame #PB
planOptions<-c("A","B","C","D","E","F","G")
for (ii in 1:7)  {
  train.purchase.m[paste("lastQuoted_",planOptions[ii],sep="")] <- as.factor(substring(train.purchase.m$lastQuotedPlan,first=ii,last=ii))
  train.purchase.m[paste("Quoted_",planOptions[ii],"_minus2",sep="")] <- as.factor(substring(train.purchase.m$QuoteMinus_2,first=ii,last=ii))
  train.purchase.m[paste("Quoted_",planOptions[ii],"_minus3",sep="")] <- as.factor(substring(train.purchase.m$QuoteMinus_3,first=ii,last=ii))
  train.purchase.m[paste("Quoted_",planOptions[ii],"_minus4",sep="")] <- as.factor(substring(train.purchase.m$QuoteMinus_4,first=ii,last=ii))
}


#looking at how often options change from purchase to last quote #PB
quoteChange_df<-data.frame(matrix(0,nrow=dim(train.purchase)[1],ncol=1))
planOptions<-c("A","B","C","D","E","F","G")
for (ii in 1:7)  {
  quoteChange_df[paste(planOptions[ii],"_change",sep="")] <- as.numeric((get(paste("lastQuoted_",planOptions[ii],sep=""),train.purchase.m))!=(get(planOptions[ii],train.purchase.m)))
}
quoteChange_df[1]<-NULL
colSums(quoteChange_df,2)
# A_change B_change C_change D_change E_change F_change G_change 
# 6894     6392     6716     4927     6067     7039    13174 



#####################################
## Impute missing values ##
#####################################
#we could use a decision tree to impute missing values. I am using the median to get the models working. Please feel free to change #PB
train.purchase.m$risk_factor[is.na(train.purchase.m$risk_factor)]<-median(train.purchase.m$risk_factor[!is.na(((train.purchase.m$risk_factor)))])
train.purchase.m$C_previous[is.na(train.purchase.m$C_previous)]<-as.factor((which.max(table(train.purchase.m$C_previous))))
train.purchase.m$duration_previous[is.na(train.purchase.m$duration_previous)]<-median(train.purchase.m$duration_previous[!is.na(((train.purchase.m$duration_previous)))])


#####################################
## Train/Validation Split ##
#####################################

# 75/25 train/validation split            #PB
n <-dim(train.purchase.m)[1] # sample size = 97009 customers purchase a policy
set.seed(1233) # set random number generator seed to enable
# repeatability of results
valid <- sample(n, round(.25*n)) # randomly sample 25% test
train.purchase.m$part<-0
train.purchase.m$part[-valid] <-"train"
train.purchase.m$part[valid]  <-"valid"
table(train.purchase.m$part)
trainSubset<-(which(train.purchase.m$part=="train"))
validSubset<-(which(train.purchase.m$part=="valid"))

# add variable "part" to the full training data set
lookup<-train.purchase.m[c("customer_ID","part")]
train<-merge(x=train,y=lookup,by="customer_ID")


#####################################
## Model Build ##
#####################################

###################
# Logistic Regression to predict the prob of buying something other than you last quote 
###################
#PB 
# #### Still working on the code below
# model.log1.A <- glm(A.change ~ (lastQuoted_A) + Quoted_A_minus2 + Quoted_A_minus3+ Quoted_A_minus4 + risk_factor  + group_size + car_age  + cost + age_oldest + age_youngest  + shopping_pt + state,
#                   data=train.purchase.m,subset = trainSubset , family=binomial("logit"))
# summary(model.log1.A)
# post.valid.log1.A <- predict(model.log1,train.purchase.m[validSubset,], type="response") # n.valid post probs
# table((post.valid.log1.A>.1),train.purchase.m$A.change[validSubset])


###################
# LDA Classification Example 
###################
### This is not working code, just here for an example on how to run LDA in R. #PB
library(MASS)
model.lda1 <- lda(x ~ y,data.train)
# Note: strictly speaking, LDA should not be used with qualitative predictors,
# but in practice it often is if the goal is simply to find a good predictive model
post.valid.lda1 <- predict(model.lda1, data.train)$posterior[,2] 

###################
# QDA Classification Example 
###################
### This is not working code, just here for an example on how to run LDA in R. #PB
qda.fit=qda(x ~ y,data.train)
qda.fit
qda.class=predict(qda.fit,data.validation)$class 
table(qda.class ,data.test$response)





# ###################
# # Dec Tree Model
# ###################
#PB
# Option A TREE ###################

library(tree)
tree.fit.A=tree(A ~ risk_factor + lastQuoted_A ,
                data=train.purchase.m,subset = trainSubset)

plot(tree.fit.A)
text(tree.fit.A,pretty=1)

cv.tree.fit.A=cv.tree(tree.fit.A,FUN=prune.misclass)
names(cv.tree.fit.A)
cv.tree.fit.A
par(mfrow=c(1,2))
plot(cv.tree.fit.A$size,cv.tree.fit.A$dev,type="b")
plot(cv.tree.fit.A$k,cv.tree.fit.A$dev,type="b")

# apply the prune.misclass() function in order to prune the tree to the nodes with the lowest dev
# lowest.dev.node<-cv.tree.fit.A$size[which.min(cv.tree.fit.A$dev)]
# prune.tree=prune.misclass(tree.fit.A,best=lowest.dev.node)
# plot(prune.tree)
# text(prune.tree,pretty=1)

post.valid.tree.A <- predict(tree.fit.A, train.purchase.m[validSubset,], type="class") # n.valid post probs
length(post.valid.tree.A)
table(post.valid.tree,train.purchase.m$A[validSubset])
error.tree.A <- round(mean(post.valid.tree!=train.purchase.m$A[validSubset]),4)
error.tree.A.base <- round(mean(train.purchase.m$lastQuoted_A[validSubset]!=train.purchase.m$A[validSubset]),4)

error.tree.A
error.tree.A.base 

# Option G TREE ###################


x<-model.matrix(~  (lastQuoted_G) + risk_factor + car_age + car_value + cost + age_oldest + age_youngest + day + shopping_pt + state +
                  Quoted_G_minus2 + Quoted_G_minus3 + Quoted_G_minus4, 
                data=train.purchase.m[trainSubset,])
y<-(train.purchase.m$G[trainSubset])

library(tree)
nobs<-length(trainSubset)
tree.fit.G=tree(y~x, control = tree.control(nobs, mindev=.01))

plot(tree.fit.G)
text(tree.fit.G,pretty=1)

cv.tree.fit.G=cv.tree(tree.fit.G,FUN=prune.misclass)
names(cv.tree.fit.G)
cv.tree.fit.G
par(mfrow=c(1,2))
plot(cv.tree.fit.G$size,cv.tree.fit.G$dev,type="b")
plot(cv.tree.fit.G$k,cv.tree.fit.G$dev,type="b")

# apply the prune.misclass() function in order to prune the tree to the nodes with the lowest dev
# lowest.dev.node<-cv.tree.fit.G$size[which.min(cv.tree.fit.G$dev)]
# prune.tree=prune.misclass(tree.fit.G,best=lowest.dev.node)
# plot(prune.tree)
# text(prune.tree,pretty=1)

x<-model.matrix(~  (lastQuoted_G) + risk_factor + car_age + car_value + cost + age_oldest + age_youngest + day + shopping_pt + state +
                  Quoted_G_minus2 + Quoted_G_minus3 + Quoted_G_minus4, 
                data=train.purchase.m[validSubset,])
y<-(train.purchase.m$G[validSubset])

post.valid.tree.G <- predict(tree.fit.G, data.frame(x), type="class") # n.valid post probs
length(post.valid.tree.G)
table(post.valid.tree.G,train.purchase.m$G[validSubset])
error.tree.G <- round(mean(post.valid.tree.G!=train.purchase.m$G[validSubset]),4)
error.tree.G.base <- round(mean(train.purchase.m$lastQuoted_G[validSubset]!=train.purchase.m$G[validSubset]),4)

error.tree.G
error.tree.G.base 


# ###################
# # Random Forest Model
# ###################

# Option A Model ###################


# library(randomForest)
# set.seed(3)
# model.rf.A <- randomForest(A ~ (lastQuoted_A) + risk_factor + car_age + car_value + cost + age_oldest + age_youngest + day + shopping_pt + state +
#                              Quoted_A_minus2 + Quoted_A_minus3 + Quoted_A_minus4 ,
#                                        data=train.purchase.m,subset = trainSubset,ntrees=500) 
# model.rf.A
# 
# #Var importance stats and plot
# randomForest::importance(model.rf.A)
# randomForest::varImpPlot(model.rf.A)
#  
# post.valid.rf.A <- predict(model.rf.A, train.purchase.m[validSubset,], type="class") 
# length(post.valid.rf.A)
# table(post.valid.rf.A,train.purchase.m$A[validSubset])
# error.rf.A <- round(mean(post.valid.rf.A!=train.purchase.m$A[validSubset]),4)
# error.rf.A.base <- round(mean(train.purchase.m$lastQuoted_A[validSubset]!=train.purchase.m$A[validSubset]),4)
#  
# error.rf.A
# error.rf.A.base 


# x<-model.matrix(~  (lastQuoted_G) + risk_factor + car_age + car_value + cost + age_oldest + age_youngest + day + shopping_pt + state +
#                   Quoted_G_minus2 + Quoted_G_minus3 + Quoted_G_minus4, 
#                 data=train.purchase.m[validSubset,])
# y<-(train.purchase.m$G[validSubset])
# 
# library(foreach)
# library(caret)
# rfParam <- expand.grid(ntree=500, importance=TRUE,mtry = 3)
# rfParam <- expand.grid(mtry = 3)
# 
# m <- train(x, y, method="parRF", tuneGrid=rfParam)
# 

# Option G Model ###################
model.rf.G <- randomForest(G ~ (lastQuoted_G) + risk_factor + car_age + car_value + cost + age_oldest + age_youngest + day + shopping_pt + state +
                             Quoted_G_minus2 + Quoted_G_minus3 + Quoted_G_minus4 ,
                           data=train.purchase.m,subset = trainSubset,ntrees=500) 
model.rf.G

#Var importance stats and plot
randomForest::importance(model.rf.G)
randomForest::varImpPlot(model.rf.G)

post.valid.rf.G <- predict(model.rf.G, train.purchase.m[validSubset,]) 
length(post.valid.rf.G)
table(post.valid.rf.G,train.purchase.m$G[validSubset])
error.rf.G <- round(mean(post.valid.rf.G!=train.purchase.m$G[validSubset]),4)
error.rf.G.base <- round(mean(train.purchase.m$lastQuoted_G[validSubset]!=train.purchase.m$G[validSubset]),4)

error.rf.G
error.rf.G.base 

library(caret)
confusionMatrix(post.valid.rf.G,train.purchase.m$G[validSubset],)




###################
# Boosting Model 
###################
library(gbm)
# gbm() with the option distribution="gaussian" for a regression problem; 
# gbm() for a classification problem, we would use distribution="bernoulli". 
# The argument n.trees=5000 indicates that we want 5000 trees, and the option interaction.depth=x limits the depth of each tree.

#Using this code to tune the GBM model. 
#Commented out due to the time it takes to run the code
# library(caret)
# myTuneGrid <- expand.grid(n.trees = 500,interaction.depth = c(6,7),shrinkage = c(.001,.01,.1),n.minobsinnode=10)
# fitControl <- trainControl(method = "repeatedcv", number = 3,repeats = 1, verboseIter = FALSE,returnResamp = "all")
# myModel <- train(data.train.std.c$donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + genf + wrat + 
#                    avhv + incm + inca + plow + npro + tgif  + tdon + tlag , 
#                  data=data.train.std.c,method = "gbm",trControl = fitControl,tuneGrid = myTuneGrid)

#Commented out due to the time it takes to run the code
set.seed(1)
model.boost.G=gbm(G ~ (lastQuoted_G) + risk_factor + car_age + car_value + cost + age_oldest + age_youngest + day + shopping_pt + state +
                  Quoted_G_minus2 + Quoted_G_minus3 + Quoted_G_minus4  ,
                data=train.purchase.m[trainSubset,],
                distribution="multinomial",
                n.trees=1000,
                interaction.depth=4,
                shrinkage = .01)

# The summary() function produces a relative influence plot and also outputs the relative influence statistics.
summary(model.boost.G)

summaryBoost<-summary(model.boost.G)


post.valid.boost.prob.G <- predict(model.boost.G, train.purchase.m[validSubset,],type='response',n.trees=1000) 
post.valid.boost.G<-apply(post.valid.boost.prob.G, 1, which.max)
length(post.valid.boost.G)
head(post.valid.boost.G)


table(post.valid.boost.G,train.purchase.m$G[validSubset])
error.boost.G <- round(mean(post.valid.boost.G!=train.purchase.m$G[validSubset]),4)
error.boost.G.base <- round(mean(train.purchase.m$lastQuoted_G[validSubset]!=train.purchase.m$G[validSubset]),4)

error.boost.G 
error.boost.G.base 
confusionMatrix(post.valid.boost.G,train.purchase.m$G[validSubset],)
#plotting relative influence of variables
summaryBoost<-summaryBoost[order(summaryBoost$rel.inf,decreasing=FALSE),]
par(mar=c(3,10,3,3))
barplot(t(summaryBoost$rel.inf),names.arg = summaryBoost$var ,las=2,col="darkblue",main = "Relative Influence",horiz=TRUE)
    
###################
# SVM
###################
# library(e1071)
# 
# svmfit.G=svm(G ~ (lastQuoted_G) + risk_factor + car_age + car_value + cost + age_oldest + age_youngest + day + shopping_pt + state +
#              Quoted_G_minus2 + Quoted_G_minus3 + Quoted_G_minus4  ,
#            data=train.purchase.m[train.purchase.m$part=="train",],
#            kernel="radial",  
#            gamma=.03125, 
#            cost=10,
#            probability =TRUE)
# summary(svmfit.G)


# #use this code to tune SVM using cross validation. 
# #commented out due to the time it requires to run # set.seed(1)
# # myTuneGrid <- expand.grid(sigma=2^c(-25,-5,-1),C=10)
# # fitControl <- trainControl(method = "cv", number = 5,repeats = 1, verboseIter = FALSE,returnResamp = "all",classProbs = TRUE)
# # myModel <- train(as.factor(data.train.std.c$donr) ~ reg1 + reg2 + reg3 + reg4 + home + factor(chld) + hinc + genf + wrat + 
# #                    avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
# #                  data=data.train.std.c , method = "svmRadial",trControl = fitControl,tuneGrid = myTuneGrid)

post.valid.svm.G<-predict(svmfit.G,train.purchase.m[validSubset,])
length(post.valid.svm.G)

table(post.valid.svm.G,train.purchase.m$G[validSubset])
error.svm.G <- round(mean(post.valid.svm.G!=train.purchase.m$G[validSubset]),4)
error.svm.G.base <- round(mean(train.purchase.m$lastQuoted_G[validSubset]!=train.purchase.m$G[validSubset]),4)

error.svm.G 
error.svm.G.base 


###################
# ## Data manipulation for test set ## 
###################
#PB

#Baseline prediction using last quoted plan #.53793 on Kaggle #PB
test$planCombo<-paste(test$A,test$B,test$C,test$D,test$E,test$F,test$G,sep="")
#find the last shopping point for each customer
maxShopping_pt<-(aggregate(test$shopping_pt, by = list(test$customer_ID), max))
names(maxShopping_pt)<-c("customer_ID","lastquote")
lookup<-test[c("customer_ID","shopping_pt","planCombo")]

#SUBMIT File for baseline model - based on last quote 
#merge each customer ID with their last quoted plan
lastQuoteSubmit<-merge(x=maxShopping_pt,y=lookup,by.x=c("lastquote","customer_ID"),by.y=c("shopping_pt","customer_ID"))
names(lastQuoteSubmit)[names(lastQuoteSubmit)=="planCombo"]<-"lastQuotedPlan"
#clean up dataframe
lastQuoteSubmit$lastquote <- NULL
lastQuoteSubmit<-lastQuoteSubmit[order(lastQuoteSubmit$customer_ID),]
names(lastQuoteSubmit)<-c("customer_ID","plan")

#data frame of unique customers
test.m<-merge(x=maxShopping_pt,y=test,by.x=c("lastquote","customer_ID"),by.y=c("shopping_pt","customer_ID"))
names(test.m)[names(test.m)=="planCombo"]<-"lastQuotedPlan"
test.m$shopping_pt<-test.m$lastquote

#Adding previous quoted plans to train.purchase.m data frame #PB
test.m$purchaseMinus_2<-test.m$lastquote-1
test.m$purchaseMinus_3<-test.m$lastquote-2
test.m$purchaseMinus_4<-test.m$lastquote-3

test.m<-merge(x=test.m,y=lookup,by.x=c("purchaseMinus_2","customer_ID"),by.y=c("shopping_pt","customer_ID"),all.x=TRUE)
names(test.m)[names(test.m)=='planCombo']<-"QuoteMinus_2"

test.m<-merge(x=test.m,y=lookup,by.x=c("purchaseMinus_3","customer_ID"),by.y=c("shopping_pt","customer_ID"),all.x=TRUE)
names(test.m)[names(test.m)=='planCombo']<-"QuoteMinus_3"

test.m<-merge(x=test.m,y=lookup,by.x=c("purchaseMinus_4","customer_ID"),by.y=c("shopping_pt","customer_ID"),all.x=TRUE)
names(test.m)[names(test.m)=='planCombo']<-"QuoteMinus_4"

#Cleaning up the test.m data frame
test.m$QuoteMinus_3[is.na(test.m$QuoteMinus_3)]<-"XXXXXXX"
test.m$QuoteMinus_4[is.na(test.m$QuoteMinus_4)]<-"XXXXXXX"
test.m<-test.m[order(test.m$customer_ID),]

#adding quoting history for each option to model data frame #PB
planOptions<-c("A","B","C","D","E","F","G")
for (ii in 1:7)  {
  test.m[paste("lastQuoted_",planOptions[ii],sep="")] <- as.factor(substring(test.m$lastQuotedPlan,first=ii,last=ii))
  test.m[paste("Quoted_",planOptions[ii],"_minus2",sep="")] <- as.factor(substring(test.m$QuoteMinus_2,first=ii,last=ii))
  test.m[paste("Quoted_",planOptions[ii],"_minus3",sep="")] <- as.factor(substring(test.m$QuoteMinus_3,first=ii,last=ii))
  test.m[paste("Quoted_",planOptions[ii],"_minus4",sep="")] <- as.factor(substring(test.m$QuoteMinus_4,first=ii,last=ii))
}

test.m<-test.m[order(test.m$customer_ID),]
head(test.m)


#####################################
## Impute missing values ##
#####################################
#we could use a decision tree to impute missing values. I am using the median to get the models working. Please feel free to change #PB
test.m$risk_factor[is.na(test.m$risk_factor)]<-median(train.purchase.m$risk_factor[!is.na(((train.purchase.m$risk_factor)))])
test.m$C_previous[is.na(test.m$C_previous)]<-as.factor((which.max(table(train.purchase.m$C_previous))))
test.m$duration_previous[is.na(test.m$duration_previous)]<-median(train.purchase.m$duration_previous[!is.na(((train.purchase.m$duration_previous)))])


# #Predict A on the test set #PB
# predictA <- predict(model.rf.A, test.m, type="class") 
# predictResults<-data.frame(test.m$customer_ID,predictA)
# 
# #replacing last quoted A with predicted A #PB #0.53602.
# predictA_Submit<-merge(x=lastQuoteSubmit,y=predictResults,by.x=c("customer_ID"),by.y=c("test.m.customer_ID"))
# predictA_Submit$plan<-(paste(predictA_Submit$predictA,substring(predictA_Submit$plan,first=2,last=7),sep=""))
# predictA_Submit$predictA<-NULL
# 


#Predict G on the test set using Random Forest #PB  0.53955
predictG <- predict(model.rf.G, test.m, type="class")

#Predict G on the test set using Boosted Tree  #PB   0.54045
predictG <- predict(model.boost.G, test.m,type='response',n.trees=1000) 
predictG<-apply(predictG, 1, which.max)


predictResults<-data.frame(test.m$customer_ID,predictG)
test.m$predictG<-predictG

# hard coded rules
#all resdints in Florida should select 3 or 4, predictions of 2 change to 3, could use decision tree for this #PB
predictResults$predictG[test.m$state=="FL" & test.m$predictG=="2"]<-as.factor("3")
#add option F - NY and CT rules

#replacing last quoted G with predicted G #PB 
predictG_Submit<-merge(x=lastQuoteSubmit,y=predictResults,by.x=c("customer_ID"),by.y=c("test.m.customer_ID"))
predictG_Submit$plan<-paste(substring(predictG_Submit$plan,first=1,last=6),predictG_Submit$predictG,sep="")
predictG_Submit$predictG<-NULL
head(predictG_Submit)



#view sample before exporting to csv #PB  
head(predictG_Submit)
write.csv(predictG_Submit, file=file.path(path,"submit_GBM_1.csv"), row.names=FALSE, quote=FALSE)

