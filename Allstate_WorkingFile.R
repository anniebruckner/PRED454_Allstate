#PRED 454 Advances Modeling
#Allstate Purchase Prediction Challenge

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
                      ,"plyr"
					  ,"maps")

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
path <- "/Users/paulbertucci/Desktop/MSPA/PRED454_AdvancedModeling/FinalProject/AllState" #paulpath

#load the train and the test data
train <- read.csv(file.path(path,"train.csv"), stringsAsFactors=TRUE)
test <- read.csv(file.path(path,"test_v2.csv"), stringsAsFactors=TRUE)



#####################################
## EDA ##
#####################################


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
detach(train)

#####Begin Data Description - DT 1/22/17
#Graphing policy shoppers by state

#load state codes csv file
state_codes <- read.csv(file.choose())
#head(state_codes)

state_df <- as.data.frame(table(state)) #turn state data into DF for manipulating purposes
state_df$postal_code <- state_df$state #rename column for easy merging below
#head(state_df)

#merge state data frame with frequency counts and postal codes to get state names
state_combo <- merge(state_codes, state_df, by="postal_code", all=TRUE)
#head(state_combo)

#merge state_combo with state plotting data
names(state_combo)[names(state_combo)=="state.x"] <- "region"
state_combo$region<-tolower(state_combo$region) #done for merging on region to work
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
par.default <- par() #save in case needed later
par(mfrow=c(1,2)) #fit more graphs
#barplots for predictor variables
par(mfrow=c(1,2)); ylab.box <- "Number of Records"; col.bar = "dodgerblue4" 
barplot(table(car_value),main="Frequency by Car Values (New)",ylab=ylab.box,xlab="Car Value Categories",col=col.bar)
barplot(table(day),main="Frequency of Site Visits by Day",ylab=ylab.box,xlab="Day (0=Monday,...,6=Sunday)",col=col.bar)
barplot(table(C_previous),main="Frequency by Policy Option C",ylab=ylab.box,xlab="Policy Option C Choices (0=nothing)",col=col.bar)
barplot(table(record_type),main="Frequency of Record Types",ylab=ylab.box,xlab="0 = Shopping Point, 1 = Purchase Point",col=col.bar)
barplot(table(homeowner),main="Frequency by Homeowner", ylab=ylab.box,xlab="0 = Not Homeowner, 1 = Homeowner",col=col.bar)
barplot(table(married_couple),main="Frequency by Marital Status",ylab=ylab.box,xlab="0 = Married, 1 = Not Married",col=col.bar)
barplot(table(risk_factor),main="Frequency by Risk Factor",ylab=ylab.box,xlab="Risk Factor Levels",col=col.bar)
barplot(table(shopping_pt),main="Frequency by Shopping Point",ylab=ylab.box,xlab="Shopping Point Depth",col=col.bar)
barplot(table(time),main="Frequency of Site Visits by Time of Day",ylab=ylab.box,xlab="Time (HH:MM)",col=col.bar,border=NA)

#histograms for predictor variables
col.hist = "dodgerblue4" 
hist(car_age,xlab="Age of Car (Years)", main="Frequency by Age of Car",col=col.hist)
hist(age_oldest,xlab="Age of Oldest Person in Group", main="Frequency by Oldest Age",col=col.hist)
hist(age_youngest,xlab="Age of Youngest Person in Group", main="Frequency by Youngest Age",col=col.hist)
hist(cost,xlab="Cost (Dollars)", main="Frequency by Cost of Coverage Options",col=col.hist)
hist(duration_previous,xlab="Duration (Years)", main="Previous Insurance Issuer Duration",col=col.hist)


#barplots for response variables
xlab.policy = "Policy Options"; par(mfrow=c(1,4))

barplot(table(A),main="Frequency for Option A",ylab=ylab.box,xlab=xlab.policy,col=col.bar)
barplot(table(B),main="Frequency for Option B",ylab=ylab.box,xlab=xlab.policy,col=col.bar)
barplot(table(C),main="Frequency for Option C",ylab=ylab.box,xlab=xlab.policy,col=col.bar)
barplot(table(D),main="Frequency for Option D",ylab=ylab.box,xlab=xlab.policy,col=col.bar)
par(mfrow=c(1,3))
barplot(table(E),main="Frequency for Option E",ylab=ylab.box,xlab=xlab.policy,col=col.bar)
barplot(table(F),main="Frequency for Option F",ylab=ylab.box,xlab=xlab.policy,col=col.bar)
barplot(table(G),main="Frequency for Option G",ylab=ylab.box,xlab=xlab.policy,col=col.bar)


#look at 5 number summaries
lapply(train, summary)

#consider outliers in select variables
col.box = "dodgerblue4"
bxp.shopping_pt <- boxplot(shopping_pt,main="Box Plot of Shopping Points", ylab="Shopping Point", col=col.box)
bxp.car_age <- boxplot(car_age,main="Box Plot of Car Age", ylab="Car Age (Years)", col=col.box)
bxp.cost <- boxplot(cost,main="Box Plot of Policy Option Cost", ylab="Cost (Dollars)", col=col.box)
#####End Data Description - DT 1/22/17


# summary statistics
summary(train)
# risk_factor 240418 NAs
# C_previous & duration_previous - 1811 NAs


#Creating planCombo variable for plan total #PB
train$planCombo<-paste(train$A,train$B,train$C,train$D,train$E,train$F,train$G)

#creating a dataframe of purchased records #PB
train.purchase<-train[which(train$record_type==1),]

#Adding previous quorted plans to train.purchase data frame #PB
train.purchase$purchaseMinus_1<-train.purchase$shopping_pt-1
train.purchase$purchaseMinus_2<-train.purchase$shopping_pt-2
train.purchase$purchaseMinus_3<-train.purchase$shopping_pt-3
train.purchase$purchaseMinus_4<-train.purchase$shopping_pt-4

#purchase minus 1 quote
lookup<-train[c("customer_ID","shopping_pt","planCombo")]
train.purchase<-merge(x=train.purchase,y=lookup,by.x=c("purchaseMinus_1","customer_ID"),by.y=c("shopping_pt","customer_ID"))
#cleaning up train.purchase dataframe
names(train.purchase)[names(train.purchase)=='planCombo.x']<-"planCombo"
names(train.purchase)[names(train.purchase)=='planCombo.y']<-"lastQuotedPlan"

#purchase minus 2 quote
train.purchase<-merge(x=train.purchase,y=lookup,by.x=c("purchaseMinus_2","customer_ID"),by.y=c("shopping_pt","customer_ID"),all.x=TRUE)
#cleaning up train.purchase dataframe
names(train.purchase)[names(train.purchase)=='planCombo.x']<-"planCombo"
names(train.purchase)[names(train.purchase)=='planCombo.y']<-"QuoteMinus_2"

#purchase minus 3 quote
train.purchase<-merge(x=train.purchase,y=lookup,by.x=c("purchaseMinus_3","customer_ID"),by.y=c("shopping_pt","customer_ID"),all.x=TRUE)
#cleaning up train.purchase dataframe
names(train.purchase)[names(train.purchase)=='planCombo.x']<-"planCombo"
names(train.purchase)[names(train.purchase)=='planCombo.y']<-"QuoteMinus_3"

#purchase minus 4 quote
train.purchase<-merge(x=train.purchase,y=lookup,by.x=c("purchaseMinus_4","customer_ID"),by.y=c("shopping_pt","customer_ID"),all.x=TRUE)
#cleaning up train.purchase dataframe
names(train.purchase)[names(train.purchase)=='planCombo.x']<-"planCombo"
names(train.purchase)[names(train.purchase)=='planCombo.y']<-"QuoteMinus_4"

#Cleaning up the train.purchsase data frame
train.purchase$QuoteMinus_3[is.na(train.purchase$QuoteMinus_3)]<-"NA"
train.purchase$QuoteMinus_4[is.na(train.purchase$QuoteMinus_4)]<-"NA"
train.purchase<-train.purchase[order(train.purchase$customer_ID),]
#removing unneccessary variables
train.purchase$purchaseMinus_1 <- NULL
train.purchase$purchaseMinus_2 <- NULL
train.purchase$purchaseMinus_3 <- NULL
train.purchase$purchaseMinus_4 <- NULL

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
hist(as.numeric((train.purchase$shopping_pt)),col="blue",breaks=20,xlab = "Purchase Shopping Point",
     main = "Purchase Point - Training Set")

# distribution of shopping point for purchased plans
hist(as.numeric((train.purchase$shopping_pt)),col="darkblue",breaks=20,xlab = "Purchase Shopping Point",
     main = "Purchase Shopping Point - Training Set")
maxShoppingPoint.test<-aggregate(test$shopping_pt, by = list(test$customer_ID), max)
hist(as.numeric((maxShoppingPoint.test$x)),col="darkblue",breaks=20,xlab = "Max Shopping Point",
     main = "Max Shopping Point - Test Set")


#Exploring the components of a plan against variables
histogram(~ A+B+C+D+E+F+G | car_value, data = train.purchase)
histogram(~ C | car_value, data = train.purchase)


# histogram(~ A+B+C +D+E+F+G| homeowner, data = train.purchase)

#Frequency of policy Option by state
A_Freq<-prop.table(table(train.purchase$state,train.purchase$A),1)
B_Freq<-prop.table(table(train.purchase$state,train.purchase$B),1)
C_Freq<-prop.table(table(train.purchase$state,train.purchase$C),1)
D_Freq<-prop.table(table(train.purchase$state,train.purchase$D),1)
E_Freq<-prop.table(table(train.purchase$state,train.purchase$E),1)
F_Freq<-prop.table(table(train.purchase$state,train.purchase$F),1)
G_Freq<-prop.table(table(train.purchase$state,train.purchase$G),1)
#Stack bar graph
plot(F_Freq)
plot(G_Freq)


#can't get the below function to work, trying to plot hist for each a variable for each purchase option
my_hist2<-function(variable)
{
  x <- get(variable)
  ggplot(train.purchase,aes(x=day))+geom_histogram()+facet_grid(~x)
  #h<-hist(x,breaks=seq(from=-.5,to=4.5,by=1),col="red",main=variable)
}
apply(X = array(names(train.purchase)[18:24]),MARGIN =1,FUN = my_hist2)


ddply(train,~customer_ID,summarise,mean=mean(group_size))
train.uniquechar = unique(train[c("customer_ID","state", "group_size","homeowner","car_age","car_value","risk_factor","age_oldest",
                                  "age_youngest","married_couple","C_previous","duration_previous")])

#add numeric factors in place of categorical for correlation analysis
train_cp = train

state_factor = as.factor(train[,c("state")])
state_ranks <- rank(-table(state_factor), ties.method="first")
train_cp$state_num <- data.frame(category=state_factor, rank=state_ranks[as.character(state_factor)])$rank

car_value_factor = as.factor(train[,c("car_value")])
car_value_ranks <- rank(-table(car_value_factor), ties.method="first")
train_cp$car_value_num <- data.frame(category=car_value_factor, rank=car_value_ranks[as.character(car_value_factor)])$rank

#correlation matrix for numeric variables
cormat = cor(train_cp[c(2:4,7:10,12:17,27:28)], use="na.or.complete")
cormat_table <- as.data.frame(as.table(cormat))
cormat_table <- cormat_table[order(abs(cormat_table$Freq),decreasing = TRUE),]
write.csv(cormat_table, "cormat_table.csv")

#PCA
train.pca <- prcomp(na.omit(train_cp[c(2:4,7:10,12:17,27:28)]),center = TRUE,scale. = TRUE)
print(train.pca)
summary(train.pca)
plot(train.pca, type="l")

write.csv(train.pca$rotation,"pca.csv")

#####################################
## Train/Validation Split ##
#####################################

# 75/25 train/validation split            #PB
n <-dim(train.purchase)[1] # sample size = 97009 customers purchase a policy
set.seed(1233) # set random number generator seed to enable
# repeatability of results
valid <- sample(n, round(.25*n)) # randomly sample 25% test
train.purchase$part<-0
train.purchase$part[-valid] <-"train"
train.purchase$part[valid]  <-"valid"
table(train.purchase$part)

# add variable "part" to the full training data set
lookup<-train.purchase[c("customer_ID","part")]
train<-merge(x=train,y=lookup,by="customer_ID")




#####################################
## Model Build ##
#####################################


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
# Example code below , do not use #PB

# library(tree)
# 
# tree.fit=tree(factor(data.train.std.c$donr) ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + genf + wrat + 
#                 avhv_log + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
#               data=data.train.std.c)
# 
# plot(tree.fit)
# text(tree.fit,pretty=1)
# 
# cv.tree.fit=cv.tree(tree.fit,FUN=prune.misclass)
# names(cv.tree.fit)
# cv.tree.fit
# par(mfrow=c(1,2))
# plot(cv.tree.fit$size,cv.tree.fit$dev,type="b")
# plot(cv.tree.fit$k,cv.tree.fit$dev,type="b")
# 
# # We now apply the prune.misclass() function in order to prune the tree to the nodes with the lowest dev
# lowest.dev.node<-cv.tree.fit$size[which.min(cv.tree.fit$dev)]
# prune.tree=prune.misclass(tree.fit,best=lowest.dev.node)
# plot(prune.tree)
# text(prune.tree,pretty=1)
# 
# post.valid.tree <- predict(prune.tree, data.valid.std.c, type="vector")[,2] # n.valid post probs
# 
# profit.tree <- cumsum(14.5*c.valid[order(post.valid.tree, decreasing=T)]-2)
# plot(profit.tree) # see how profits change as more mailings are made
# n.mail.valid <- which.max(profit.tree) # number of mailings that maximizes profits
# c(n.mail.valid, max(profit.tree)) # report number of mailings and maximum profit
# n.mail.valid
# max(profit.tree)
# 
# cutoff.tree <- sort(post.valid.tree, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
# chat.valid.tree <- ifelse(post.valid.tree>cutoff.tree, 1, 0) # mail to everyone above the cutoff
# table(chat.valid.tree, c.valid) # classification table
# error.tree <- round(mean(chat.valid.tree!=c.valid),4)
# 
# 
# 
# ###################
# # Random Forest Model
# ###################
# Example code below , do not use #PB

# set.seed(3)
# model.rf <- randomForest::randomForest(factor(data.train.std.c$donr) ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + genf + wrat + 
#                                          avhv_log + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
#                                        data.train.std.c) 
# model.rf
# 
# #Var importance stats and plot
# randomForest::importance(model.rf)
# randomForest::varImpPlot(model.rf)
# 
# post.valid.rf <- predict(model.rf, data.valid.std.c,type='prob')[,2] # n.valid.c post probs
# # calculate ordered profit function using average donation = $14.50 and mailing cost = $2
# 
# profit.rf <- cumsum(14.5*c.valid[order(post.valid.rf, decreasing=T)]-2)
# plot(profit.rf) # see how profits change as more mailings are made
# n.mail.valid <- which.max(profit.rf) # number of mailings that maximizes profits
# c(n.mail.valid, max(profit.rf)) # report number of mailings and maximum profit
# 
# cutoff.rf <- sort(post.valid.rf, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
# chat.valid.rf <- ifelse(post.valid.rf>cutoff.rf, 1, 0) # mail to everyone above the cutoff
# table(chat.valid.rf, c.valid) # classification table
# error.rf <- round(mean(chat.valid.rf!=c.valid),4)




###################
# Boosting Model 
###################
# Example code below , do not use #PB
# library(gbm)
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

# set.seed(1)
# model.boost=gbm(data.train.std.c$donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + genf + wrat + 
#                   avhv + incm + inca + plow + npro + tgif  + tdon + tlag , 
#                 data=data.train.std.c,distribution="bernoulli",n.trees=5000,interaction.depth=6,shrinkage = .01)
# 
# # The summary() function produces a relative influence plot and also outputs the relative influence statistics.
# summary(model.boost)
# 
# post.valid.boost <- predict(model.boost, data.valid.std.c,type='response',n.trees=5000) # n.valid.c post probs
# # calculate ordered profit function using average donation = $14.50 and mailing cost = $2
# 
# profit.boost <- cumsum(14.5*c.valid[order(post.valid.boost, decreasing=T)]-2)
# plot(profit.boost) # see how profits change as more mailings are made
# n.mail.valid <- which.max(profit.boost) # number of mailings that maximizes profits
# c(n.mail.valid, max(profit.boost)) # report number of mailings and maximum profit
# 
# cutoff.boost <- sort(post.valid.boost, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
# chat.valid.boost <- ifelse(post.valid.boost>cutoff.boost, 1, 0) # mail to everyone above the cutoff
# table(chat.valid.boost, c.valid) # classification table
# error.boost <- round(mean(chat.valid.boost!=c.valid),4)



###################
# SVM
###################
# Example code below , do not use #PB
# 
# library(e1071)
# 
# svmfit=svm(as.factor(data.train.std.c$donr) ~ reg1 + reg2 + reg3 + reg4 + home + factor(chld) + hinc + genf + wrat + 
#              avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
#            data=data.train.std.c, kernel="radial",  gamma=.03125, cost=10,probability =TRUE)
# summary(svmfit)
# 
# #use this code to tune SVM using cross validation. 
# #commented out due to the time it requires to run # set.seed(1)
# # myTuneGrid <- expand.grid(sigma=2^c(-25,-5,-1),C=10)
# # fitControl <- trainControl(method = "cv", number = 5,repeats = 1, verboseIter = FALSE,returnResamp = "all",classProbs = TRUE)
# # myModel <- train(as.factor(data.train.std.c$donr) ~ reg1 + reg2 + reg3 + reg4 + home + factor(chld) + hinc + genf + wrat + 
# #                    avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
# #                  data=data.train.std.c , method = "svmRadial",trControl = fitControl,tuneGrid = myTuneGrid)
# 
# 
# pred<-predict(svmfit, data.valid.std.c,probability = TRUE)
# post.valid.svm <- attr(pred, "probabilities")[,2] # n.valid.c post probs
# profit.svm <- cumsum(14.5*c.valid[order(post.valid.svm, decreasing=T)]-2)
# plot(profit.svm) # see how profits change as more mailings are made
# n.mail.valid <- which.max(profit.svm) # number of mailings that maximizes profits
# c(n.mail.valid, max(profit.svm)) # report number of mailings and maximum profit
# 
# cutoff.svm <- sort(post.valid.svm, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
# chat.valid.svm <- ifelse(post.valid.svm>cutoff.svm, 1, 0) # mail to everyone above the cutoff
# table(chat.valid.svm, c.valid) # classification table
# error.svm <- round(mean(chat.valid.svm!=c.valid),4)




#Baseline prediction using last quoted plan #.53793 on Kaggle #PB
test$planCombo<-paste(test$A,test$B,test$C,test$D,test$E,test$F,test$G,sep="")
#find the last shopping point for each customer
maxShopping_pt<-(aggregate(test$shopping_pt, by = list(test$customer_ID), max))
names(maxShopping_pt)<-c("customer_ID","lastquote")
lookup<-test[c("customer_ID","shopping_pt","planCombo")]
#merge each customer ID with their last quoted plan
csvSubmit<-merge(x=maxShopping_pt,y=lookup,by.x=c("lastquote","customer_ID"),by.y=c("shopping_pt","customer_ID"))
#clean up dataframe
csvSubmit$lastquote <- NULL
csvSubmit<-csvSubmit[order(csvSubmit$customer_ID),]
names(csvSubmit)<-c("customer_ID","plan")

#view sample before exporting to csv
head(csvSubmit)
write.csv(csvSubmit, file=file.path(path,"submit_2.csv"), row.names=FALSE, quote=FALSE)






