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
                      ,"reshape"
                      ,"reshape2"
                      ,"nnet"
                      ,"mlogit"
                      ,"e1071"
                      ,"gmodels")

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
# <---Ahmar: Start of missing values Impute -------------->
# Check for NAs %ages
round(colSums(is.na(train))[colSums(is.na(train)) > 0] * 100/ dim(train)[1],2)

# variables with missing data:
# risk_factor   C_previous    duration_previous 
# 240418        18711         18711
# variable with missing data in %age:
#risk_factor        C_previous duration_previous 
#36.14              2.81              2.81 

# Finding association between C-previous & duration_previous
table(train$C_previous,train$duration_previous, exclude =NULL)

table(train$C_previous, exclude=NULL)
table(train$risk_factor, exclude=NULL)

# Finding association between risk_factor and other variables.
set.seed(1)
tree.x <- rpart(risk_factor ~ car_value + homeowner+married_couple+car_age+age_oldest + age_youngest + duration_previous+group_size    , data = train, method = "class")
tree.x # splits on age_oldest
prp(tree.x)
fancyRpartPlot(tree.x,main="Risk Factor Association", sub = "   ", tweak=1, palettes=c( "YlOrRd")) # unreadable

# Imputing missing values
# risk_factor
train$risk_factor_imp = train$risk_factor
train$risk_factor_imp[is.na(train$risk_factor_imp) & train$age_oldest >=58] <- 1;table(train$risk_factor_imp, exclude=NULL)
train$risk_factor_imp[is.na(train$risk_factor_imp) & train$age_oldest>=22] <- 4;table(train$risk_factor_imp, exclude=NULL)
train$risk_factor_imp[is.na(train$risk_factor_imp) & train$age_oldest<22] <- 3;table(train$risk_factor_imp, exclude=NULL)

# C_previous
train$C_previous_imp <- train$C_previous
train$C_previous_imp[is.na(train$C_previous_imp)] <- 0;table(train$C_previous_imp, exclude=NULL)

# duration_previous
train$duration_previous_imp <- train$duration_previous
train$duration_previous_imp[is.na(train$duration_previous_imp)] <- 0;table(train$duration_previous_imp, exclude=NULL)

###   Test Data ################
# Check for NAs
colSums(is.na(test))[colSums(is.na(test)) > 0]
# <---Ahmar: Start of missing values Impute -------------->
# Check for NAs %ages
round(colSums(is.na(test))[colSums(is.na(test)) > 0] * 100/ dim(test)[1],2)

# variables with missing data:
#location       risk_factor        C_previous duration_previous 
#678             75487              9769              9769 
# variable with missing data in %age:
#location       risk_factor        C_previous duration_previous 
#0.34             37.96              4.91              4.91 

# Finding association between C-previous & duration_previous
table(test$C_previous,test$duration_previous, exclude =NULL)

table(test$C_previous, exclude=NULL)
table(test$risk_factor, exclude=NULL)
table(test$location, exclude=NULL)

# Finding association between risk_factor and other variables.
set.seed(1)
tree.x <- rpart(risk_factor ~ car_value + homeowner+married_couple+car_age+age_oldest + age_youngest + duration_previous+group_size    , data = test, method = "class")
tree.x # splits on age_oldest
prp(tree.x)
fancyRpartPlot(tree.x,main="Risk Factor Association", sub = "   ", tweak=1, palettes=c( "YlOrRd")) # unreadable

# state and location
st_loc = data.frame(state=test$state, loc=test$location )

#st_loc
dim(st_loc) #198856      2

colSums(is.na(st_loc))[colSums(is.na(st_loc)) > 0] # 678

# Remove NA locations
st_loc = st_loc[which(!is.na(st_loc$loc)),]

dim(st_loc) #198178      2

# location frequency by state
st_loc = ddply(test, .(state,location), summarize, frequency = (length(state)))

st_loc = st_loc[which(!is.na(st_loc$location)),]

# Maximum location frequency by State
dfsl = setkeyv(setDT(st_loc), c("state","location"))[,list(mxloc=max(frequency)), by=list(state)]

# Location, maximum frequency by location 
st_loc = merge(st_loc, dfsl, by.x=c("state","frequency"), by.y = c("state", "mxloc"))

# Remove duplicate locations by State based on frequency - taking highest location id
st_loc = setkeyv(setDT(st_loc), c("state","frequency"))[,list(mxloc=max(location)), by=list(state,frequency)]

# Temp df and merge by state 
test2 = test #198856 25var
test2 = merge(test2, st_loc, by.x=c("state"), by.y = c("state"), all.x=TRUE)

colSums(is.na(test))[colSums(is.na(test)) > 0]


# Imputing missing values
# risk_factor
test$risk_factor_imp = test$risk_factor
test$risk_factor_imp[is.na(test$risk_factor_imp) & test$age_oldest >=58] <- 1;table(test$risk_factor_imp, exclude=NULL)
test$risk_factor_imp[is.na(test$risk_factor_imp) & test$age_oldest>=22] <- 4;table(test$risk_factor_imp, exclude=NULL)
test$risk_factor_imp[is.na(test$risk_factor_imp) & test$age_oldest<22] <- 3;table(test$risk_factor_imp, exclude=NULL)

# C_previous
test$C_previous_imp <- test$C_previous
test$C_previous_imp[is.na(test$C_previous_imp)] <- 0;table(test$C_previous_imp, exclude=NULL)

# duration_previous
test$duration_previous_imp <- test$duration_previous
test$duration_previous_imp[is.na(test$duration_previous_imp)] <- 0;table(test$duration_previous_imp, exclude=NULL)


# Replace missing locations from State Level high frequency locations.
test$location_imp <- test$location
test$location_imp[which(is.na(test$location))] <- test2$mxloc[which(is.na(test$location))]

colSums(is.na(test))[colSums(is.na(test)) > 0]

# <----------------- End of Missing Impute ----------------> 

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
names(state_combo)[names(state_combo)=="state"] <- "region"
state_combo$region<-tolower(state_combo$region) #done for merging on region to work
# [ SOLVED] AM: changed state.x to state -> # AB: I got this error when I ran the above line of code:
# Error in `$<-.data.frame`(`*tmp*`, "region", value = character(0)) : 
# replacement has 0 rows, data has 59
state_total <- merge(all_states,state_combo, by="region", all=TRUE)
# AM: all_state object is missing
#  Error in merge(all_states, state_combo, by = "region", all = TRUE) : 
#  object 'all_states' not found
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
barplot(table(train$married_couple),main="Frequency by Marital Status",ylab=ylab.box,xlab="0 = Not Married, 1 = Married",col=col.bar)
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

# Plots for Checkpoint 2 Data Check
colplots <- "dodgerblue4"
ylab.box <- "Number of Records"
par(mfrow=c(2,4))
hist(train$car_age,xlab="Age of Car (Years)", main="Frequency by Age of Car",col=colplots)
barplot(table(train$shopping_pt),main="Frequency by Shopping Point",ylab=ylab.box,xlab="Shopping Point Depth",col=colplots)
hist(train$duration_previous,xlab="Duration (Years)", main="Previous Insurance Issuer Duration",col=colplots)
barplot(table(train$day),main="Frequency of Site Visits by Day",ylab=ylab.box,xlab="Day (0=Monday,...,6=Sunday)",col=colplots)
barplot(table(train$car_value),main="Frequency by Car Values (New)",ylab=ylab.box,xlab="Car Value Categories",col=colplots)
hist(train$age_oldest,xlab="Age of Oldest Person in Group", main="Frequency by Oldest Age",col=colplots)
hist(train$age_youngest,xlab="Age of Youngest Person in Group", main="Frequency by Youngest Age",col=colplots)

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
#my_hist2<-function(variable)
#{
  #x <- get(variable)
#  ggplot(train.purchase,aes(x=day))+geom_bar()+facet_grid(~variable)
  #h<-hist(x,breaks=seq(from=-.5,to=4.5,by=1),col="red",main=variable)
#}
#apply(X = array(names(train.purchase)[18:24]),MARGIN =1,FUN = my_hist2)

#find numeric columns #SC
nums <- sapply(train.purchase, is.numeric)
train.purchase[ , nums]
str(train.purchase)
ggplot(train.purchase,aes(x=day)) + theme_bw() + facet_grid(~A) + geom_bar(color =I("black"), fill = I("dodgerblue4")) + ggtitle("Insurance Option A") + theme(plot.title = element_text(hjust = 0.5))

#graph of predictor vs response
ggplot(train.purchase,aes(x=age_oldest))+geom_bar()+facet_grid(~A)
ggplot(train.purchase,aes(x=train.purchase[,paste("day")]))+geom_bar()+facet_grid(paste("~","A"))

forLoopGraph <- function(x) {
  for (i in 1:7) {
    t = ggplot(train.purchase,aes(x=train.purchase[,paste(x)]))+geom_bar()+facet_grid(paste("~",names(train.purchase)[17+1]))
  }
  return(t)
}

forLoopGraph("car_value")
dfgraph = apply(X=array(names(train.purchase)[c(4,8:17)]), MARGIN = 1, FUN = forLoopGraph)
dfgraph[2]
#histtable of each predictor for each response #SC

pctTot <- function(x) { 
  length(x) / nrow(train.purchase) * 100
}

forLoopFunc <- function(x) {
  print(x)
  histtableEDAtemp = list()
  for (i in 1:7) {
    #print(myFunction2(train.purchase, names(train.purchase)[17+i], x))
    df = melt(cast(train.purchase, paste(names(train.purchase)[17+i],x, sep = "~"), pctTot))
    df$col1 = names(df)[1]
    df$col2 = names(df)[3]
    names(df)[1] = "cat1"
    names(df)[3] = "cat2"
    histtableEDAtemp[[i]] = df
  }
  histtableEDA = do.call(rbind, histtableEDAtemp)
  return(histtableEDA)
}

#EDA for continous variables (average by coverage option) #SC
ddply(train.purchase, .(A), summarize, avg=mean(age_oldest))
ddply(train.purchase, .(B), summarize, avg=mean(age_oldest))
ddply(train.purchase, .(C), summarize, avg=mean(age_oldest))
ddply(train.purchase, .(D), summarize, avg=mean(age_oldest))
ddply(train.purchase, .(E), summarize, avg=mean(age_oldest))
ddply(train.purchase, .(F), summarize, avg=mean(age_oldest))
ddply(train.purchase, .(G), summarize, avg=mean(age_oldest))

ddply(train.purchase, .(A), summarize, avg=mean(age_youngest))
ddply(train.purchase, .(B), summarize, avg=mean(age_youngest))
ddply(train.purchase, .(C), summarize, avg=mean(age_youngest))
ddply(train.purchase, .(D), summarize, avg=mean(age_youngest))
ddply(train.purchase, .(E), summarize, avg=mean(age_youngest))
ddply(train.purchase, .(F), summarize, avg=mean(age_youngest))
ddply(train.purchase, .(G), summarize, avg=mean(age_youngest))

ddply(train.purchase, .(A), summarize, avg=mean(car_age))
ddply(train.purchase, .(B), summarize, avg=mean(car_age))
ddply(train.purchase, .(C), summarize, avg=mean(car_age))
ddply(train.purchase, .(D), summarize, avg=mean(car_age))
ddply(train.purchase, .(E), summarize, avg=mean(car_age))
ddply(train.purchase, .(F), summarize, avg=mean(car_age))
ddply(train.purchase, .(G), summarize, avg=mean(car_age))

dcast(train.purchase, age_oldest~A, mean)
histtable = apply(X=array(names(train.purchase)[c(4,8:17)]), MARGIN = 1, FUN = forLoopFunc)
write.csv(rbind.fill(newdf[1]), "c:/histtbl1.csv")
write.csv(rbind.fill(newdf[2]), "c:/histtbl2.csv")
write.csv(rbind.fill(newdf[3]), "c:/histtbl3.csv")
write.csv(rbind.fill(newdf[4]), "c:/histtbl4.csv")
write.csv(rbind.fill(newdf[5]), "c:/histtbl5.csv")
write.csv(rbind.fill(newdf[6]), "c:/histtbl6.csv")
write.csv(rbind.fill(newdf[7]), "c:/histtbl7.csv")
write.csv(rbind.fill(newdf[8]), "c:/histtbl8.csv")
write.csv(rbind.fill(newdf[9]), "c:/histtbl9.csv")
write.csv(rbind.fill(newdf[10]), "c:/histtbl10.csv")
write.csv(rbind.fill(newdf[11]), "c:/histtbl11.csv")

##uniquechar
train.uniquechar = unique(train[c("customer_ID","state", "group_size","homeowner","car_age","car_value","risk_factor","age_oldest",
                                  "age_youngest","married_couple","C_previous","duration_previous")])

#add numeric factors in place of categorical for correlation analysis #SC
#correlation matrix for numeric variables #SC -- AB: had to modify since we changed some from integer to factors
#code works as of 2/4/17 #SC
train_cp = train

state_factor = as.factor(train[,c("state")])
state_ranks <- rank(-table(state_factor), ties.method="first")
train_cp$state_num <- data.frame(category=state_factor, rank=state_ranks[as.character(state_factor)])$rank

car_value_factor = as.factor(train[,c("car_value")])
car_value_ranks <- rank(-table(car_value_factor), ties.method="first")
train_cp$car_value_num <- data.frame(category=car_value_factor, rank=car_value_ranks[as.character(car_value_factor)])$rank

sapply(train_cp, class)
# 2, 8, 10, 12:14, 17, 25, 27, 28
#cormat = cor(train_cp[c(2:4,7:10,12:17,27:28)], use="na.or.complete")
cormat = cor(train_cp[c(2, 8, 10, 12:14, 17, 25, 27, 28)], use="na.or.complete") # AB: This doesn't work either yet.
cormat_table <- as.data.frame(as.table(cormat))
cormat_table <- cormat_table[order(abs(cormat_table$Freq),decreasing = TRUE),]
write.csv(cormat_table, "cormat_table.csv")

#PCA #SC -- AB: This no longer works since 'x' must be numeric
#train.pca <- prcomp(na.omit(train_cp[c(2:4,7:10,12:17,27:28)]),center = TRUE,scale. = TRUE)
#print(train.pca)
#summary(train.pca)
#plot(train.pca, type="l")

#write.csv(train.pca$rotation,"pca.csv")

########### EDA Naive Models ###########
# shopping_pt + day + time + state + location + group_size + homeowner + car_age + car_value + risk_factor + age_oldest + age_youngest + married_couple + C_previous + duration_previous + cost

# Create tree model
#tree.x <- rpart(A ~ shopping_pt + day + location + group_size + homeowner + car_value +
                  #risk_factor + age_oldest + age_youngest + married_couple + C_previous +
                  #duration_previous + cost, data = train, method = "anova", control= rpart.control(maxdepth= 3))
#tree.x # splits on cost and location
#prp(tree.x)
#fancyRpartPlot(tree.x,sub = "") # unreadable

########## END EDA ###########

# Create LDA model--
#model.lda <- lda(A ~ shopping_pt + day + homeowner, data = train)
#+ day + time + state + location + group_size + homeowner + car_age +
#  car_value + risk_factor + age_oldest + age_youngest + married_couple + C_previous +
#  duration_previous + cost

#plot(model.lda, main = "LDA Model", cex = 0.90)

# Use backward subset selection on model.lda--crashes RStudio
#model.lda.bwd <- regsubsets(A ~ shopping_pt + day + state + location + group_size + homeowner + car_age +
#                              car_value + risk_factor + age_oldest + age_youngest + married_couple + C_previous +
#                              duration_previous + cost, data = train, nvmax=5, method="backward")
#summary(model.lda.bwd)

# Create second LDA model using top selected variables
#model.lda2 <- lda(A ~ , data = train)
#plot(model.lda2)


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
## Data manipulation for Model Build ## 
#####################################
#PB

train.purchase.m<-train.purchase

#Adding previous quoted plans to train.purchase.m data frame #PB
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

# study the changes G records
changeG<-(subset(train.purchase.m,train.purchase.m$lastQuoted_G!=train.purchase.m$G))



quoteChange_df<-data.frame(matrix(0,nrow=dim(train.purchase.m[validSubset,])[1],ncol=1))
planOptions<-c("A","B","C","D","E","F","G")
for (ii in 1:7)  {
  quoteChange_df[paste(planOptions[ii],"_change",sep="")] <- as.numeric((get(paste("lastQuoted_",planOptions[ii],sep=""),train.purchase.m[validSubset,]))!=(get(planOptions[ii],train.purchase.m[validSubset,])))
}
quoteChange_df[1]<-NULL
colSums(quoteChange_df,2)
colSums(quoteChange_df,2)/dim(train.purchase.m[validSubset,])[1]

1-mean(train.purchase.m$lastQuoted_G[validSubset]==train.purchase.m$G[validSubset])
#0.1374732


#####################################
## Impute missing values ##
#####################################
#we could use a decision tree to impute missing values. I am using the median to get the models working. Please feel free to change #PB
# Ahmar: I have added code in the Data Quality section to identify relationship using tree and also created new variables
# with name like _imp for 3 variables. If we want to stick with original names of variables then following code will work.
#train.purchase.m$risk_factor[is.na(train.purchase.m$risk_factor)]<-median(train.purchase.m$risk_factor[!is.na(((train.purchase.m$risk_factor)))])
#train.purchase.m$C_previous[is.na(train.purchase.m$C_previous)]<-as.factor((which.max(table(train.purchase.m$C_previous))))
#train.purchase.m$duration_previous[is.na(train.purchase.m$duration_previous)]<-median(train.purchase.m$duration_previous[!is.na(((train.purchase.m$duration_previous)))])

train.purchase.m$risk_factor <- train.purchase.m$risk_factor_imp 
train.purchase.m$C_previous <- train.purchase.m$C_previous_imp
train.purchase.m$duration_previous <- train.purchase.m$duration_previous_imp

summary(train.purchase.m)

#####################################
## Model Build ##
#####################################

##########################################################################
## Option *G* Models ##
##########################################################################


####################
#LDA G 
####################
set.seed(1)
##Initial Full model

ptm <- proc.time() # Start the clock!
model.lda0.g <- lda(G ~ (lastQuoted_G) + risk_factor + car_age + car_value + cost + age_oldest + age_youngest + day + shopping_pt + state +
    Quoted_G_minus2 + Quoted_G_minus3 + Quoted_G_minus4,
                      data = train.purchase.m,
                      subset = trainSubset)
proc.time() - ptm # Stop the clock
#RunTime
#   user  system elapsed 
#   3.17    0.24    3.42 

#classification accuracy for training data
post.train.lda0.g <- predict(object=model.lda0.g, newdata = train.purchase.m[trainSubset,])
plot(model.lda0.g, col = as.integer(train.purchase.m$G[-validSubset]), dimen = 2) #scatterplot with colors
table(post.train.lda0.g$class, train.purchase.m$G[trainSubset]) #confusion matrix
mean(post.train.lda0.g$class==train.purchase.m$G[trainSubset]) #what percent did we predict successfully?
plot(train.purchase.m$G[trainSubset], post.train.lda0.g$class, col=c("blue","red","yellow","green"),main ="Training Set", xlab = "Actual Choice", ylab="Predicted Choice") #how well did we predict trainSubset?

#classification accuracy for validation data
post.valid.lda0.g <- predict(object=model.lda0.g, newdata = train.purchase.m[validSubset,])
plot(model.lda0.g, col = as.integer(train.purchase.m$G[validSubset]), dimen = 2) #scatterplot with colors
table(post.valid.lda0.g$class, train.purchase.m$G[validSubset]) #confusion matrix
mean(post.valid.lda0.g$class==train.purchase.m$G[validSubset]) #what percent did we predict successfully?
plot(train.purchase.m$G[validSubset], post.valid.lda0.g$class, col=c("blue","red","yellow","green"),main ="Validation Set", xlab = "Actual Choice", ylab="Predicted Choice") #how well did we predict validSubset?

###Other less good models mentioned in paper
#Reduced model - only lastQuoted_G
model.lda1.g <- lda(G ~ (lastQuoted_G),
                      data = train.purchase.m,
                      subset = trainSubset)
#Reduced model - everything but lastQuoted_G
model.lda2.g <- lda(G ~ (risk_factor + car_age + car_value + cost + age_oldest + age_youngest + day + shopping_pt + state +
    Quoted_G_minus2 + Quoted_G_minus3 + Quoted_G_minus4),
                      data = train.purchase.m,
                      subset = trainSubset)
#Reduced model - everything but lastQuoted_G and Quoted_G_minus2
model.lda3.g <- lda(G ~ (risk_factor + car_age + car_value + cost + age_oldest + age_youngest + day + shopping_pt + state +
     Quoted_G_minus3 + Quoted_G_minus4),
                      data = train.purchase.m,
                      subset = trainSubset)


####################
# K-Nearest Neighbors -- 2/4/17 FP
####################
### Use this code to create a sample of the training data to fit a model   #PB
n <-dim(train.purchase.m[train.purchase.m$part=="train",])[1] 
# repeatability of results
knn.sample <- sample(n, round(.25*n)) # randomly sample 25% test
train.purchase.m.knn<-train.purchase.m[train.purchase.m$part=="train",][knn.sample,] 
dim(train.purchase.m.knn)

### KNN SAMPLING CODE (need 'train' and 'valid' in part column)   #FP
n <-dim(train.purchase.m)[1] 
knn.sample <- sample(n, round(.25*n)) 
train.purchase.m.knn<-train.purchase.m[knn.sample,] 
dim(train.purchase.m.knn)
View(train.purchase.m.knn)

set.seed(1)
library(class)
dim(train.purchase.m.knn)
table(train.purchase.m.knn$part)
### 24,252 observations | 18,209 train | 6,043 valid

### Define KNN training and test sets
knn.training <- train.purchase.m.knn[train.purchase.m.knn$part=="train", c(8,9,10,12,13,14,15,16,17)]
knn.test <- train.purchase.m.knn[train.purchase.m.knn$part=="valid", c(8,9,10,12,13,14,15,16,17)]
View(knn.training)
knn.trainLabels <- train.purchase.m.knn[train.purchase.m.knn$part=="train", 24]
knn.testLabels <- train.purchase.m.knn[train.purchase.m.knn$part=="valid", 24]

summary(knn.testLabels)
### Building classifier 
knn_pred <- knn(train = knn.training, test = knn.test, cl = knn.trainLabels, k=3)

knn_pred

library(gmodels)
CrossTable(x = knn.testLabels, y = knn_pred, prop.chisq=FALSE)

####################
# RandomForest G
####################
set.seed(1)

ptm <- proc.time() # Start the clock!

model.rf.G <- randomForest(G ~ (lastQuoted_G) + risk_factor + car_age + car_value + cost + age_oldest + age_youngest + day + shopping_pt + state +
    Quoted_G_minus2 + Quoted_G_minus3 + Quoted_G_minus4 ,
  data=train.purchase.m,subset = trainSubset,ntrees=500) 

proc.time() - ptm # Stop the clock

#RunTime
#user  system elapsed 
#730.899   6.797 742.187 

#model summary,Var importance stats and plot
model.rf.G
randomForest::importance(model.rf.G)
randomForest::varImpPlot(model.rf.G)

# Predict random forest on validation set
post.valid.rf.G <- predict(model.rf.G, train.purchase.m[validSubset,]) 
length(post.valid.rf.G)

#Create a simple confusion matrix
table(post.valid.rf.G,train.purchase.m$G[validSubset])

#Check the misclassification rate
error.rf.G <- round(mean(post.valid.rf.G!=train.purchase.m$G[validSubset]),4)
error.rf.G

#Compare against the misclassification rate for the base model 
error.rf.G.base <- round(mean(train.purchase.m$lastQuoted_G[validSubset]!=train.purchase.m$G[validSubset]),4)
error.rf.G.base 

# Fit Metrics
confusionMatrix(post.valid.rf.G,train.purchase.m$G[validSubset],)



####################
# Boosting Model G
####################
set.seed(1)

#### Use this code to tune the GBM model. ###

# library(caret)
# myTuneGrid <- expand.grid(n.trees = 500,interaction.depth = c(6,7),shrinkage = c(.001,.01,.1),n.minobsinnode=10)
# fitControl <- trainControl(method = "repeatedcv", number = 3,repeats = 1, verboseIter = FALSE,returnResamp = "all")
# myModel <- train(G ~ (lastQuoted_G)+ risk_factor + car_age + car_value + cost + age_oldest + age_youngest + day + shopping_pt + state +
#                   Quoted_G_minus2 + Quoted_G_minus3 + Quoted_G_minus4  , 
#                   data=train.purchase.m[trainSubset,],
#                     method = "gbm",
#                     trControl = fitControl,
#                     tuneGrid = myTuneGrid)


ptm <- proc.time() # Start the clock!
set.seed(1)
model.boost.G=gbm(G ~ (lastQuoted_G)+ risk_factor + car_age + car_value + cost + age_oldest + age_youngest + day + shopping_pt + state +
    Quoted_G_minus2 + Quoted_G_minus3 + Quoted_G_minus4  ,
  data=train.purchase.m[trainSubset,],
  distribution="multinomial",
  n.trees=1000,
  interaction.depth=4,
  shrinkage = .01)

proc.time() - ptm # Stop the clock
#RunTime
#user  system elapsed 
#283.950   4.094 302.237

#relative influence statistics & plot.
summary(model.boost.G)
summaryBoost<-summary(model.boost.G)

# Predict GBM on validation set
post.valid.boost.prob.G <- predict(model.boost.G, train.purchase.m[validSubset,],type='response',n.trees=1000) 
post.valid.boost.G<-apply(post.valid.boost.prob.G, 1, which.max)
length(post.valid.boost.G)
head(post.valid.boost.G)

#Create a simple confusion matrix
table(post.valid.boost.G,train.purchase.m$G[validSubset])

#Check the misclassification rate
error.boost.G <- round(mean(post.valid.boost.G!=train.purchase.m$G[validSubset]),4)
error.boost.G 

#Compare against the misclassification rate for the base model 
error.boost.G.base <- round(mean(train.purchase.m$lastQuoted_G[validSubset]!=train.purchase.m$G[validSubset]),4)
error.boost.G.base 

# Fit Metrics
confusionMatrix(post.valid.boost.G,train.purchase.m$G[validSubset],)

#plot relative influence of variables
summaryBoost<-summaryBoost[order(summaryBoost$rel.inf,decreasing=FALSE),]
par(mar=c(3,10,3,3))
barplot(t(summaryBoost$rel.inf),names.arg = summaryBoost$var ,las=2,col="darkblue",main = "Relative Influence",horiz=TRUE)


###################
# SVM G
###################
set.seed(1) 

### Use this code to create a sample of the training data to fit a model   #PB
n <-dim(train.purchase.m[train.purchase.m$part=="train",])[1] 
# repeatability of results
svm.sample <- sample(n, round(.25*n)) # randomly sample 25% test
train.purchase.m.svm<-train.purchase.m[train.purchase.m$part=="train",][svm.sample,] 
dim(train.purchase.m.svm)



# # We can perform cross-validation using tune() to select the best choice of
# # gamma and cost for an SVM with a radial kernel:
# set.seed(1)
# control <- tune.control(nrepeat = 5,cross = 5)
# tune.out = tune(
#   svm,G ~ (lastQuoted_G) + risk_factor + car_age + car_value + cost + age_oldest + age_youngest + day + shopping_pt + state +
#     Quoted_G_minus2 + Quoted_G_minus3 + Quoted_G_minus4  ,
#   data = train.purchase.m.svm,
#   kernel = "linear",
#   ranges = list(cost = c(.01,.1,.5,1),
#     gamma = c(1)),
#   tunecontrol = control
# )
# summary(tune.out)


#Fit a linear SVM Model
ptm <- proc.time() # Start the clock!
svmfit.G=svm(G ~ (lastQuoted_G) + risk_factor + car_age + car_value + cost + age_oldest + age_youngest + day + shopping_pt + state +
    Quoted_G_minus2 + Quoted_G_minus3 + Quoted_G_minus4  ,
  data=train.purchase.m.svm,
  kernel="linear",  
  gamma=.01, 
  cost=1,
  probability =TRUE)
proc.time() - ptm # Stop the clock

#Summary statitics
summary(svmfit.G)

#RunTime
# user  system elapsed 
# 156.099   1.407 158.419 

# Predict SVM on validation set
post.valid.svm.G<-predict(svmfit.G,train.purchase.m[validSubset,])
length(post.valid.svm.G)

#Create a simple confusion matrix
table(post.valid.svm.G,train.purchase.m$G[validSubset])

#Check the misclassification rate
error.svm.G <- round(mean(post.valid.svm.G!=train.purchase.m$G[validSubset]),4)
error.svm.G 
# [1] 0.1363

#Compare against the misclassification rate for the base model 
error.svm.G.base <- round(mean(train.purchase.m$lastQuoted_G[validSubset]!=train.purchase.m$G[validSubset]),4)
error.svm.G.base 

##########################################################################
## Option *A* Models ##
##########################################################################

# Copy training data and remove factor variables with > 53 levels

train.purchase.m.A <- train.purchase.m
train.purchase.m.A$state <- NULL
train.purchase.m.A$location <- NULL
train.purchase.m.A$time <- NULL

# Create RF model
ptm <- proc.time() # Start the clock!
set.seed(1)
model.RF.naive.A <- randomForest(na.omit(A ~ .), data = train.purchase.m.A, ntree =500)
proc.time() - ptm # Stop the clock
#Error in na.fail.default: missing values in object
# Error in na.fail.default(list(G = c(2L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L,  : 
#missing values in object
#Error in randomForest.default(m, y, ...) : 
#  NA/NaN/Inf in foreign function call (arg 1)
#In addition: Warning messages:
#  1: In data.matrix(x) : NAs introduced by coercion
#2: In data.matrix(x) : NAs introduced by coercion
#importance(model.RF.naive.A)
#varImpPlot(model.RF.naive, main = "Random Forest Model: \n Variable Importance")

set.seed(1)
ptm <- proc.time() # Start the clock!
model.rf.A.var <- randomForest(A ~ ., data=train.purchase.m.A ,subset = trainSubset, ntrees=500) 
proc.time() - ptm # Stop the clock


####################
#LDA *A* 
####################
set.seed(1)

##Initial Full model

ptm <- proc.time() # Start the clock!
model.lda0.a <- lda(A ~ (lastQuoted_A) + risk_factor + car_age + car_value + cost + age_oldest + age_youngest + day + shopping_pt + state +
                      Quoted_A_minus2 + Quoted_A_minus3 + Quoted_A_minus4,
                    data = train.purchase.m,
                    subset = trainSubset)
proc.time() - ptm # Stop the clock
#RunTime
#   user  system elapsed 
#   2.768   0.422   3.437

#classification accuracy for training data
post.train.lda0.a <- predict(object=model.lda0.a, newdata = train.purchase.m[trainSubset,])
plot(model.lda0.a, col = as.integer(train.purchase.m$A[-validSubset]), dimen = 2) #scatterplot with colors
table(post.train.lda0.a$class, train.purchase.m$A[trainSubset]) #confusion matrix
#0     1     2
#0 14633   777   276
#1  1116 43070  1828
#2   286   844  9927

mean(post.train.lda0.a$class==train.purchase.m$A[trainSubset]) #what percent did we predict successfully?
#0.9295326
plot(train.purchase.m$A[trainSubset], post.train.lda0.a$class, col=c("blue","red","yellow","green"),main ="Training Set", xlab = "Actual Choice", ylab="Predicted Choice") #how well did we predict trainSubset?

#classification accuracy for validation data
post.valid.lda0.a <- predict(object=model.lda0.a, newdata = train.purchase.m[validSubset,])
plot(model.lda0.a, col = as.integer(train.purchase.m$A[validSubset]), dimen = 2) #scatterplot with colors
table(post.valid.lda0.a$class, train.purchase.m$A[validSubset]) #confusion matrix
#0     1     2
#0  4922   254    94
#1   325 14349   710
#2    96   288  3214

mean(post.valid.lda0.a$class==train.purchase.m$A[validSubset]) #what percent did we predict successfully?
#0.92714
plot(train.purchase.m$A[validSubset], post.valid.lda0.a$class, col=c("blue","red","yellow","green"),main ="Validation Set", xlab = "Actual Choice", ylab="Predicted Choice") #how well did we predict validSubset?


####################
# K-Nearest Neighbors *A*
####################
set.seed(1)
### Use this code to create a sample of the training data to fit a model   #PB
n <-dim(train.purchase.m[train.purchase.m$part=="train",])[1] 
# repeatability of results
knn.sample <- sample(n, round(.25*n)) # randomly sample 25% test
train.purchase.m.knn<-train.purchase.m[train.purchase.m$part=="train",][knn.sample,] 
dim(train.purchase.m.knn)

### KNN SAMPLING CODE (need 'train' and 'valid' in part column)   #FP
n <-dim(train.purchase.m)[1] 
knn.sample <- sample(n, round(.25*n)) 
train.purchase.m.knn<-train.purchase.m[knn.sample,] 
dim(train.purchase.m.knn)
View(train.purchase.m.knn)

set.seed(1)
dim(train.purchase.m.knn)
table(train.purchase.m.knn$part)
### 24,252 observations | 18,209 train | 6,043 valid

### Define KNN training and test sets
knn.training <- train.purchase.m.knn[train.purchase.m.knn$part=="train", c(8,9,10,12,13,14,15,16,17)]
knn.test <- train.purchase.m.knn[train.purchase.m.knn$part=="valid", c(8,9,10,12,13,14,15,16,17)]
View(knn.training)
knn.trainLabels <- train.purchase.m.knn[train.purchase.m.knn$part=="train", c("A")]
knn.testLabels <- train.purchase.m.knn[train.purchase.m.knn$part=="valid", c("A")]

summary(knn.testLabels)

### Building classifier -- can't get this to work
knn_pred <- knn(train = knn.training, test = knn.test, cl = knn.trainLabels, k=3)
#Error in knn(train = knn.training, test = knn.test, cl = knn.trainLabels,  : 
#               k = 0 must be at least 1
#             In addition: Warning message:
#               In knn(train = knn.training, test = knn.test, cl = knn.trainLabels,  :
#                        k = 3 exceeds number 0 of patterns

#knn_pred

CrossTable(x = knn.testLabels, y = knn_pred, prop.chisq=FALSE)


####################
# RandomForest *A*
####################
set.seed(1)
ptm <- proc.time() # Start the clock!
model.rf.A <- randomForest(A ~ (lastQuoted_A) + risk_factor + car_age + car_value + cost + age_oldest + age_youngest + day + shopping_pt + state +
                             Quoted_A_minus2 + Quoted_A_minus3 + Quoted_A_minus4 ,
                           data=train.purchase.m,subset = trainSubset,ntrees=500) 

proc.time() - ptm # Stop the clock
#   user  system elapsed 
#492.976  14.823 560.965

#model summary,Var importance stats and plot
model.rf.A
randomForest::importance(model.rf.A)
randomForest::varImpPlot(model.rf.A)

# Predict random forest on validation set
post.valid.rf.A <- predict(model.rf.A, train.purchase.m[validSubset,]) 
length(post.valid.rf.A)

#Create a simple confusion matrix
table(post.valid.rf.A,train.purchase.m$A[validSubset])
#post.valid.rf.A     0     1     2
#0  4965   240    81
#1   283 14363   723
#2    95   288  3214

#Check the misclassification rate
error.rf.A <- round(mean(post.valid.rf.A!=train.purchase.m$A[validSubset]),4)
error.rf.A #0.0705

#Compare against the misclassification rate for the base model 
error.rf.A.base <- round(mean(train.purchase.m$lastQuoted_A[validSubset]!=train.purchase.m$A[validSubset]),4)
error.rf.A.base #0.0729

# Fit Metrics
confusionMatrix(post.valid.rf.A,train.purchase.m$A[validSubset],)
#Confusion Matrix and Statistics
#Reference
#Prediction     0     1     2
#0  4965   240    81
#1   283 14363   723
#2    95   288  3214

#Overall Statistics

#Accuracy : 0.9295          
#95% CI : (0.9262, 0.9327)
#No Information Rate : 0.614           
#P-Value [Acc > NIR] : < 2.2e-16       

#Kappa : 0.869           
#Mcnemar's Test P-Value : < 2.2e-16       

#Statistics by Class:

#                     Class: 0 Class: 1 Class: 2
#Sensitivity            0.9293   0.9645   0.7999
#Specificity            0.9830   0.8925   0.9811
#Pos Pred Value         0.9393   0.9345   0.8935
#Neg Pred Value         0.9801   0.9406   0.9611
#Prevalence             0.2203   0.6140   0.1657
#Detection Rate         0.2047   0.5922   0.1325
#Detection Prevalence   0.2180   0.6337   0.1483
#Balanced Accuracy      0.9561   0.9285   0.8905


####################
# Boosting Model *A*
####################

ptm <- proc.time() # Start the clock!
set.seed(1)
model.boost.A=gbm(A ~ (lastQuoted_A)+ risk_factor + car_age + car_value + cost + age_oldest + age_youngest + day + shopping_pt + state +
                    Quoted_A_minus2 + Quoted_A_minus3 + Quoted_A_minus4  ,
                  data=train.purchase.m[trainSubset,],
                  distribution="multinomial",
                  n.trees=1000,
                  interaction.depth=4,
                  shrinkage = .01)

proc.time() - ptm # Stop the clock
#user  system elapsed 
#221.621   4.041 245.252

#relative influence statistics & plot.
summary(model.boost.A)
#var      rel.inf
#lastQuoted_A       lastQuoted_A 92.328072714
#Quoted_A_minus2 Quoted_A_minus2  3.032895016
#car_age                 car_age  1.438400995
#cost                       cost  1.113347824
#Quoted_A_minus3 Quoted_A_minus3  0.808758078
#state                     state  0.632935398
#Quoted_A_minus4 Quoted_A_minus4  0.377225706
#age_youngest       age_youngest  0.137980397
#risk_factor         risk_factor  0.064046183
#age_oldest           age_oldest  0.049202355
#shopping_pt         shopping_pt  0.009389194
#car_value             car_value  0.005511336
#day                         day  0.002234804
#summaryBoostA<-summary(model.boost.A)

# Predict GBM on validation set
post.valid.boost.prob.A <- predict(model.boost.A, train.purchase.m[validSubset,],type='response',n.trees=1000) 
post.valid.boost.A<-apply(post.valid.boost.prob.A, 1, which.max)
length(post.valid.boost.A)
head(post.valid.boost.A)

#Create a simple confusion matrix
table(post.valid.boost.A,train.purchase.m$A[validSubset])
#post.valid.boost.A     0     1     2
#1  4997   232    77
#2   264 14369   727
#3    82   290  3214

#Check the misclassification rate
error.boost.A <- round(mean(post.valid.boost.A!=train.purchase.m$A[validSubset]),4)
error.boost.A 
#0.9605

#Compare against the misclassification rate for the base model 
error.boost.A.base <- round(mean(train.purchase.m$lastQuoted_A[validSubset]!=train.purchase.m$A[validSubset]),4)
error.boost.A.base
#0.0729

# Fit Metrics
confusionMatrix(post.valid.boost.A,train.purchase.m$A[validSubset],)
#Error in confusionMatrix.default(post.valid.boost.A, train.purchase.m$A[validSubset],  : 
#The data contain levels not found in the data.

#plot relative influence of variables
summaryBoostA<-summaryBoostA[order(summaryBoostA$rel.inf,decreasing=FALSE),]
par(mar=c(3,10,3,3))
barplot(t(summaryBoostA$rel.inf),names.arg = summaryBoostA$var ,las=2,col="darkblue",main = "Relative Influence",horiz=TRUE)


###################
# SVM *A* -- need to redo--running it with imputed data set messed something up
###################
set.seed(1)
### Use this code to create a sample of the training data to fit a model   #PB
n <-dim(train.purchase.m[train.purchase.m$part=="train",])[1] 
# repeatability of results
svm.sample <- sample(n, round(.25*n)) # randomly sample 25% test
train.purchase.m.svm<-train.purchase.m[train.purchase.m$part=="train",][svm.sample,] 
dim(train.purchase.m.svm)
#0 61


# # We can perform cross-validation using tune() to select the best choice of
# # gamma and cost for an SVM with a radial kernel:
# set.seed(1)
# control <- tune.control(nrepeat = 5,cross = 5)
# tune.out = tune(
#   svm,A ~ (lastQuoted_A) + risk_factor + car_age + car_value + cost + age_oldest + age_youngest + day + shopping_pt + state +
#     Quoted_A_minus2 + Quoted_A_minus3 + Quoted_A_minus4  ,
#   data = train.purchase.m.svm,
#   kernel = "linear",
#   ranges = list(cost = c(.01,.1,.5,1),
#     gamma = c(1)),
#   tunecontrol = control
# )
# summary(tune.out)


#Fit a linear SVM Model
ptm <- proc.time() # Start the clock!
svmfit.A=svm(A ~ (lastQuoted_A) + risk_factor + car_age + car_value + cost + age_oldest + age_youngest + day + shopping_pt + state +
               Quoted_A_minus2 + Quoted_A_minus3 + Quoted_A_minus4  ,
             data=train.purchase.m.svm,
             kernel="linear",  
             gamma=.01, 
             cost=1,
             probability =TRUE)
#Error in if (any(co)) { : missing value where TRUE/FALSE needed
proc.time() - ptm # Stop the clock

#Summary statitics
#summary(svmfit.A)
#Parameters:
#  SVM-Type:  C-classification 
#SVM-Kernel:  linear 
#cost:  1 
#gamma:  0.01 

#Number of Support Vectors:  3430
#( 1722 989 719 )
#Number of Classes:  3 
#Levels: 
#  0 1 2

#RunTime
# user  system elapsed 
# 49.066   0.533  52.545 

# Predict SVM on validation set
post.valid.svm.A<-predict(svmfit.A,train.purchase.m[validSubset,])
length(post.valid.svm.A) #24252

#Create a simple confusion matrix
table(post.valid.svm.A,train.purchase.m$A[validSubset])
#post.valid.svm.A     0     1     2
#0  4909   228    78
#1   361 14380   740
#2    73   283  3200

#Check the misclassification rate
error.svm.A <- round(mean(post.valid.svm.A!=train.purchase.m$A[validSubset]),4)
error.svm.A 
# [1] 0.0727

#Compare against the misclassification rate for the base model 
error.svm.A.base <- round(mean(train.purchase.m$lastQuoted_A[validSubset]!=train.purchase.m$A[validSubset]),4)
error.svm.A.base #0.0729



##########################################################################
## Option *B* Models ##
##########################################################################

####################
#LDA *B* 
####################
set.seed(1)
##Initial Full model

ptm <- proc.time() # Start the clock!
model.lda0.b <- lda(B ~ (lastQuoted_B) + risk_factor + car_age + car_value + cost + age_oldest + age_youngest + day + shopping_pt + state +
                      Quoted_B_minus2 + Quoted_B_minus3 + Quoted_B_minus4,
                    data = train.purchase.m,
                    subset = trainSubset)
proc.time() - ptm # Stop the clock
#RunTime
#user  system elapsed 
#2.36    0.31    3.27 

#classification accuracy for training data
post.train.lda0.b <- predict(object=model.lda0.b, newdata = train.purchase.m[trainSubset,])
plot(model.lda0.b, col = as.integer(train.purchase.m$B[-validSubset]), dimen = 2) #scatterplot with colors
table(post.train.lda0.b$class, train.purchase.m$B[trainSubset]) #confusion matrix
mean(post.train.lda0.b$class==train.purchase.m$B[trainSubset]) #what percent did we predict successfully?
plot(train.purchase.m$B[trainSubset], post.train.lda0.b$class, col=c("blue","red","yellow","green"),main ="Training Set", xlab = "Actual Choice", ylab="Predicted Choice") #how well did we predict trainSubset?

#classification accuracy for validation data
post.valid.lda0.b <- predict(object=model.lda0.b, newdata = train.purchase.m[validSubset,])
plot(model.lda0.b, col = as.integer(train.purchase.m$B[validSubset]), dimen = 2) #scatterplot with colors
table(post.valid.lda0.b$class, train.purchase.m$B[validSubset]) #confusion matrix
mean(post.valid.lda0.b$class==train.purchase.m$B[validSubset]) #what percent did we predict successfully?
plot(train.purchase.m$B[validSubset], post.valid.lda0.b$class, col=c("blue","red","yellow","green"),main ="Validation Set", xlab = "Actual Choice", ylab="Predicted Choice") #how well did we predict validSubset?

####################
# K-Nearest Neighbors *B*
####################
set.seed(1)

### Use this code to create a sample of the training data to fit a model   #PB
n <-dim(train.purchase.m[train.purchase.m$part=="train",])[1] 
# repeatability of results
knn.sample <- sample(n, round(.25*n)) # randomly sample 25% test
train.purchase.m.knn<-train.purchase.m[train.purchase.m$part=="train",][knn.sample,] 
dim(train.purchase.m.knn)

### KNN SAMPLING CODE (need 'train' and 'valid' in part column)   #FP
n <-dim(train.purchase.m)[1] 
knn.sample <- sample(n, round(.25*n)) 
train.purchase.m.knn<-train.purchase.m[knn.sample,] 
dim(train.purchase.m.knn)
View(train.purchase.m.knn)

set.seed(1)
dim(train.purchase.m.knn)
table(train.purchase.m.knn$part)
### 24,252 observations | 18,209 train | 6,043 valid

### Define KNN training and test sets
knn.training <- train.purchase.m.knn[train.purchase.m.knn$part=="train", c(8,9,10,12,13,14,15,16,17)]
knn.test <- train.purchase.m.knn[train.purchase.m.knn$part=="valid", c(8,9,10,12,13,14,15,16,17)]
View(knn.training)
knn.trainLabels <- train.purchase.m.knn[train.purchase.m.knn$part=="train", c("B")]
knn.testLabels <- train.purchase.m.knn[train.purchase.m.knn$part=="valid", c("B")]

summary(knn.testLabels)
### Building classifier 
knn_pred <- knn(train = knn.training, test = knn.test, cl = knn.trainLabels, k=3)

#knn_pred

CrossTable(x = knn.testLabels, y = knn_pred, prop.chisq=FALSE)

####################
# RandomForest *B*
####################
set.seed(1)

ptm <- proc.time() # Start the clock!

model.rf.B <- randomForest(B ~ (lastQuoted_B) + risk_factor + car_age + car_value + cost + age_oldest + age_youngest + day + shopping_pt + state +
                             Quoted_B_minus2 + Quoted_B_minus3 + Quoted_B_minus4 ,
                           data=train.purchase.m,subset = trainSubset,ntrees=500) 

proc.time() - ptm # Stop the clock

#RunTime
#user  system elapsed 
#114.52    0.93  116.36 

#model summary,Var importance stats and plot
model.rf.B
randomForest::importance(model.rf.B)
randomForest::varImpPlot(model.rf.B)

# Predict random forest on validation set
post.valid.rf.B <- predict(model.rf.B, train.purchase.m[validSubset,]) 
length(post.valid.rf.B)
#str(post.valid.rf.B)
#str(train.purchase.m$B[validSubset])
#Create a simple confusion matrix
table(post.valid.rf.B,train.purchase.m$B[validSubset])

#Check the misclassification rate
error.rf.B <- round(mean(post.valid.rf.B!=train.purchase.m$B[validSubset]),4)
error.rf.B

#Compare against the misclassification rate for the base model 
error.rf.B.base <- round(mean(train.purchase.m$lastQuoted_B[validSubset]!=train.purchase.m$B[validSubset]),4)
error.rf.B.base 

# Fit Metrics
confusionMatrix(post.valid.rf.B,train.purchase.m$B[validSubset],)


####################
# Boosting Model *B*
####################
set.seed(1)

ptm <- proc.time() # Start the clock!

model.boost.B=gbm(B ~ (lastQuoted_B)+ risk_factor + car_age + car_value + cost + age_oldest + age_youngest + day + shopping_pt + state +
                    Quoted_B_minus2 + Quoted_B_minus3 + Quoted_B_minus4  ,
                  data=train.purchase.m[trainSubset,],
                  distribution="multinomial",
                  n.trees=1000,
                  interaction.depth=2,
                  shrinkage = .01)

proc.time() - ptm # Stop the clock
#RunTime
#user  system elapsed 
#151.54    0.47  153.98

#relative influence statistics & plot.
summary(model.boost.B)
summaryBoost<-summary(model.boost.B)

# Predict GBM on validation set
post.valid.boost.prob.B <- predict(model.boost.B, train.purchase.m[validSubset,],type='response',n.trees=1000) 
post.valid.boost.B<-apply(post.valid.boost.prob.B, 1, which.max) - 1
length(post.valid.boost.B)
head(post.valid.boost.B)

#Create a simple confusion matrix
table(post.valid.boost.B,train.purchase.m$B[validSubset])

#Check the misclassification rate
error.boost.B <- round(mean(post.valid.boost.B!=train.purchase.m$B[validSubset]),4)
error.boost.B 

#Compare against the misclassification rate for the base model 
error.boost.B.base <- round(mean(train.purchase.m$lastQuoted_B[validSubset]!=train.purchase.m$B[validSubset]),4)
error.boost.B.base 

# Fit Metrics
confusionMatrix(post.valid.boost.B,train.purchase.m$B[validSubset])

#plot relative influence of variables
summaryBoost<-summaryBoost[order(summaryBoost$rel.inf,decreasing=FALSE),]
par(mar=c(3,10,3,3))
barplot(t(summaryBoost$rel.inf),names.arg = summaryBoost$var ,las=2,col="darkblue",main = "Relative Influence",horiz=TRUE)


###################
# SVM *B*
###################
set.seed(1)

### Use this code to create a sample of the training data to fit a model   #PB
n <-dim(train.purchase.m[train.purchase.m$part=="train",])[1] 
# repeatability of results
svm.sample <- sample(n, round(.25*n)) # randomly sample 25% test
train.purchase.m.svm<-train.purchase.m[train.purchase.m$part=="train",][svm.sample,] 
dim(train.purchase.m.svm)

# # We can perform cross-validation using tune() to select the best choice of
# # gamma and cost for an SVM with a radial kernel:
# set.seed(1)
# control <- tune.control(nrepeat = 5,cross = 5)
# tune.out = tune(
#   svm,G ~ (lastQuoted_G) + risk_factor + car_age + car_value + cost + age_oldest + age_youngest + day + shopping_pt + state +
#     Quoted_G_minus2 + Quoted_G_minus3 + Quoted_G_minus4  ,
#   data = train.purchase.m.svm,
#   kernel = "linear",
#   ranges = list(cost = c(.01,.1,.5,1),
#     gamma = c(1)),
#   tunecontrol = control
# )
# summary(tune.out)


#Fit a linear SVM Model
ptm <- proc.time() # Start the clock!
svmfit.B=svm(B ~ (lastQuoted_B) + risk_factor + car_age + car_value + cost + age_oldest + age_youngest + day + shopping_pt + state +
               Quoted_B_minus2 + Quoted_B_minus3 + Quoted_B_minus4  ,
             data=train.purchase.m.svm,
             kernel="linear",  
             gamma=.01, 
             cost=1,
             probability =TRUE)
proc.time() - ptm # Stop the clock

#Summary statitics
summary(svmfit.B)

#RunTime
# user  system elapsed 
# 156.099   1.407 158.419 

# Predict SVM on validation set
post.valid.svm.B<-predict(svmfit.B,train.purchase.m[validSubset,])
length(post.valid.svm.B)

#Create a simple confusion matrix
table(post.valid.svm.B,train.purchase.m$B[validSubset])

#Check the misclassification rate
error.svm.B <- round(mean(post.valid.svm.B!=train.purchase.m$B[validSubset]),4)
error.svm.B 
# [1] 0.1363

#Compare against the misclassification rate for the base model 
error.svm.B.base <- round(mean(train.purchase.m$lastQuoted_B[validSubset]!=train.purchase.m$B[validSubset]),4)
error.svm.B.base 


##########################################################################
## Option *C* Models ##
##########################################################################
set.seed(1)

####################
#LDA *C* 
####################
####################
#LDA *C* 
####################
set.seed(1)
##Initial Full model

ptm <- proc.time() # Start the clock!
model.lda.C <- lda(C ~ (lastQuoted_C) + risk_factor_imp + car_age + car_value + cost + age_oldest + age_youngest + day + shopping_pt + state +
                      Quoted_C_minus2 + Quoted_C_minus3 + Quoted_C_minus4 +
                      C_previous_imp + duration_previous_imp,
                    data = train.purchase.m,
                    subset = trainSubset)
proc.time() - ptm # Stop the clock
#RunTime
#user  system elapsed 
#1.89    0.28    3.50 

#classification accuracy for training data
post.train.lda.C <- predict(object=model.lda.C, newdata = train.purchase.m[trainSubset,])
plot(model.lda.C, col = as.integer(train.purchase.m$C[-validSubset]), dimen = 2) #scatterplot with colors
table(post.train.lda.C$class, train.purchase.m$C[trainSubset]) #confusion matrix
mean(post.train.lda.C$class==train.purchase.m$C[trainSubset]) #what percent did we predict successfully?
plot(train.purchase.m$C[trainSubset], post.train.lda.C$class, col=c("blue","red","yellow","green"),main ="Training Set", xlab = "Actual Choice", ylab="Predicted Choice") #how well did we predict trainSubset?

#classification accuracy for validation data
post.valid.lda.C <- predict(object=model.lda.C, newdata = train.purchase.m[validSubset,])
plot(model.lda.C, col = as.integer(train.purchase.m$C[validSubset]), dimen = 2) #scatterplot with colors
table(post.valid.lda.C$class, train.purchase.m$C[validSubset]) #confusion matrix
mean(post.valid.lda.C$class==train.purchase.m$C[validSubset]) #what percent did we predict successfully?
plot(train.purchase.m$C[validSubset], post.valid.lda.C$class, col=c("blue","red","yellow","green"),main ="Validation Set", xlab = "Actual Choice", ylab="Predicted Choice") #how well did we predict validSubset?


confusionMatrix(post.valid.lda.C$class,train.purchase.m$C[validSubset])

#modelCompare.df <- My.ModelCompare("LDA", "C", post.valid.lda.C$class,train.purchase.m$C[validSubset]);modelCompare.df 

#Check the misclassification rate
error.lda.C <- round(mean(post.valid.lda.C$class!=train.purchase.m$C[validSubset]),4)
error.lda.C 
# 0.069

#Compare against the misclassification rate for the base model 
error.lda.C.base <- round(mean(train.purchase.m$lastQuoted_C[validSubset]!=train.purchase.m$C[validSubset]),4)
error.lda.C.base 
# 0.069

confusionMatrix(post.valid.lda.C$class,train.purchase.m$C[validSubset])
# Kappa 0.9018    


####################
# K-Nearest Neighbors *C*
####################
set.seed(1)

####################
# RandomForest *C*
####################
set.seed(1)

####################
# Boosting Model *C*
####################
set.seed(1)

###################
# SVM *C*
###################
set.seed(1)



##########################################################################
## Option *D* Models ##
##########################################################################

####################
#LDA *D* 
####################
set.seed(1)
##Initial Full model

ptm <- proc.time() # Start the clock!
model.lda.D <- lda(D ~ (lastQuoted_D) + risk_factor_imp + car_age + car_value + cost + age_oldest + age_youngest + day + shopping_pt + state +
    Quoted_D_minus2 + Quoted_D_minus3 + Quoted_D_minus4 + C_previous_imp + duration_previous_imp  + group_size + homeowner + married_couple,
  data = train.purchase.m,
  subset = trainSubset)
proc.time() - ptm # Stop the clock
# #RunTime
# user  system elapsed 
# 8.056   1.232  10.878 

#classification accuracy for training data
post.train.lda.D <- predict(object=model.lda.D, newdata = train.purchase.m[trainSubset,])
# plot(model.lda.D, col = as.integer(train.purchase.m$D[-validSubset]), dimen = 2) #scatterplot with colors
table(post.train.lda.D$class, train.purchase.m$D[trainSubset]) #confusion matrix
mean(post.train.lda.D$class!=train.purchase.m$D[trainSubset]) #what percent did we predict successfully?
# plot(train.purchase.m$D[trainSubset], post.train.lda.D$class, col=c("blue","red","yellow","green"),main ="Training Set", xlab = "Actual Choice", ylab="Predicted Choice") #how well did we predict trainSubset?

#classification accuracy for validation data
post.valid.lda.D <- predict(object=model.lda.D, newdata = train.purchase.m[validSubset,])$class
length(post.valid.lda.D)
# plot(model.lda.D, col = as.integer(train.purchase.m$D[validSubset]), dimen = 2) #scatterplot with colors
table(post.valid.lda.D, train.purchase.m$D[validSubset]) #confusion matrix
# plot(train.purchase.m$D[validSubset], post.valid.lda.D$class, col=c("blue","red","yellow","green"),main ="Validation Set", xlab = "Actual Choice", ylab="Predicted Choice") #how well did we predict validSubset?

#Check the misclassification rate
error.lda.D <- round(mean(post.valid.lda.D!=train.purchase.m$D[validSubset]),4)
error.lda.D 
# 0.0484

#Compare against the misclassification rate for the base model 
error.lda.D.base <- round(mean(train.purchase.m$lastQuoted_D[validSubset]!=train.purchase.m$D[validSubset]),4)
error.lda.D.base 
# 0.0484

confusionMatrix(post.valid.lda.D,train.purchase.m$D[validSubset],)
# Kappa 0.9074    

####################
# K-Nearest Neighbors *D*
####################
set.seed(1)
### Use this code to create a sample of the training data to fit a model   #PB
n <-dim(train.purchase.m[train.purchase.m$part=="train",])[1] 
# repeatability of results
knn.sample <- sample(n, round(.25*n)) # randomly sample 25% test
train.purchase.m.knn<-train.purchase.m[train.purchase.m$part=="train",][knn.sample,] 
dim(train.purchase.m.knn)
library(class)

ctrl <- trainControl(method="repeatedcv",repeats = 1) #,classProbs=TRUE,summaryFunction = twoClassSummary)
knnFit <- train(D ~ (lastQuoted_D) + risk_factor_imp + car_age + car_value + cost + age_oldest + age_youngest + day + shopping_pt + state +
    Quoted_D_minus2 + Quoted_D_minus3 + Quoted_D_minus4 + C_previous_imp + duration_previous_imp  + group_size + homeowner + married_couple
  , data = train.purchase.m.knn, method = "knn", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 5)


# Resampling results across tuning parameters:
#   
#   k   Accuracy   Kappa      Accuracy SD  Kappa SD   
# 5  0.9207772  0.8450820  0.005632213  0.011662121
# 7  0.9225365  0.8484299  0.007032948  0.014358392
# 9  0.9216569  0.8463100  0.005811135  0.011981198
# 11  0.9205566  0.8437952  0.004690705  0.009812055
# 13  0.9205023  0.8434500  0.005354910  0.011164977
# 
# Accuracy was used to select the optimal model using  the largest value.
# The final value used for the model was k = 7. 

knnPredict <- predict(knnFit,newdata = train.purchase.m[validSubset,])



knn.trainLabels <- train.purchase.m.knn[,c('D')]
knn.testLabels <- train.purchase.m[validSubset,c('D')]

summary(knn.testLabels)
### Building classifier 
knn_pred <- knn(train = knn.training, test = knn.test, cl = knn.trainLabels, k=3)
knn_pred

library(gmodels)
CrossTable(x = knn.testLabels, y = knn_pred, prop.chisq=FALSE)

####################
# RandomForest *D*
####################
set.seed(1)

ptm <- proc.time() # Start the clock!

model.rf.D <- randomForest(D ~ (lastQuoted_D) + risk_factor_imp + car_age + car_value + cost + age_oldest + age_youngest + day + shopping_pt + state +
    Quoted_D_minus2 + Quoted_D_minus3 + Quoted_D_minus4 + C_previous_imp + duration_previous_imp  + group_size + homeowner + married_couple,
  data=train.purchase.m,subset = trainSubset,ntrees=500) 

proc.time() - ptm # Stop the clock

#RunTime
#user  system elapsed 
#730.899   6.797 742.187 

#model summary,Var importance stats and plot
model.rf.D
randomForest::importance(model.rf.D)
randomForest::varImpPlot(model.rf.D)

post.train.rf.D <- predict(model.rf.D, train.purchase.m[trainSubset,]) 

# Predict random forest on validation set
post.valid.rf.D <- predict(model.rf.D, train.purchase.m[validSubset,]) 
length(post.valid.rf.D)

#Create a simple confusion matrix
table(post.valid.rf.D,train.purchase.m$D[validSubset])
# post.valid.rf.D     1     2     3
# 1  2977   186   204
# 2    94  5042   407
# 3    75   198 15069


#Check the misclassification rate
error.train.rf.D <- round(mean(post.train.rf.D!=train.purchase.m$D[trainSubset]),4)
error.train.rf.D
# 0.0463


#Check the misclassification rate
error.rf.D <- round(mean(post.valid.rf.D!=train.purchase.m$D[validSubset]),4)
error.rf.D
# 0.048

#Compare against the misclassification rate for the base model 
error.rf.D.base <- round(mean(train.purchase.m$lastQuoted_D[validSubset]!=train.purchase.m$D[validSubset]),4)
error.rf.D.base 
# 0.0484

# Fit Metrics
confusionMatrix(post.valid.rf.D,train.purchase.m$D[validSubset],)
# 
# Accuracy : 0.952
# Kappa : 0.908    


# Class: 1 Class: 2 Class: 3
# Sensitivity            0.9463   0.9292   0.9610
# Specificity            0.9815   0.9734   0.9682
# Pos Pred Value         0.8842   0.9096   0.9822
# Neg Pred Value         0.9919   0.9795   0.9314
# Prevalence             0.1297   0.2237   0.6465
# Detection Rate         0.1228   0.2079   0.6214
# Detection Prevalence   0.1388   0.2286   0.6326
# Balanced Accuracy      0.9639   0.9513   0.9646

####################
# Boosting Model *D*
####################
set.seed(1)

#### Use this code to tune the GBM model. ###

# library(caret)
# myTuneGrid <- expand.grid(n.trees = 500,interaction.depth = c(2,3),shrinkage = c(01),n.minobsinnode=10)
# fitControl <- trainControl(method = "repeatedcv", number = 3,repeats = 1, verboseIter = FALSE,returnResamp = "all")
# myModel <- train(D ~ (lastQuoted_D) + risk_factor_imp + car_age + car_value + cost + age_oldest + age_youngest + day + shopping_pt + state +
# Quoted_D_minus2 + Quoted_D_minus3 + Quoted_D_minus4 + C_previous_imp + duration_previous_imp  + group_size + homeowner + married_couple,
#                   data=train.purchase.m[trainSubset,],
#                     method = "gbm",
#                     trControl = fitControl,
#                     tuneGrid = myTuneGrid)
# 

ptm <- proc.time() # Start the clock!
set.seed(1)
model.boost.D=gbm(D ~ (lastQuoted_D) + risk_factor_imp + car_age + car_value + cost + age_oldest + age_youngest + day + shopping_pt + state +
    Quoted_D_minus2 + Quoted_D_minus3 + Quoted_D_minus4 + C_previous_imp + duration_previous_imp  + group_size + homeowner + married_couple,
  data=train.purchase.m[trainSubset,],
  distribution="multinomial",
  n.trees=1000,
  interaction.depth=2,
  shrinkage = .01)

proc.time() - ptm # Stop the clock
#RunTime
#user  system elapsed 
#283.950   4.094 302.237

#relative influence statistics & plot.
summary(model.boost.D)
summaryBoost<-summary(model.boost.D)

# Predict DBM on validation set
post.train.boost.prob.D <- predict(model.boost.D, train.purchase.m[trainSubset,],type='response',n.trees=1000) 
post.train.boost.D<-apply(post.train.boost.prob.D, 1, which.max)

post.valid.boost.prob.D <- predict(model.boost.D, train.purchase.m[validSubset,],type='response',n.trees=1000) 
post.valid.boost.D<-apply(post.valid.boost.prob.D, 1, which.max)
length(post.valid.boost.D)
head(post.valid.boost.D)

#Create a simple confusion matrix
table(post.valid.boost.D,train.purchase.m$D[validSubset])

#Compare against the misclassification rate for the base model 
error.train.boost.D.base <- round(mean(train.purchase.m$lastQuoted_D[trainSubset]!=train.purchase.m$D[trainSubset]),4)
error.train.boost.D.base 
#0.0516

train.error.boost.D <- round(mean(post.train.boost.D!=train.purchase.m$D[trainSubset]),4)
train.error.boost.D 
#0.0512

#Check the misclassification rate
error.boost.D <- round(mean(post.valid.boost.D!=train.purchase.m$D[validSubset]),4)
error.boost.D 
# 0.0474

#Compare against the misclassification rate for the base model 
error.boost.D.base <- round(mean(train.purchase.m$lastQuoted_D[validSubset]!=train.purchase.m$D[validSubset]),4)
error.boost.D.base 
# 0.0484

# Fit Metrics
confusionMatrix(post.valid.boost.D,train.purchase.m$D[validSubset],)

#plot relative influence of variables
summaryBoost<-summaryBoost[order(summaryBoost$rel.inf,decreasing=FALSE),]
par(mar=c(3,10,3,3))
barplot(t(summaryBoost$rel.inf),names.arg = summaryBoost$var ,las=2,col="darkblue",main = "Relative Influence",horiz=TRUE)
# 1  2985   182   202
# 2    94  5043   404
# 3    67   201 15074
# Accuracy : 0.9526       
# Kappa : 0.9091   

###################
# SVM *D*
###################
set.seed(1)

### Use this code to create a sample of the training data to fit a model   #PB
n <-dim(train.purchase.m[train.purchase.m$part=="train",])[1] 
# repeatability of results
svm.sample <- sample(n, round(.25*n)) # randomly sample 25% test
train.purchase.m.svm<-train.purchase.m[train.purchase.m$part=="train",][svm.sample,] 
dim(train.purchase.m.svm)



# # We can perform cross-validation using tune() to select the best choice of
# # gamma and cost for an SVM with a linear kernel:
# set.seed(1)
# control <- tune.control(nrepeat = 1,cross = 5)
# tune.out = tune(
#   svm,D ~ (lastQuoted_D) + risk_factor_imp + car_age + car_value + cost + age_oldest + age_youngest + day + shopping_pt + state +
#     Quoted_D_minus2 + Quoted_D_minus3 + Quoted_D_minus4 + C_previous_imp + duration_previous_imp  + group_size,
#   data = train.purchase.m.svm,
#   kernel = "linear",
#   ranges = list(cost = c(.01,.1,.5,1),
#     gamma = c(.01,.1,1)),
#   tunecontrol = control
# )
# summary(tune.out)


#Fit a linear SVM Model
ptm <- proc.time() # Start the clock!
svmfit.D=svm(D ~ (lastQuoted_D) + risk_factor_imp + car_age + car_value + cost + age_oldest + age_youngest + day + shopping_pt + state +
    Quoted_D_minus2 + Quoted_D_minus3 + Quoted_D_minus4 + C_previous_imp + duration_previous_imp  + group_size,
  data=train.purchase.m.svm,
  kernel="linear",  
  gamma=.01, 
  cost=1,
  probability =TRUE)
proc.time() - ptm # Stop the clock

#Summary statitics
summary(svmfit.D)

#RunTime
# user  system elapsed 
# 156.099   1.407 158.419 

# Predict SVM on validation set
post.train.svm.D<-predict(svmfit.D,train.purchase.m[trainSubset,])
post.valid.svm.D<-predict(svmfit.D,train.purchase.m[validSubset,])
length(post.valid.svm.D)

#Create a simple confusion matrix
table(post.valid.svm.D,train.purchase.m$D[validSubset])
# post.valid.svm.D     1     2     3
# 1  2987   183   224
# 2    94  5049   408
# 3    65   194 15048
#Check the misclassification rate

#Compare against the misclassification rate for the base model 
error.train.svm.D.base <- round(mean(train.purchase.m$lastQuoted_D[trainSubset]!=train.purchase.m$D[trainSubset]),4)
error.train.svm.D.base 
# 0.0516

train.error.svm.D <- round(mean(post.train.svm.D!=train.purchase.m$D[trainSubset]),4)
train.error.svm.D 
# 0.0515

error.svm.D <- round(mean(post.valid.svm.D!=train.purchase.m$D[validSubset]),4)
error.svm.D 
# 0.0482

#Compare against the misclassification rate for the base model 
error.svm.D.base <- round(mean(train.purchase.m$lastQuoted_D[validSubset]!=train.purchase.m$D[validSubset]),4)
error.svm.D.base 
# 0.0484

confusionMatrix(post.valid.svm.D,train.purchase.m$D[validSubset])
# Accuracy : 0.9518  
# Kappa : 0.9078   

##########################################################################
## Option *E* Models ##
##########################################################################
set.seed(1)

####################
#LDA *E* 
####################
set.seed(1)

##Initial Full model

model.lda0.e <- lda(E ~ (lastQuoted_E) + risk_factor + car_age + car_value + cost + age_oldest + age_youngest + day + shopping_pt + state +
                      Quoted_E_minus2 + Quoted_E_minus3 + Quoted_E_minus4,
                    data = train.purchase.m,
                    subset = trainSubset)

#classification accuracy for training data
post.train.lda0.e <- predict(object=model.lda0.e, newdata = train.purchase.m[trainSubset,])
plot(model.lda0.e, col = as.integer(train.purchase.m$E[-validSubset]), dimen = 2) #scatterplot with colors
table(post.train.lda0.e$class, train.purchase.m$E[trainSubset]) #confusion matrix
#     0     1    Total
#0 37213   2681  39894
#1  1866  30997  32863

mean(post.train.lda0.e$class==train.purchase.m$E[trainSubset]) #what percent did we predict successfully?
#0.9375043
plot(train.purchase.m$E[trainSubset], post.train.lda0.e$class, col=c("blue","red"),main ="Training Set", xlab = "Actual Choice", ylab="Predicted Choice") #how well did we predict trainSubset?

#classification accuracy for validation data
post.valid.lda0.e <- predict(object=model.lda0.e, newdata = train.purchase.m[validSubset,])
plot(model.lda0.e, col = as.integer(train.purchase.m$E[validSubset]), dimen = 2) #scatterplot with colors
table(post.valid.lda0.e$class, train.purchase.m$E[validSubset]) #confusion matrix
#     0     1
#0  12433   900   13333
#1    620 10299   10919


mean(post.valid.lda0.e$class==train.purchase.m$E[validSubset]) #what percent did we predict successfully?
#0.9373248
plot(train.purchase.m$E[validSubset], post.valid.lda0.e$class, col=c("blue","red"),main ="Validation Set", xlab = "Actual Choice", ylab="Predicted Choice") #how well did we predict validSubset?



####################
# RandomForest *E*
####################
set.seed(1)

model.rf.E <- randomForest(E ~ (lastQuoted_E) + risk_factor + car_age + car_value + cost + age_oldest + age_youngest + day + shopping_pt + state +
                             Quoted_E_minus2 + Quoted_E_minus3 + Quoted_E_minus4 ,
                           data=train.purchase.m,subset = trainSubset,ntrees=500) 

#model summary,Var importance stats and plot
model.rf.E
randomForest::importance(model.rf.E)
randomForest::varImpPlot(model.rf.E)

# Predict random forest on validation set
post.valid.rf.E <- predict(model.rf.E, train.purchase.m[validSubset,]) 
length(post.valid.rf.E)

#Create a simple confusion matrix
table(post.valid.rf.E,train.purchase.m$E[validSubset])
#post.valid.rf.E     0     1     
#0  12518   1175    
#1    535  10024

#Check the misclassification rate
error.rf.E <- round(mean(post.valid.rf.E!=train.purchase.m$E[validSubset]),4)
error.rf.E #0.0705

#Compare against the misclassification rate for the base model 
error.rf.E.base <- round(mean(train.purchase.m$lastQuoted_E[validSubset]!=train.purchase.m$E[validSubset]),4)
error.rf.E.base #0.0627

# Error: could not find function "confusionMatrix"
confusionMatrix(post.valid.rf.E,train.purchase.m$E[validSubset],)

summary(model.rf.E)





####################
# Boosting Model *E*
####################

set.seed(1)
model.boost.E=gbm(E ~ (lastQuoted_E)+ risk_factor + car_age + car_value + cost + age_oldest + age_youngest + day + shopping_pt + state +
                    Quoted_E_minus2 + Quoted_E_minus3 + Quoted_E_minus4  ,
                  data=train.purchase.m[trainSubset,],
                  distribution="multinomial",
                  n.trees=1000,
                  interaction.depth=4,
                  shrinkage = .01)

#relative influence statistics & plot.
summary(model.boost.E)
#var      rel.inf
#lastQuoted_E       lastQuoted_E 95.401047796
#Quoted_E_minus2 Quoted_E_minus2  1.400669139
#cost                       cost  0.794831135
#car_age                 car_age  0.753168589
#state                     state  0.460904932
#Quoted_E_minus3 Quoted_E_minus3  0.400308785
#age_youngest       age_youngest  0.266875249
#Quoted_E_minus4 Quoted_E_minus4  0.234940325
#age_oldest           age_oldest  0.123565662
#shopping_pt         shopping_pt  0.091063534
#risk_factor         risk_factor  0.044803514
#car_value             car_value  0.020386805
#day                         day  0.007434535
summaryBoostE<-summary(model.boost.E)

# Predict GBM on validation set
post.valid.boost.prob.E <- predict(model.boost.E, train.purchase.m[validSubset,],type='response',n.trees=1000) 
post.valid.boost.E<-apply(post.valid.boost.prob.E, 1, which.max)
length(post.valid.boost.E)
head(post.valid.boost.E)

#Create a simple confusion matrix
table(post.valid.boost.E,train.purchase.m$E[validSubset])
#post.valid.boost.A     0     1     
#0  12458   906   13364
#1    595 10293   10888


#Check the misclassification rate
error.boost.E <- round(mean(post.valid.boost.E!=train.purchase.m$E[validSubset]),4)
error.boost.E 
#0.9626

#Compare against the misclassification rate for the base model 
error.boost.E.base <- round(mean(train.purchase.m$lastQuoted_E[validSubset]!=train.purchase.m$E[validSubset]),4)
error.boost.E.base
#0.0627

# Fit Metrics
# Error: could not find function "confusionMatrix"
confusionMatrix(post.valid.boost.E,train.purchase.m$E[validSubset],)

#plot relative influence of variables
summaryBoostE<-summaryBoostE[order(summaryBoostE$rel.inf,decreasing=FALSE),]
par(mar=c(3,10,3,3))
barplot(t(summaryBoostE$rel.inf),names.arg = summaryBoostE$var ,las=2,col="darkblue",main = "Relative Influence",horiz=TRUE)




##########################################################################
## Option *F* Models ##
##########################################################################
set.seed(1)

####################
#LDA *F* 
####################
set.seed(1)

####################
# K-Nearest Neighbors *F*
####################
set.seed(1)

####################
# RandomForest *F*
####################
set.seed(1)

####################
# Boosting Model *F*
####################
set.seed(1)

###################
# SVM *F*
###################
set.seed(1)





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
#Used decision tree to impute missing values for risk_factor.
test.m$risk_factor <- test.m$risk_factor_imp
test.m$C_previous <- test.m$C_previous_imp
test.m$duration_previous <- test.m$duration_previous_imp
test.m$location <- test.m$location_imp




#################
# PREDICT G on test set
#################

#Predict G on the test set using Random Forest #PB  0.53955
predictG <- predict(model.rf.G, test.m, type = "class")
#Predict G on the test set using Boosted Tree  #PB   0.54045
predictG <-predict(model.boost.G, test.m,type = 'response',n.trees = 1000)
predictG <- apply(predictG, 1, which.max)

predictResultsG<-data.frame(test.m$customer_ID,predictG)
test.m$predictG<-predictG

# hard coded rules
#all resdints in Florida should select 3 or 4, predictions of 2 change to 3, could use decision tree for this #PB
predictResults$predictG[test.m$state=="FL" & test.m$predictG=="2"]<-as.factor("3")
#add option F - NY and CT rules

predict_Submit<-merge(x=lastQuoteSubmit,y=predictResultsG,by.x=c("customer_ID"),by.y=c("test.m.customer_ID"))
head(predict_Submit)





#################
# PREDICT F on test set
#################

#Predict F on the test set using Random Forest #PB  
# predictF <- predict(model.rf.F, test.m, type = "class")
#Predict F on the test set using Boosted Tree  #PB   
predictF <-predict(model.boost.F, test.m,type = 'response',n.trees = 1000)
predictF <- apply(predictF, 1, which.max)-1

predictResultsF<-data.frame(test.m$customer_ID,predictF)

# hard coded rules
#add option F - NY and CT rules
predict_Submit<-merge(x=predict_Submit,y=predictResultsF,by.x=c("customer_ID"),by.y=c("test.m.customer_ID"))
head(predict_Submit)

#replacing last quoted G with predicted G #PB 
predict_Submit$predict.plan<-predictG_Submit$plan
predict_Submit$predict.plan<-paste(substring(predict_Submit$predict.plan,first=1,last=6),predict_Submit$predictG,sep="")
head(predict_Submit)

# #replacing last quoted F with predicted F #PB 
# predict_Submit$predict.plan<-paste(substring(predict_Submit$predict.plan,first=1,last=5),predict_Submit$predictF,substring(predict_Submit$predict.plan,first=7,last=7),sep="")
# head(predict_Submit)


#view sample before exporting to csv #PB  
predict_Submit_Final<-(predict_Submit[c('customer_ID','predict.plan')])
names(predict_Submit_Final)<- c('customer_ID','plan')
head(predict_Submit_Final)
write.csv(predict_Submit_Final, file=file.path(path,"submit_GBM_FG_2.csv"), row.names=FALSE, quote=FALSE)
