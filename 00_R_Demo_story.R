##############################################
### 
###  Budapest BI Forum 2016 (26.10.2016)
###
###  Detecting outliers and 
###  fraud(lent) transaction using R
###
###  Author: Toma� Ka�trun  
###  Date: 20.October, 2016
###
################################################

library(e1071)
library(RODBC)
library(dplyr)
library(neuralnet)
library(MASS)
library(RgoogleMaps)
library(EMCluster)
library(ggplot2)
library(fpc)
library(ROCR)
library(lubridate)
library(randomForest)
library(caTools)



setwd("C:\\Users\\SI01017988\\Documents\\06-SQL\\3-2016\\16-BudapestBI Forum 2016 (26.10.2016)")

# Get Data from MS SQL Server to R Environment
myconn <-odbcDriverConnect("driver={SQL Server};Server=SICN-KASTRUN;database=Fraud_detection;trusted_connection=true")

Visa <- sqlQuery(myconn, "SELECT * FROM Visa_Transactions")

# Exclude All type of income!
Maestro <- sqlQuery(myconn, "SELECT Invoice, Value, Currency, Description, Purchase_day, Country, City, LAT, LNT FROM Maestro_Transactions WHERE Value > 0")

# All transactions in one (Visa + Maestro)
All <- sqlQuery(myconn, "SELECT * FROM All_Transactions")

close(myconn) 

#drop connection
rm(myconn)

#subsetting all_analysis for stats needed
All_analysis <- All[,c(1:3,5:6,10:12)]


######################
#
# 0.  Preps
#
######################

# general info
head(Visa, n = 2)
str(Visa)
summary(Visa)

#quick analyse of description; some of the rules (maybe for white-lists can be found here as well)
table((Visa$Point_of_sale))

# Transactions as: WWW.ALIEXPRESS.COM, UBER*UBER, Amazon DE Marketplac - can be trusted!
# Transactions as: LETNA �LANARINA - additional logic needed; these are maintainance transactions (year subscription for VISA)
# Transactions as: LESNINA PE BRDO, INTERSPAR CITYPARK L,ADRIA AIRWAYS D.D., SONCEK - can be trusted; are verified companies!

######################
#
# 1.  General Graphs
#
######################

#create historgram for Invoice Value distribution
ggplot(Visa, aes(Invoice_Euro)) + geom_histogram(col="black", fill="White") 

#with adding binwidth
ggplot(Visa, aes(Invoice_Euro)) + geom_histogram(binwidth=50, col="black", fill="white") 

# count of transactions per country
ggplot(Visa, aes(Country)) + geom_histogram(binwidth=50, col="black", fill="white") 

# Value of Transactions per Country
ggplot(Visa, aes(x=Invoice_Euro, y=Country)) + geom_point()

#and I immediately see which are the countries I haven't been in past 12 Months -> China and Gibraltar
Visa %>%
    filter(Country == "China" | Country == "Gibraltar") %>%
    ggplot(aes(Country)) + geom_histogram(binwidth=50, col="black", fill="white") 

# but I want to see value of transactions
Visa %>%
  filter(Country == "China" | Country == "Gibraltar") %>%
  ggplot(aes(y=Invoice_Euro, x=Country)) + geom_point(col="darkgreen")


#and check china for number transactions
Visa %>%
  filter(Country == "China" | Country == "Gibraltar") %>%
  #adding mutate to change country from China -> 1 and Gibraltar -> 2
  mutate (CountryID = 
                     ifelse(Country == "China", 1,
                     ifelse(Country == "Gibraltar", 2 , 3))) %>%
  ggplot(aes(Invoice_Euro,fill=Country)) + geom_bar(binwidth=25)


#and also check for cumulative values of these transactions for both countries
Visa %>%
  filter(Country == "China" | Country == "Gibraltar") %>%
  qplot(x=factor(Country), data=., geom="bar",weight=Invoice_Euro, xlab="Country", ylab="Sum of Transactions (�)", fill=Country) 


#or create boxplot per country
Visa %>%
qplot(Country, Invoice_Euro, data=., geom=c("boxplot", "jitter"),
      fill=Country, main="Invoice value Per Country",
      xlab="", ylab="Invoice (�)")


#We can filter out countries with one transation or transactions that are not interesting for analysis
Visa %>%
  filter(Country != "Austria" & Country != "Swiss") %>%
  qplot(Country, Invoice_Euro, data=., geom=c("boxplot", "jitter"),
        fill=Country, main="Invoice value Per Country",
        xlab="", ylab="Invoice (�)")


#We can see that Gibraltar has third highest average Invoice �
#so we exclude four transactions that are above 400�
Visa %>%
  filter(Country != "Austria" & Country != "Swiss" & Invoice_Euro < 401) %>%
  qplot(Country, Invoice_Euro, data=., geom=c("boxplot", "jitter"),
        fill=Country, main="Invoice value Per Country",
        xlab="", ylab="Invoice (�)")


#Let's just see transactions 400� and above
Visa %>%
  filter(Country != "Austria" & Country != "Swiss" & Invoice_Euro > 401)
# and we see that both Slovenian are from source "SONCEK" (travel agency) that can be trusted - Airplane tickets for Brasilia


# let's see transactions over time
# Invoice_Euro x Day_Purchase
ggplot(data = Visa, aes(Day_Purchase, Invoice_Euro )) + geom_line() 


# a lot of spikes
# let's cut at 200� but before check what are these 200� transactions
Visa %>%
  filter(Invoice_Euro > 200)
# all are traveling related; airplane tickets and hotels!
# note that these two are installments (same date, same amount �)
#2   2016-04-16    2016-05-18               SONCEK 
#3   2016-05-16    2016-06-18               SONCEK 

# so we will plot just transactions less then 200�
Visa %>%
  filter(Invoice_Euro < 200) %>%
ggplot(data = ., aes(Day_Purchase, Invoice_Euro )) + geom_line() 


# and so on. We can continue to lower the boundary to check the transaction.
# but we can check followin periods: 1.November 2015; 2. around 1.April 2016; 3. around: May 2016; 4. July 2016; 5. September 2016


### So If you are lazy, why not use Shiny or Plotly? :-)
library(plotly)

plot_ly(Visa %>%
          filter(Invoice_Euro < 200) %>%
          ggplot(data = ., aes(Day_Purchase, Invoice_Euro )) + geom_line() )

# or
plot_ly( ggplot(Visa, aes(x=Invoice_Euro, y=Country)) + geom_point() )


######################
#
# 2.  Statistics 
#
######################


#################################
# 2.1 Running general Statistics
#################################


# statistics per country
Visa %>%
  group_by(Country) %>%
    summarise(
           total_vof_tran = sum(Invoice_Euro, na.rm=TRUE)
          ,nof_trans = n()
          ,avg_of_tran = mean(Invoice_Euro, na.rm=TRUE)
          ,min_tran = min(Invoice_Euro, na.rm=TRUE)
          ,max_tran = max(Invoice_Euro, na.rm=TRUE)
          ,media_tran = median(Invoice_Euro, na.rm=TRUE)
          ,var_tran = var(Invoice_Euro, na.rm=TRUE)
          ,sd_tran = sd(Invoice_Euro, na.rm=TRUE)
          )

# statistics per month

Visa %>%
  mutate(month_of_purchase = month(Day_Purchase)) %>%
  group_by(Country,month_of_purchase) %>%
  filter(Country != "Austria" & Country != "Swiss" & Country != "USA") %>%
  summarise(
    total_vof_tran = sum(Invoice_Euro, na.rm=TRUE)
    ,nof_trans = n()
    ,avg_of_tran = mean(Invoice_Euro, na.rm=TRUE)
    ,min_tran = min(Invoice_Euro, na.rm=TRUE)
    ,max_tran = max(Invoice_Euro, na.rm=TRUE)
    ,media_tran = median(Invoice_Euro, na.rm=TRUE)
    ,var_tran = var(Invoice_Euro, na.rm=TRUE)
    ,sd_tran = sd(Invoice_Euro, na.rm=TRUE)
  )

# And we focus on  these periods
# but we can check followin periods: 1.November 2015; 2. around 1.April 2016; 3. around: May 2016; 4. July 2016; 5. September 2016
# and we skip Countries: Austria, Swiss, USA and Brasil (we have checked all the transactions)

Visa %>%
  mutate(month_of_purchase = month(Day_Purchase), year_of_purchase=year(Day_Purchase)) %>%
  group_by(Country,month_of_purchase) %>%
  filter(Country != "Austria" & Country != "Swiss" & Country != "USA" & Country != "Brasil" & Invoice_Euro < 200 & month_of_purchase  %in% c(11,4,5,6,9)) %>%
  summarise(
    total_vof_tran = sum(Invoice_Euro, na.rm=TRUE)
    ,nof_trans = n()
    ,avg_of_tran = mean(Invoice_Euro, na.rm=TRUE)
    ,min_tran = min(Invoice_Euro, na.rm=TRUE)
    ,max_tran = max(Invoice_Euro, na.rm=TRUE)
    ,media_tran = median(Invoice_Euro, na.rm=TRUE)
    ,var_tran = var(Invoice_Euro, na.rm=TRUE)
    ,sd_tran = sd(Invoice_Euro, na.rm=TRUE)
  )

# Strange transactions are Gibraltar in Month = 9 and Slovenija in Month = 4



#################################
# 2.2 Adding derived column
#################################

# Adding some logic using derived columns

# Two new varaibles:
# variables - Limit_number
#           - Fraud

All %>%
  filter(Limit_number==1 & Invoice_Value > 30 & Invoice_Type == "Visa")


All %>%
  mutate(my_filter_variable = ifelse((Limit_number==1 & Invoice_Value > 30 & Invoice_Type == "Visa"),1,0)) %>%
  filter(my_filter_variable = 1)




######################
#
# 3.  Building Models 
#
######################

# DataSet: All_analysis
# Target Varible: All_analysis$fraud

# we will use all the transactions (Visa and Mastro) 
# and directed models

#variables
names(All_analysis)

# Let's make a split over all 70/30
# I can also use k-folds, k-fold cross-validation,  bootstrapping, jagged bootstrapping, etc...
# remove target variable/label

# Y <- All_analysis[,-1]
Y <- All_analysis

#split function
splitdf <- function(dataframe, seed=NULL) {
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(dataframe)
  trainindex <- sample(index, trunc(length(index)/1.5))
  trainset <- dataframe[trainindex, ]
  testset <- dataframe[-trainindex, ]
  list(trainset=trainset,testset=testset)
}

splits <- splitdf(Y, seed=999)


# there are almost 682/341 observations in each data frame
lapply(splits,nrow)


# save the training and testing sets as data frames
training <- splits$trainset
testing <- splits$testset

#Check for the distribution 
training %>%
    group_by(Country_ID) %>%
    summarise( 
        count = n())

testing %>%
  group_by(Country_ID) %>%
  summarise( 
    count = n())

############################
# 3.1. Logistic Regression
############################

fraud.glm = glm(formula=Fraud ~ Limit_number + Invoice_Type_ID + Invoice_Value + Country_ID, data=training, family=binomial, control = list(maxit = 50)) 

#predict the outcome of the testing data and check prediction
predicted.glm <- predict(fraud.glm, testing, type="response") 

summary(fraud.glm)

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)
#(Intercept)     -1.464e+02  2.709e+05  -0.001        1
#Limit_number     6.040e+01  1.293e+05   0.000        1
#Invoice_Type_ID  6.060e+01  1.994e+05   0.000        1
# ....

predicted.glm <- ifelse(predicted.glm > 0.5,1,0)
mean(predicted.glm != testing$Fraud)
print(paste('Accuracy is ', 1-(mean(predicted.glm != testing$Fraud))))


######################
# 3.2. Naive Bayes
######################


fraud.nb <- naiveBayes(Fraud~ Limit_number + Invoice_Type_ID + Invoice_Value + Country_ID, data = training)

#predict the outcome of the testing data and check predictions
predicted.nb <- predict(fraud.nb, testing)

table(predicted.nb, training[c(7,6,2,4)])



#########################################
# 3.3. Random Forest ( Decision Trees )
#########################################


#fit the randomforest model
fraud.rf <- randomForest(Fraud~Limit_number + Invoice_Type_ID + Invoice_Value + Country_ID, data = training, importance=TRUE,keep.forest=TRUE)

#print(fraud.rf)
#finding improtant variables
varImpPlot(fraud.rf, type=1)

#predict the outcome of the testing data
predicted.rf <- predict(fraud.rf, newdata=testing[ ,-1])


# what is the proportion variation explained in the outcome of the testing data?
# I am using precision! 1-(SSerror/SStotal)

print(paste('Accuracy is ',(1-sum((testing$Fraud-predicted.rf)^2)/sum((testing$Fraud-mean(testing$Fraud))^2))))

# proportion of variation explained! = 37%
#[1] 0.3705949


################################
# 3.4. Clusters
################################

################################
# 3.4.1 Maximization Expectation
################################
library(EMCluster)




################################
# 3.4.2 Density Based
################################
library(fpc)


# eps is radius of neighborhood, MinPts is no of neighbors
# within eps
cluster <- dbscan(All[,-5], eps=0.6, MinPts=4)
plot(cluster, sampleiris)
plot(cluster, sampleiris[,c(1,4)])
# Notice points in cluster 0 are unassigned outliers
table(cluster$cluster, sampleiris$Species)





################################
#
# 4.  Measuring Efficiency
#
################################
library(ROCR)


mnistResultsDF <- data.frame(actual = mnistTest$label,
                             fit = mnist.kknn$fit,
                             as.data.frame(mnist.kknn$prob))

plotROCs <- function(df, digitList) {
  firstPlot <- TRUE
  legendList <- NULL
  for (digit in digitList) {
    dfDigit <- df %>%
      filter(as.character(actual) == as.character(digit) |
               as.character(fit) == as.character(digit))  %>%
      mutate(prediction = (as.character(actual) == as.character(fit)))
    
    pred <- prediction(dfDigit[,digit+3], dfDigit$prediction)
    perf <- performance(pred, "tpr", "fpr")
    auc <- performance(pred, "auc")
    legendList <- append(legendList, 
                         paste0("Digit: ",digit,", AUC: ",
                                round(auc@y.values[[1]], digits = 4)))
    if (firstPlot == TRUE) {
      plot(perf, colorize = FALSE, lty = digit+1, col = digit+1)
      firstPlot <- FALSE  
    } else {
      plot(perf, colorize = FALSE, add = TRUE, lty = digit+1, col = digit+1)
    }
  }
  legend(x=0.4, y=0.6,
         legend = legendList,
         col = 1:10,
         lty = 1:10,
         bty = "n")
}


plotROCs(mnistResultsDF, 0:9)



#####

library(ROCR)
p <- predict(model, newdata=subset(test,select=c(2,3,4,5,6,7,8)), type="response")
pr <- prediction(p, test$Survived)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc