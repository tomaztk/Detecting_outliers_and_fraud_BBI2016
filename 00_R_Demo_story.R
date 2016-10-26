##############################################
### 
###  Budapest BI Forum 2016 (26.10.2016)
###
###  Detecting outliers and 
###  fraud(lent) transaction using R
###
###  Author: Tomaž Kaštrun  
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
library(mclust)
library(ggplot2)
library(fpc)
library(ROCR)
library(lubridate)
library(randomForest)
library(caTools)
library(ggmap)




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
# Transactions as: LETNA ÈLANARINA - additional logic needed; these are maintainance transactions (year subscription for VISA)
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
  qplot(x=factor(Country), data=., geom="bar",weight=Invoice_Euro, xlab="Country", ylab="Sum of Transactions (€)", fill=Country) 


#or create boxplot per country
Visa %>%
qplot(Country, Invoice_Euro, data=., geom=c("boxplot", "jitter"),
      fill=Country, main="Invoice value Per Country",
      xlab="", ylab="Invoice (€)")


#We can filter out countries with one transation or transactions that are not interesting for analysis
Visa %>%
  filter(Country != "Austria" & Country != "Swiss") %>%
  qplot(Country, Invoice_Euro, data=., geom=c("boxplot", "jitter"),
        fill=Country, main="Invoice value Per Country",
        xlab="", ylab="Invoice (€)")


#We can see that Gibraltar has third highest average Invoice €
#so we exclude four transactions that are above 400€
Visa %>%
  filter(Country != "Austria" & Country != "Swiss" & Invoice_Euro < 401) %>%
  qplot(Country, Invoice_Euro, data=., geom=c("boxplot", "jitter"),
        fill=Country, main="Invoice value Per Country",
        xlab="", ylab="Invoice (€)")


#Let's just see transactions 400€ and above
Visa %>%
  filter(Country != "Austria" & Country != "Swiss" & Invoice_Euro > 401)
# and we see that both Slovenian are from source "SONCEK" (travel agency) that can be trusted - Airplane tickets for Brasilia


# let's see transactions over time
# Invoice_Euro x Day_Purchase
ggplot(data = Visa, aes(Day_Purchase, Invoice_Euro )) + geom_line() 


# a lot of spikes
# let's cut at 200€ but before check what are these 200€ transactions
Visa %>%
  filter(Invoice_Euro > 200)
# all are traveling related; airplane tickets and hotels!
# note that these two are installments (same date, same amount €)
#2   2016-04-16    2016-05-18               SONCEK 
#3   2016-05-16    2016-06-18               SONCEK 

# so we will plot just transactions less then 200€
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


### GEO SKIP!
#since we have Long/Lat Data, we can plot on Maps
Visa_Geo <- Visa[,c(6,9:10)]

Visa_Geo <- Visa_Geo %>%
            mutate(vof_invoice_recode = ifelse(Invoice_Euro > 0 & Invoice_Euro < 50, 1,
                                              ifelse(Invoice_Euro >= 50 & Invoice_Euro < 100, 2,
                                                    ifelse(Invoice_Euro >= 100 & Invoice_Euro < 200, 3, 4)
                                                          )
                                                    )
                                            )


library(ggmap)
map <- get_map(location = 'Europe', zoom = 4)
#map <- get_map(location = 'World', zoom = 2)



mapPoints <- ggmap(map) + geom_point(aes(x = LNG, y = LAT, size = sqrt(vof_invoice_recode)), data = Visa_Geo, alpha = .5)
mapPoints <- ggmap(map) + geom_point(aes(x = LNG, y = LAT, size = sqrt(Invoice_Euro)), data = Visa_Geo, alpha = .5)
mapPoints <- ggmap(map) + geom_point(data=Visa_Geo, aes(x = LNG, y = LAT),alpha = 0.5, size = 4)

#mapPoints <- ggmap(map) + geom_point(aes(x = LNG, y = LAT, size = vof_invoice_recode), data = Visa_Geo, alpha = .5)

mapPoints

#str(Visa_Geo)
#Visa_Geo$LAT <- as.character(Visa_Geo$LAT)
#Visa_Geo$LNG <- as.character(Visa_Geo$LNG)



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
# I can also use k-fold Cross Validation, Repeated k-fold Cross Validation, Leave One Out Cross Validation,  bootstrapping, jagged bootstrapping, etc...
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
# 3.1. GLM - Regression
############################

#extract just the variables I need
training_lr <- training[,c(2,4,6:8)]
testing_lr <- testing[,c(2,4,6:8)]

training_lr$Fraud <- factor(training_lr$Fraud)
testing_lr$Fraud <- factor(testing_lr$Fraud)


fraud.glm = glm(formula=Fraud ~ Limit_number + Invoice_Type_ID + Invoice_Value + Country_ID, data=training_lr, family=binomial, control = list(maxit = 50)) 

#predict the outcome of the testing data and check prediction
predicted.glm <- predict(fraud.glm, testing_lr, type="response") 

summary(fraud.glm)

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)
#(Intercept)     -1.464e+02  2.709e+05  -0.001        1
#Limit_number     6.040e+01  1.293e+05   0.000        1
#Invoice_Type_ID  6.060e+01  1.994e+05   0.000        1
# ....

predicted.glm <- ifelse(predicted.glm > 0.5,1,0)
mean(predicted.glm != testing_lr$Fraud)
print(paste('Accuracy is ', 1-(mean(predicted.glm != testing_lr$Fraud))))

table(predicted.glm, testing_lr[,5])


rm(training_lr,testing_lr)

######################
# 3.2. Naive Bayes
######################

#extract just the variables I need
training_nb <- training[,c(2,4,6:8)]
testing_nb <- testing[,c(2,4,6:8)]

training_nb$Fraud <- factor(training_nb$Fraud)
testing_nb$Fraud <- factor(testing_nb$Fraud)

str(training_nb)
str(testing_nb)

fraud.nb <- naiveBayes(Fraud~ Limit_number + Invoice_Type_ID + Invoice_Value + Country_ID, data = training_nb)

#predict the outcome of the testing data and check predictions
predicted.nb <- predict(fraud.nb, testing_nb[,-5]) # test without the label data

#results of NB
table(predicted.nb, testing_nb[,5])

#Confusion Matrix
confusionMatrix(predicted.nb, testing_nb[,5])

 #           Accuracy: 0.9941 
# P-Value [Acc > NIR]: 0.6767 

rm(training_nb,testing_nb)

#########################################
# 3.3. Random Forest ( Decision Trees )
#########################################

#extract just the variables I need
training_rf <- training[,c(2,4,6:8)]
testing_rf <- testing[,c(2,4,6:8)]

training_rf$Fraud <- factor(training_rf$Fraud)
testing_rf$Fraud <- factor(testing_rf$Fraud)

#fit the randomforest model
fraud.rf <- randomForest(Fraud~Limit_number + Invoice_Type_ID + Invoice_Value + Country_ID, data = training_rf, importance=TRUE,keep.forest=TRUE)

#print(fraud.rf)
#finding improtant variables
varImpPlot(fraud.rf, type=1)


#predict the outcome of the testing data
predicted.rf <- predict(fraud.rf, newdata=testing_rf[ ,-5])


# what is the proportion variation explained in the outcome of the testing data?
# I am using precision! 1-(SSerror/SStotal)

print(paste('Proportion of variance explained '
            ,(1-sum((as.numeric(testing_rf$Fraud)-predicted.rf)^2)/sum((as.numeric(testing_rf$Fraud)-mean(as.numeric(testing_rf$Fraud))^2)))))

# proportion of variation explained! = 32%
#[1] 0.3254076

#class error predictions are very acccurate to the fact that they are wrong!
table(predicted.rf, testing_rf[,5])

#browse the model
confusionMatrix(predicted.rf, testing_rf[,5])
#            Accuracy : 0.9941         
#              95% CI : (0.979, 0.9993)
# No Information Rate : 0.9941 

rm(testing_rf,training_rf)

################################
# 3.4. Clusters
################################

################################
# 3.4.1 Expectation-Maximization
################################
library(EMCluster)
library(mclust)

all_cluster <- All_analysis[,c(2,4,6:8)]

ME_mc <- Mclust(all_cluster[,1:4], 3) #presumme 3 clusters
ME_mc <- Mclust(all_cluster[,1:4], 4) 
ME_mc <- Mclust(all_cluster[,1:4], 5) 

table(all_cluster$Fraud, ME_mc$classification)

#classification for each cluster
ME_mc$classification


#loading for each cluster
ME_mc$z


################################
# 3.4.2 Density Based / eps radius
################################
library(fpc)

# eps is radius of neighborhood, MinPts is no of neighbors within eps
DB_cluster <- dbscan(all_cluster[,-5], eps=0.4, MinPts=1)
#vs
DB_cluster <- dbscan(all_cluster[,-5], eps=0.6, MinPts=4)

plot(DB_cluster, all_cluster)
plot(DB_cluster, all_cluster[,c(1,4)])

# Cluster 0 are unassigned outliers and all fraudulent are outliers!
table(DB_cluster$cluster, all_cluster$Fraud)





################################
#
# 4.  Measuring Efficiency
#
################################
library(ROCR)


pred <- prediction(predicted.nb, testing_nb[,-5]) 
#plot for NB
prf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc




################################# 
#
# 99.  MISC (export datasets)
#
#################################

# export data to csv
write.csv(Maestro, file = "maestro.csv", row.names = TRUE)
write.csv(Visa, file = "visa.csv", row.names = FALSE)
write.csv(All, file = "all.csv", row.names = FALSE)