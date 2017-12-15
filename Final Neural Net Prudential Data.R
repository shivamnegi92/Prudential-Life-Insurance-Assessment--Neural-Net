install.packages("forecast")

library('forecast')

# Import train data
library(readxl)

train.data <- read.csv("C:/Users/shiva/Desktop/ADS Midterm data cleaning/ADS MidTerm Data Cleaning/train.csv/train.csv")
View(train)
str(train.data)

head(train.data)

#2. Import test dataset

test.data <- read.csv("C:/Users/shiva/Desktop/ADS Midterm data cleaning/ADS MidTerm Data Cleaning/test.csv/test.csv")

str(test.data)


head(test.data)

#3. Remove columns that has too much missing value

ValueMissingVecTrain <- NA
for (i in (1:127)) ValueMissingVecTrain[i] <- sum(is.na(train.data[,i]))/nrow(train.data)
inedxMatRemove1 <- which(ValueMissingVecTrain >= 0.55)
ValueMissingVecTest <- NA
for (i in (1:127)) ValueMissingVecTest[i] <- sum(is.na(test.data[,i]))/nrow(test.data)
inedxMatRemove2 <- which(ValueMissingVecTest >= 0.55)



inedxMatRemove1
inedxMatRemove2



train.data.processed <- train.data[,-inedxMatRemove1]
test.data.processed <- test.data[,-inedxMatRemove1]

head(train.data.processed)

head(test.data.processed)

#4. Get rows that have NA values

ValueMissingColTrain <- NA
for (i in (1:122)) ValueMissingColTrain[i] <- sum(is.na(train.data.processed[,i]))
inedxMatKeep1 <- which(ValueMissingColTrain > 0)

ValueMissingColText <- NA
for (i in (1:121)) ValueMissingColText[i] <- sum(is.na(test.data.processed[,i]))
inedxMatKeep2 <- which(ValueMissingColText > 0)

inedxMatKeep1
inedxMatKeep2

#5. Filling NA value in 13 Column for train.data.processed

colnames(train.data.processed)[13]    # Continuous
paste("Number of missing value:", sum(is.na(train.data.processed[,13])))
print("--------------------------------------------------------------------------------------")
str(train.data.processed[,13])
print("--------------------------------------------------------------------------------------")

paste("MEAN  :", mean(train.data.processed[,13], na.rm = TRUE))
paste("MEDIAN:", median(train.data.processed[,13], na.rm = TRUE))
paste("MAX   :", max(train.data.processed[,13], na.rm = TRUE))
paste("MIN   :", min(train.data.processed[,13], na.rm = TRUE))

# most value is around 0.06, less than 0.1
# we choose Average as missing value
train.data.processed.filling <- train.data.processed
test.data.processed.filling <- test.data.processed
train.data.processed.filling[,13][is.na(train.data.processed.filling[,13])] <- 0.078
test.data.processed.filling[,13][is.na(test.data.processed.filling[,13])] <- 0.078


#6. Filling NA value in 16 Column for train.data.processed

colnames(train.data.processed)[16]    # Continuous
paste("Number of missing value:", sum(is.na(train.data.processed[,16])))
print("--------------------------------------------------------------------------------------")
str(train.data.processed[,16])
print("--------------------------------------------------------------------------------------")

paste("MEAN  :", mean(train.data.processed[,16], na.rm = TRUE))
paste("MEDIAN:", median(train.data.processed[,16], na.rm = TRUE))
paste("MAX   :", max(train.data.processed[,16], na.rm = TRUE))
paste("MIN   :", min(train.data.processed[,16], na.rm = TRUE))
percentage_0 <- length(which(train.data.processed[,16] == 0)) / length(which(!is.na(train.data.processed[,16])))
paste("Percentage of Value 0:", paste(percentage_0*100, "%", sep=''))

# more than 84% of column 16 is 0, we choose 0 as missing value
train.data.processed.filling[,16][is.na(train.data.processed.filling[,16])] <- 0
test.data.processed.filling[,16][is.na(test.data.processed.filling[,16])] <- 0

#7. Filling NA value in 18 Column for train.data.processed

colnames(train.data.processed)[18]    # Continuous
paste("Number of missing value:", sum(is.na(train.data.processed[,18])))
print("--------------------------------------------------------------------------------------")
str(train.data.processed[,18])
print("--------------------------------------------------------------------------------------")

paste("MEAN  :", mean(train.data.processed[,18], na.rm = TRUE))
paste("MEDIAN:", median(train.data.processed[,18], na.rm = TRUE))
paste("MAX   :", max(train.data.processed[,18], na.rm = TRUE))
paste("MIN   :", min(train.data.processed[,18], na.rm = TRUE))

# 18 column is Continuous from 0 to 1, we choose average as missing value
train.data.processed.filling[,18][is.na(train.data.processed.filling[,18])] <- 0.36
test.data.processed.filling[,18][is.na(test.data.processed.filling[,18])] <- 0.36

#8. Filling NA value in 30 Column for train.data.processed

colnames(train.data.processed)[30]    # Continuous
paste("Number of missing value:", sum(is.na(train.data.processed[,30])))
print("--------------------------------------------------------------------------------------")
str(train.data.processed[,30])
print("--------------------------------------------------------------------------------------")

paste("MEAN  :", mean(train.data.processed[,30], na.rm = TRUE))
paste("MEDIAN:", median(train.data.processed[,30], na.rm = TRUE))
paste("MAX   :", max(train.data.processed[,30], na.rm = TRUE))
paste("MIN   :", min(train.data.processed[,30], na.rm = TRUE))
percentage_0.01 <- length(which(train.data.processed[,30] < 0.01)) / length(which(!is.na(train.data.processed[,30])))
paste("Percentage of Value 0:", paste(percentage_0.01*100, "%", sep=''))

# more than 98% of column 30 is less than 0.01, we choose 0 as missing value
train.data.processed.filling[,30][is.na(train.data.processed.filling[,30])] <- 0
test.data.processed.filling[,30][is.na(test.data.processed.filling[,30])] <- 0

#9. Filling NA value in 35 Column for train.data.processed

colnames(train.data.processed)[35]    # Continuous
paste("Number of missing value:", sum(is.na(train.data.processed[,35])))
print("--------------------------------------------------------------------------------------")
str(train.data.processed[,35])
print("--------------------------------------------------------------------------------------")

paste("MEAN  :", mean(train.data.processed[,35], na.rm = TRUE))
paste("MEDIAN:", median(train.data.processed[,35], na.rm = TRUE))
paste("MAX   :", max(train.data.processed[,35], na.rm = TRUE))
paste("MIN   :", min(train.data.processed[,35], na.rm = TRUE))

# MEAN and MEDIAN is similiar, column 35 has Continuous data between 0 to 1
# we choose median as missing value
train.data.processed.filling[,35][is.na(train.data.processed.filling[,35])] <- 0.46
test.data.processed.filling[,35][is.na(test.data.processed.filling[,35])] <- 0.46

#10. Filling NA value in 36 Column for train.data.processed

colnames(train.data.processed)[36]    # Continuous
paste("Number of missing value:", sum(is.na(train.data.processed[,35])))
print("--------------------------------------------------------------------------------------")
str(train.data.processed[,36])
print("--------------------------------------------------------------------------------------")

paste("MEAN  :", mean(train.data.processed[,36], na.rm = TRUE))
paste("MEDIAN:", median(train.data.processed[,36], na.rm = TRUE))
paste("MAX   :", max(train.data.processed[,36], na.rm = TRUE))
paste("MIN   :", min(train.data.processed[,36], na.rm = TRUE))

# MEAN and MEDIAN is similiar, column 36 has Continuous data between 0 to 1
# we choose median as missing value
train.data.processed.filling[,36][is.na(train.data.processed.filling[,36])] <- 0.44
test.data.processed.filling[,36][is.na(test.data.processed.filling[,36])] <- 0.44

#11. Filling NA value in 37 Column for train.data.processed

colnames(train.data.processed)[37]    # Discrete
paste("Number of missing value:", sum(is.na(train.data.processed[,37])))
print("--------------------------------------------------------------------------------------")
str(train.data.processed[,37])
print("--------------------------------------------------------------------------------------")

paste("MEAN  :", mean(train.data.processed[,37], na.rm = TRUE))
paste("MEDIAN:", median(train.data.processed[,37], na.rm = TRUE))
paste("MAX   :", max(train.data.processed[,37], na.rm = TRUE))
paste("MIN   :", min(train.data.processed[,37], na.rm = TRUE))
b <- table(train.data.processed[,37])
paste("MODE  :", as.numeric(names(b)[b == max(b)]))

# Column 37 is discrete, range between 0 to 240
# we choose to set Mode "1" as the missing value, because its repetition rate is high
train.data.processed.filling[,37][is.na(train.data.processed.filling[,37])] <- 1
test.data.processed.filling[,37][is.na(test.data.processed.filling[,37])] <- 1

#12. Check the new data frame

str(train.data.processed.filling)



#12. Convert all categorical variables to factor and remove id column


train.data.processed.convert <- train.data.processed.filling
train.data.processed.convert<-train.data.processed.convert[,-1]

train.data.processed.convert

colnames(train.data.processed.convert)

train.data.processed.convert[,'Product_Info_2']<-as.numeric(train.data.processed.convert[,'Product_Info_2'])
str(train.data.processed.convert)

fit <- lm(Response~. ,data=train.data.processed.convert)
summary(fit)



significant_colnames<-rownames(data.frame(summary(fit)$coef[summary(fit)$coef[,4] <= .05,4])[,0])
significant_colnames

categorical.names <- c(paste("Product_Info_", c(1:3,5:7), sep=""), paste("Employment_Info_", c(2,3,5), sep=""),paste("InsuredInfo_", 1:7, sep=""), paste("Insurance_History_", c(1:4,7:9), sep=""),"Family_Hist_1", paste("Medical_History_", c(2:9, 11:14, 16:23, 25:31, 33:41), sep=""))
categorical.names


cat.names<-intersect(significant_colnames,categorical.names)
cat.names


for (i in cat.names) train.data.processed.convert[,i]<-as.factor(train.data.processed.convert[,i])
str(train.data.processed.convert)

head(train.data.processed.convert)

predictors <- ''

#binary <- model.matrix(~Product_Info_6,data=train.data.processed.convert)

for( i in cat.names){
                          predictors   <- paste( predictors, i, sep=' + ')
                        #cbind(binary,model.matrix(~ i,data=train.data.processed.convert))
                        }
predictors <- substring(predictors, 4)
#predictors
form <- as.formula(paste(paste("~", predictors), '-1'))

binary <- model.matrix(form,data=train.data.processed.convert)


str(binary)

x <- setdiff(significant_colnames, categorical.names)
x

#train.data.processed.convert <- cbind(train.data.processed.convert,model.matrix(form,data=train.data.processed.convert))
# drop the cat.names columns 
#train.data.processed.convert.NC <- train.data.processed.convert[ , !(names(train.data.processed.convert) %in% cat.names)]
train.data.processed.convert.significant <- train.data.processed.convert[ , (names(train.data.processed.convert) %in% x)]
str(train.data.processed.convert.significant)
# update the binary columns of cat.names columns and bind with the dataframe
#train.data.processed.convert.binary <- cbind(train.data.processed.convert , binary)

train.data.processed.convert.significant.binary <- cbind(train.data.processed.convert.significant, binary)

str(train.data.processed.convert.significant.binary)


p <- train.data.processed.convert.significant.binary
length(names(p))
#col <- as.character(colnames(p))
#for(i in colnames(p)){paste(i,1)}


for(i in names(p)){
            IndexList <- which(colnames(p) == i)
            #IndexList
            totalCount = length(IndexList)
            #paste(i, totalCount)
            if(totalCount > 1){
                #i
                for(j in IndexList){
                    cnt = 1
                    colnames(p)[j] <- paste(i,cnt,sep ='_')
                    cnt = cnt + 1
                }
            }
    
}



p['Response'] <- train.data.processed$Response
length(names(p))

names(p)


model <- lm (Response~., data= p)
summary(model)

significant_colnames_model<-rownames(data.frame(summary(model)$coef[summary(model)$coef[,4] <= .05,4])[,0])
significant_colnames_model

q <- p[,significant_colnames_model]
q['Response'] <- p['Response']
length(colnames(q))

model <- lm (Response~., data= q)
summary(model)
significant_colnames_model1<-rownames(data.frame(summary(model)$coef[summary(model)$coef[,4] <= .05,4])[,0])
length(significant_colnames_model1)

r <- q[,significant_colnames_model]
r['Response'] <- p['Response']
length(colnames(r))

model <- lm (Response~., data= r)
summary(model)
significant_colnames_model2<-rownames(data.frame(summary(model)$coef[summary(model)$coef[,4] <= .05,4])[,0])
length(significant_colnames_model2)
significant_colnames_model2

n <- length(significant_colnames_model2)
significant_colnames_model2 <- significant_colnames_model2[2:n]
s <- r[,significant_colnames_model2]
s['Response'] <- r['Response']
length(colnames(s))

model <- lm (Response~., data= s)
summary(model)
significant_colnames_model3<-rownames(data.frame(summary(model)$coef[summary(model)$coef[,4] < .01,4])[,0])
length(significant_colnames_model3)

n <- length(significant_colnames_model3)
significant_colnames_model3 <- significant_colnames_model3[2:n]
t <- s[,significant_colnames_model3]
t['Response'] <- s['Response']
length(colnames(t))

model <- lm (Response~., data= t)
summary(model)
significant_colnames_model4<-rownames(data.frame(summary(model)$coef[summary(model)$coef[,4] < .05,4])[,0])
length(significant_colnames_model4)

n <- length(significant_colnames_model4)
significant_colnames_model4 <- significant_colnames_model4[2:n]
u <- t[,significant_colnames_model3]
u['Response'] <- t['Response']
length(colnames(u))

model <- lm (Response~., data= u)
summary(model)
significant_colnames_model5<-rownames(data.frame(summary(model)$coef[summary(model)$coef[,4] < .01,4])[,0])
length(significant_colnames_model5)

n <- length(significant_colnames_model5)

significant <- intersect(significant_colnames_model5[2:n], significant_colnames_model)
length(significant)


#------------selecting 15 important features USING xgboost---
important_features <- c('BMI','Medical_History_233_1.1','Medical_History_42','Medical_Keyword_3','Product_Info_4','Wt','Ins_Age','Medical_History_403','Medical_History_133','Product_Info_215','InsuredInfo_62','Insurance_History_23','InsuredInfo_53','Medical_History_1','InsuredInfo_73')

NN <- u[,important_features]
NN$Response <- u[,"Response"]
str(NN)
NN <- data.matrix(NN)



set.seed(101) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 75% of data as sample from total 'n' rows of the data  
sample <- sample.int(n = nrow(NN), size = floor(.80*nrow(NN)), replace = F)
NN_Ttrain <- NN[sample, ]
NN_Ttest  <- NN[-sample, ]

nrow(NN)
nrow(NN_Ttrain)
nrow(NN_Ttest)

nTrain <- colnames(NN_Ttrain)
f <- as.formula(paste("Response ~", paste(nTrain[!nTrain %in% "Response"], collapse = " + ")))
f

#trin_data <- NN_Ttrain[,- which(colnames(NN_Ttrain) == "Response")]
library(neuralnet)
nn <- neuralnet(f,data=NN_Ttrain,hidden=3,act.fct = "tanh",stepmax = 1e6, linear.output=T)
plot(nn)
#colnames(NN_Ttrain[,- which(colnames(NN_Ttrain) == "Response")])

head(nn$generalized.weights[[1]])
  
head(NN_Ttest)
ncol(NN_Ttest)


TestInput<-NN_Ttest[,-16]
colnames(TestInput)

ncol(TestInput)

nn.results <- compute(nn, TestInput)

length(nn.results)

head(nn.results)

TestInputorginal<-data.frame(TestInput)

nn.results <- compute(nn, TestInput)




actualinput<-data.frame(NN_Ttest)

predictedoutput<-data.frame(nn.results$net.result)

head(predictedoutput)

nrow(actualinput)
nrow(predictedoutput)

table(predictedoutput, actualinput[16])

NROW(TestInput)
ncol(TestInput)


responsedata<-data.frame(nn.results)


results <- data.frame(actual = actualinput[16], prediction = predictedoutput)
results
roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
roundedresultsdf
labels(roundedresults)

table(roundedresults[,1], roundedresults[,2])
dieff<-roundedresults[,1]-roundedresults[,2]



error <- dieff
rmse <- sqrt(mean(error^2))

View(rmse)


accuracy(roundedresults[,1], roundedresults[,2])
