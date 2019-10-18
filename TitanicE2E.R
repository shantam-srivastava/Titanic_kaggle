
setwd("E:/Downloads/Models on Titanic Dataset")

require(titanic)

# Imorting the datasets
train <- titanic::titanic_train
test <- titanic::titanic_test

require(dplyr)
full <- bind_rows(train, test)

# Let's take a peek at dataset
str(full)     
glimpse(train)

summary(full)

# Case matching
full <- mutate_if(full, is.character, toupper)

sapply(full, n_distinct)

# Check duplicates
duplicated(full[,-1])

sum(duplicated(full[,-1]))


colSums(is.na(full))
# NA's
# 418 in Survived
# 263 in Age
# 1 in Fare

# Cheking NA of 'Fare'.
which(is.na(full$Fare))

full[1044,]

# It has 'Pclass' = 3 and 'Embarked' = S
# Let's see if they have impact on price.
summary(full$Fare[which(full$Embarked == 'S' & full$Pclass == 3)])

# We see '0' Fare which seems ambiguous.
# Let's check in detail.
View(subset(full, Fare == 0))

# We see that for all Fare = 0 : 
#   1. 'Embarked' at Port 'S'; 
#   2. Are all male; 
#   3. Have SibSp and Parch '0' i.e. no family travelling with them.
#   4. Have only one person survived i.e. very low survival.

# It seems to be the working crew of the ship for many reasons

# Replacing NA with median.
full$Fare[1044] <- 8.050

sapply(full, function(x) length(which(x == "")))

# Blanks
# 1014 in Cabin
# 2 in Embarked
full$Cabin <- NULL

# 2 Blanks in Embarked.
table(full$Embarked)

full[which(full$Embarked == ""),]

require(ggplot2)
ggplot(full, aes(x = Embarked, y = Fare)) + facet_grid(. ~ Pclass) + geom_boxplot()

ggplot(full, aes(x = Embarked, y = Fare)) + facet_grid(. ~ Pclass) + geom_boxplot() + 
  geom_hline(aes(yintercept = 80), col = "red", linetype = 2)

# So we should replace it by 'C'
full$Embarked[which(full$Embarked == "")] <- "C"

full$Embarked <- as.factor(full$Embarked)

sapply(full, function(x) length(which(x == " ")))


#### EDA

# Univariate Analysis
titanic_plot <- ggplot(full[1:891,], aes(fill = factor(Survived)))

# Survived
titanic_plot + geom_bar(aes(x = Survived))
table(full$Survived)

# Pclass
titanic_plot + geom_bar(aes(x = Pclass))
titanic_plot + geom_bar(aes(x = Pclass), position = 'fill')

full$Pclass <- as.factor(full$Pclass)
median(full$Fare)
# Name
length(unique(full$Name))
arrange(full[duplicated(full$Name) | duplicated(full$Name, fromLast = T),], Name)

require(stringr)
surname <- str_split(full$Name, pattern = ", ", simplify = T)
full$surname <- surname[,1]

title <- str_split(surname[,2], pattern = "[.]", simplify = T)
full$title <- title[,1]

firstname <- str_split(title[,2], pattern = " ", simplify = T)
full$firstname <- firstname[,2]

full$Name <- NULL

length(unique(full$title))
knitr::kable(table(full$title, full$Sex))

full$title[full$title %in% c('CAPT','COL','MAJOR','REV','DR')] <- 'OFFICER'
full$title[full$title %in% c('DON','DONA','JONKHEER','LADY','MLLE','MME','SIR','THE COUNTESS')] <- 'NOVELTY'
full$title[full$title %in% c('MS')] <- 'MISS'

knitr::kable(table(full$title, full$Sex))
full$title <- as.factor(full$title)

length(unique(full$firstname))
length(unique(full$surname))

full$firstname <- NULL
full$surname <- NULL


#### Visualization ----
titanic_plot <- ggplot(full[1:891,], aes(fill = factor(Survived)))

titanic_plot + geom_bar(aes(x = title))
titanic_plot + geom_bar(aes(x = title), position = 'fill')

# Sex
titanic_plot + geom_bar(aes(x = Sex))
titanic_plot + geom_bar(aes(x = Sex), position = 'fill')

full$Sex <- as.factor(full$Sex)

# Age
titanic_plot + geom_histogram(aes(x = Age), binwidth = 10, col = 'black')
titanic_plot + geom_histogram(aes(x = Age), binwidth = 10, col = 'black', position = 'fill')

summary(full$Age)

plot(quantile(full$Age, seq(0,1,0.01), na.rm = T))
quantile(full$Age, seq(0,1,0.01), na.rm = T)

full$Age[full$Age > 65] <- 67
summary(full$Age)

age <- full$Age
age[which(is.na(age))] <- median(age, na.rm = T)

summary(age)

par(mfrow = c(1,2))
hist(full$Age, col = 'grey', main = 'Original Age')
hist(age, col = 'skyblue', main = 'Imputed Age')

require(mice)
mice_df <- full[,!colnames(full) %in% c('PassengerId','Survived','Ticket')]

set.seed(1)
mice_model <- mice(mice_df, method = 'rf')
mice_data <- complete(mice_model)

hist(full$Age, col = 'grey', main = 'Original Age')
hist(mice_data$Age, col = 'skyblue', main = 'Imputed Age')

par(mfrow = c(1,1))
full$Age <- mice_data$Age
titanic_plot <- ggplot(full[1:891,], aes(fill = factor(Survived)))

titanic_plot + geom_histogram(aes(x = Age), binwidth = 10, col = 'black')

titanic_plot + geom_histogram(aes(x = Age), binwidth = 10, col = 'black', position = 'fill')

# SibSp and Parch
full$relatives <- full$SibSp + full$Parch
titanic_plot <- ggplot(full[1:891,], aes(fill = factor(Survived)))

titanic_plot + geom_histogram(aes(SibSp), binwidth = 1, col = 'black')
titanic_plot + geom_histogram(aes(SibSp), binwidth = 1, col = 'black', position = 'fill')

plot(quantile(full$SibSp, seq(0,1,0.01)))

titanic_plot + geom_histogram(aes(Parch), binwidth = 1, col = 'black')
titanic_plot + geom_histogram(aes(Parch), binwidth = 1, col = 'black', position = 'fill')

plot(quantile(full$Parch, seq(0,1,0.01)))

titanic_plot + geom_histogram(aes(relatives), binwidth = 1, col = 'black')
titanic_plot + geom_histogram(aes(relatives), binwidth = 1, col = 'black', position = 'fill')

plot(quantile(full$relatives, seq(0,1,0.01)))

# Ticket
length(unique(full$Ticket))

tkt_df <- arrange(full[duplicated(full$Ticket) | duplicated(full$Ticket, fromLast = TRUE),],Ticket)

sum(for(i in 1:(nrow(tkt_df)-1)) {
        tkt_df[which(tkt_df$Ticket[i] == tkt_df$Ticket[i+1] &
        tkt_df$Fare[i] != tkt_df$Fare[i+1])]})

full$Ticket <- NULL

# Fare
titanic_plot + geom_histogram(aes(Fare), bins = 10, col = 'black')
titanic_plot + geom_histogram(aes(Fare), bins = 10, col = 'black', position = 'fill')

plot(quantile(full$Fare, seq(0,1,0.01)))
quantile(full$Fare, seq(0,1,0.01))

# Embarked
titanic_plot <- ggplot(full[1:891,], aes(fill = factor(Survived)))
titanic_plot + geom_bar(aes(Embarked))
titanic_plot + geom_bar(aes(Embarked), position = 'fill')

str(full)

#### EDA complete

rm(firstname, mice_data, mice_df, surname, tkt_df,
   age, i, mice_model, titanic_plot)

dev.off()

#### Logistic Regression

# Creating dummy df
require(dummies)
ful_dum <- dummy.data.frame(full)

# Split the data back into a train set and a test set
tr_dum <- ful_dum[1:891,]
ts_dum <- ful_dum[892:1309,]

require(caTools)
set.seed(123)

i <- sample.split(tr_dum$Survived, SplitRatio = 0.8)

trn <- tr_dum[i,]
val <- tr_dum[!i,]

# Build the model 
model_1 <- glm(Survived ~ ., data = trn[,-1], family = 'binomial')
summary(model_1)

require(MASS)
model_2 <- stepAIC(model_1, direction = 'both')

summary(model_2)

require(car)
sort(vif(model_2), decreasing = T)

# EmbarkedC
model_3 <- glm(formula = Survived ~ Pclass1 + Pclass2 + SexFEMALE + Age + 
                 SibSp + Parch + titleMASTER + titleMISS, family = "binomial", 
                 data = trn[, -1])

summary(model_3)

# Parch
model_4 <- glm(formula = Survived ~ Pclass1 + Pclass2 + SexFEMALE + Age + 
                 SibSp + titleMASTER + titleMISS, family = "binomial", 
                 data = trn[, -1])

summary(model_4)

# titleMISS
model_5 <- glm(formula = Survived ~ Pclass1 + Pclass2 + SexFEMALE + Age + 
               SibSp + titleMASTER, family = "binomial", data = trn[, -1])

summary(model_5)

log_model <- model_5


# Prediction on training set
predicted_probability <- predict(log_model,newdata = val, type = "response")
summary(predicted_probability)

actual_survived <- as.factor(val$Survived)

OUT <- matrix(nrow = 100, ncol = 3)
s <- seq(min(predicted_probability), max(predicted_probability), length.out = 100)

require(caret)

cutoff_finder <- function(cutoff) {
  pred_survived <- factor(ifelse(predicted_probability >= cutoff, "1", "0"))
  conf <- confusionMatrix(pred_survived, actual_survived, positive = "1")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- c(sens, spec, acc) 
  return(out) }

for(j in 1:100) {OUT[j,] <- cutoff_finder(s[j])}

cutoff <- s[which.min(abs(OUT[,1]-OUT[,2]))]
cutoff

# Thus, predicting final booking as per cutoff value of predicted probability
pred_survived <- factor(ifelse(predicted_probability >= cutoff, "1", "0"))

con_mat <- confusionMatrix(pred_survived, actual_survived, positive = "1")
con_mat


roc_df <- data.frame(cbind(predicted_probability, val$Survived))

roc_df <- arrange(roc_df, desc(predicted_probability))

colnames(roc_df) <- c("Predicted_Prob","Actual_Labels")


roc_df$Deciles = ntile(1:nrow(helper), 10)

gaintable = roc_df %>% group_by(Deciles)  %>%
            summarise(total = n(), totalresp = sum(Actual_Labels))
        
gaintable <- mutate(gaintable, Cumresp = cumsum(totalresp), Gain = Cumresp/sum(totalresp)*100)


ggplot(gaintable, aes(y = gaintable$Gain, x = gaintable$Deciles)) + 
  geom_line(aes(x = 1:10, y = seq(1, 100, length = 10)), col = 'red', size = 1)


ggplot(gaintable, aes(y = gaintable$Gain, x = gaintable$Deciles)) + 
  geom_line(col = 'green', size = 1)


ggplot(gaintable, aes(y = gaintable$Gain, x = gaintable$Deciles)) + 
  geom_line(col = 'green', size = 1) + 
  geom_line(aes(x = 1:10, y = seq(1, 100, length = 10)), col = 'red', size = 1) + 
  geom_vline(xintercept = 5, linetype = 3, size = 1)



write.csv(roc_df, "roc.csv", row.names = F)


#### RF
full$Survived <- as.factor(full$Survived)

tr.data <- full[1:891, ]
ts.data <- full[892:1309, ]

trn <- tr.data[i, ]
val <- tr.data[!i, ]

# Building the model 
require(randomForest)

rf_model <- randomForest(Survived ~ ., data = trn[, -1], ntree = 1000,
                     proximity = F, do.trace = T, importance = T)

predicted_probability <- predict(rf_model, val[,-2], type = "prob")[,2]
summary(predicted_probability)

s <- seq(min(predicted_probability), max(predicted_probability), length.out = 100)

for(j in 1:100) {OUT[j,] <- cutoff_finder(s[j])}

cutoff <- s[which.min(abs(OUT[,1]-OUT[,2]))]
cutoff

pred_survived <- factor(ifelse(predicted_probability > cutoff, "1", "0"))

confusionMatrix(pred_survived, val$Survived, positive = '1')

ts.data$Survived <- factor(ifelse(predict(rf_model, ts.data, type = "prob")[,2] > cutoff, "1", "0"))
table(ts.data$Survived)

write.csv(ts.data[, c(1,2)], "sub_rf.csv", row.names = F)


#### SVM
require(e1071)
require(caret)

tr_dum$Survived <- as.factor(tr_dum$Survived)

trn <- tr_dum[i, ]

val <- tr_dum[!i, ]

svm_lnr <- svm(Survived ~ ., data = trn[,-1], kernel = 'linear')

pred_sur <- predict(svm_lnr, val[,-c(1,2)])

confusionMatrix(pred_sur, val$Survived, positive = '1')

svm_pol <- svm(Survived ~ ., data = trn[,-1], kernel = 'polynomial')

pred_sur <- predict(svm_pol, val[,-c(1,2)])

confusionMatrix(pred_sur, val$Survived, positive = '1')

svm_rad <- svm(Survived ~ ., data = trn[,-1], kernel = 'radial')
pred_sur <- predict(svm_rad, val[,-c(1,2)])
confusionMatrix(pred_sur, val$Survived, positive = '1')

ts_dum$Survived <- predict(svm_lnr, newdata = ts_dum)

table(ts_dum$Survived)

submission <- ts_dum[, c(1,2)]

write.csv(submission, "sub1.csv", row.names = T)


