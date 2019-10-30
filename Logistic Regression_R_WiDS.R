# Logistic Regression - WiDS

# Define the problem 
# set the working directory 
# Load the libraries 
# Acquire the data 
# ingest the data 
# Explore the data 
# Data Clean and Data preparation 
# Scale the data 
# split the data into test and train 
# train the model and test it 
# Validate the model 


setwd("C:/Users/agupta82/Desktop/WiDS")

library(data.table)
library(caret)
library(ggplot2)

base_data <- fread("Predicting_MS_admission/binary.csv")

summary(base_data)
str(base_data)

# Check Missing 
colSums(is.na(base_data))

# Outlier Treatment 
# Feature Engeering 


ggplot(data=base_data[,.N/400,admit]) + 
  geom_bar(aes(y = V1, x = admit), stat = "identity") + 
  labs(x = "Admit", y = "% Students") + 
  scale_y_continuous(labels = scales::percent) + 
  scale_x_discrete(limits = c(0,1), labels = c("No","Yes")) + 
  geom_text(data = base_data[,.N/400,admit], aes(x= admit , y =  V1 + 0.04, label = scales::percent(V1)))

ggplot(data=base_data) + 
  geom_histogram(aes(x = gpa), bins = 50) +
  labs(x = "GPA Score", y = "# Students") 

ggplot(data=base_data) + 
  geom_bar(aes(x = rank)) +
  labs(x = "Rank", y = "# Students") 

# check coorelation 
correl <- cor(base_data)
high_cor_vars <- findCorrelation(correl, cutoff = .7, names = TRUE)
#base_data[, (high_cor_vars) := NULL]


base_data$admit = as.factor(base_data$admit)
#base_data$rank = as.factor(base_data$rank)

cols = names(dplyr::select_if(base_data,is.numeric))
base_data[, (cols) := lapply(.SD,scale), .SDcols = cols]

# split test/train
set.seed(1001)
train_index <- createDataPartition(base_data$admit, p = .7, list = F)
train_dat <- base_data[train_index]
test_dat  <- base_data[-train_index]

m_log <- glm(admit ~ gre + gpa + rank, data = train_dat, family = "binomial")
summary(m_log)

pred_test <- predict(m_log,test_dat,type = "response")
pred_train <- predict(m_log,train_dat,type = "response")

(cm <- confusionMatrix(data = factor(as.integer(pred_train >= .5)), 
                       reference = factor(train_dat$admit), 
                       positive = "1"))

(cm <- confusionMatrix(data = factor(as.integer(pred_test >= .5)), 
                       reference = factor(test_dat$admit), 
                       positive = "1"))


library(pROC)
(roc <- roc(train_dat$admit, pred_train))
(roc <- roc(test_dat$admit, pred_test))

plot(roc, print.thres="best", print.thres.best.method="closest.topleft")
