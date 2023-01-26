library(tidyverse)
library(mlbench)
data(PimaIndiansDiabetes)
head(PimaIndiansDiabetes)

library(caret)
set.seed(366)
trainIndex <- createDataPartition(PimaIndiansDiabetes$diabetes, p=0.7, list=FALSE, times=1)
train <- PimaIndiansDiabetes[trainIndex,]
nrow(train)
test <- PimaIndiansDiabetes[-trainIndex,]
nrow(test)

train %>% ggplot(aes(x=glucose, y=pressure)) + 
  geom_point(aes(color=diabetes)) + 
  labs(x='Glucose', y='Blood Pressure')

model = train(form=diabetes~., data=train, method="glm", family="binomial")
summary(model)

model

confusionMatrix(data=predict(model, test), reference=test$diabetes)

library(naivebayes)
model_nb = train(form=diabetes~., data=train, method="naive_bayes")
summary(model_nb)

model_nb

confusionMatrix(data=predict(model_nb, test), reference=test$diabetes)
