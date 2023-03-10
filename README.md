# Pima Diabetes Prediction

```{r, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, warning=FALSE, message=FALSE)
```

## Background

The Pima Indians data set was originally produced by the National Institute of Diabetes and Digestive and Kidney Diseases. The goal of this data is to diagnostically predict whether or not a patient has diabetes, based on certain diagnostic measurements included in the data set. Several constraints were placed on the selection of these instances from a larger data base: particularly, all patients are females at least 21 years old of Pima Indian heritage.

```{r}
library(tidyverse)
library(mlbench)
data(PimaIndiansDiabetes)
head(PimaIndiansDiabetes)
```

Variable|Description
-------|---------
`pregnant`|	Number of times pregnant
`glucose`|	Plasma glucose concentration (glucose tolerance test)
`pressure`|	Diastolic blood pressure (mm Hg)
`triceps`|	Triceps skin fold thickness (mm)
`insulin`|	2-Hour serum insulin (mu U/ml)
`mass`|	Body mass index (weight in kg/(height in m)\^2)
`pedigree`|	Diabetes pedigree function
`age`|	Age (years)
`diabetes`|	Class variable (test for diabetes)

## Exploratory Analysis

I will build two machine learning models to classify patients as positive or negative for diabetes, and compare their performance.

First, I will build a testing and training data set using a 70-30 split.

```{r}
library(caret)
set.seed(366)
trainIndex <- createDataPartition(PimaIndiansDiabetes$diabetes, p=0.7, list=FALSE, times=1)
train <- PimaIndiansDiabetes[trainIndex,]
nrow(train)
test <- PimaIndiansDiabetes[-trainIndex,]
nrow(test)
```

I will make a scatterplot of glucose and blood pressure using the training data, using the plotting color to indicate whether or not a person in the training data has diabetes. 

```{r}
train %>% ggplot(aes(x=glucose, y=pressure)) + 
  geom_point(aes(color=diabetes)) + 
  labs(x='Glucose', y='Blood Pressure')
```

Based on the plot, glucose and blood pressure may be useful input variables. Higher glucose levels tend to correspond to more positive diabetes tests, while lower glucose levels tend to correspond to more negative diabetes tests. This indicates that high glucose lvels may be correlated with diabetes. The blood pressures of patients who tested positive for diabetes occupy a wide range, while the blood pressures of patients who tested negative for diabetes are more concentrated, leading me to believe that it is possible that abnormal (higher or lower than average) blood pressures may be correlated with diabetes. 

## Logistic Regression 

Now, I will use the training data to build a logistic regression model. I am choosing to build a logistic regression model because my goal is to predict a categorical dependent variable.

```{r}
model = train(form=diabetes~., data=train, method="glm", family="binomial")
summary(model)
```

Pregnancy, glucose, blood pressure, mass, and pedigree are the statistically significant variables in the model, indicated by their relatively small p-values. Thus, according to the model, these variables likely play a role in predicting whether or not a patient has diabetes.

I will now look into the accuracy of the model.

```{r}
model
```

For the training data, the accuracy of the model is 0.7691 and the kappa of the model is 0.4650. An accuracy rate of 76.91% means that the model correctly classified diabetes 76.91% of the time, and the kappa of 46.50% means that, excluding random chance, the model correctly classified diabetes 46.50% of the time. 

Next, I will create a confusion matrix for the logistic regression model using the testing data. 

```{r}
confusionMatrix(data=predict(model, test), reference=test$diabetes)
```

The confusion matrix tells me that the model's true negative rate is 56.09%, found by dividing the 129 observations that were predicted to be negative and were actually negative by the 230 total observations in the testing data set. In context, this means that this model would predict that 56.09% of patients without diabetes do not have diabetes. It also tells me that the true positive rate is 21.3%, found by diving the 49 observations that were predicted to be positive and were actually positive by the 230 total observations in the testing data set. In context, this means that this model would "diagnose" 21.3% of diabetic patients as diabetic. In other words, if this model took the place of a doctor, it would only "recommend" diabetes care to 21.3% of patients who actually had diabetes, which is rather startling! 

## Naive Bayes Classification

Now, I will use the training data to build a naive Bayes classification model. This model works by assuming that all of the predictor variables are independent.

```{r}
library(naivebayes)
model_nb = train(form=diabetes~., data=train, method="naive_bayes")
summary(model_nb)
```

The "a priori" probabilities in the model are 65.06% negative and 34.94% positive. This means there is a 65.06% chance of having diabetes and a 34.94% chance of not having diabetes, according to this model.

```{r}
model_nb
```

The accuracy for this model is 0.7609 and the kappa for this model is 0.4615 The accuracy rate of 76.09% means that the model correctly classified diabetes 76.09% of the time, and the kappa of 46.15% means that, excluding random chance, the model correctly classified diabetes 46.15% of the time.  

I will also create a confusion matrix for the naive Bayes classification model using the testing data.

```{r}
confusionMatrix(data=predict(model_nb, test), reference=test$diabetes)
```

The true negative rate is 52.2%, found by dividing the 120 observations that were predicted to be negative and were actually negative by the 230 total observations in the testing data set. The true positive rate is 22.2%, found by dividing the 51 observations that were predicted to be positive and were actually positive by the 230 observations in the testing data set. 

## Summary

Both of the models I considered were extremely similar in their accuracy, so I wouldn't say that either one is terribly better than the other. With that being said, if I had to choose the "best" model for this scenario I would choose the naive Bayes model. I say this because it has a slightly higher true positive rate, which means that it would be slightly better than the logistic regression model at letting people who have diabetes know that they have diabetes so that they can get treatment.

Overall, I certainly would not trust either of these models to be my doctor!
