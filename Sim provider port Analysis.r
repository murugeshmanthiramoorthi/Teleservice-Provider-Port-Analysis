# Let us load the dataset
df=read.csv("switching.csv")

# We will perform logistic regression to identify the relationship between variables
r1 <- glm(SwBh~Age+MF+ProfessionBusiness+UrbanRural+RecipientOperator+
            Sat+Servqual+PSB+ESB+OSB+Price,data=df,binomial(link="logit"))
summary(r1)

# Let us now generate a predictive model to perform the analysis
a=sample(1:dim(df)[1],120)
train=df[-a,]
test=df[a,]

# Performing Logistic Regression on the train data
r2 <- glm(SwBh~Age+MF+ProfessionBusiness+UrbanRural+RecipientOperator+
            Sat+Servqual+PSB+ESB+OSB+Price,data=train,binomial(link="logit"))
summary(r2)

# Let us view the step by step analysis of logistic regression
r3 <- step(r2)
summary(r3)

# Now we will predict the choice of the customers for the test data
predictions<-predict(r3,newdata=test,type="response")
predicted<-ifelse(predictions<mean(test$SwBh),0,1)

# Now we will build a confusion matrix to analyse the performance of the model
actual=test$SwBh
table(predicted,actual)