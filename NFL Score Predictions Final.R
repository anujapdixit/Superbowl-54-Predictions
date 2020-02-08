setwd("C://Users//anuja//OneDrive//Desktop//Spring Quarter//Forecasting//NFL")
df = read.csv("NFL 2018-19 Regular Season Data.csv")
head(df)
str(df)
summary(df)

df <- df[ -c(2:3) ]
head(df)
colnames(df)

names(df) <- c("Team1","Loc","Team2","Team1_Score","Team2_Score","Team1_Passing", "Team1_Rushing","Team1_Turnovers","Team2_Passing","Team2_Rushing","Team2_Turnovers","Result")

colnames(df)

install.packages("ggplot2")
library(ggplot2)
ggplot(df, aes(x = Team1_Score))+geom_histogram(binwidth = 3, aes(fill = ..count..))+ggtitle("Team 1 Score Histogram")
ggplot(df, aes(x = Team2_Score))+geom_histogram(binwidth = 3, aes(fill = ..count..))+ggtitle("Team 2 Score Histogram")
ggplot(df, aes(x = Team1_Turnovers, y = Team1_Score)) + geom_point(alpha = 0.5,size = 2, aes(color = Result))
#ggplot(df, aes(x = Team2_Turnovers, y = Team2_Score)) + geom_point(alpha = 0.5, size = 2, aes(color = Result))
ggplot(df, aes(x=Result)) + geom_bar(aes(fill = Loc), position = "dodge")

ggplot(df, aes(x=Result, y = Team1_Passing))+geom_boxplot(aes(fill = Loc))

pl <- ggplot(df, aes(x=Team1)) + geom_bar(aes(fill = Result)) + coord_flip() + xlab("Teams") + ylab("Count")+ggtitle("Win Loss Table for Teams")

install.packages("plotly")
library(plotly)
gpl <- ggplotly(pl)
print(gpl)


library(caTools)
set.seed(101)
split <- sample.split(df$Result, SplitRatio = 0.70)
final.train <- subset(df, split == TRUE)
final.test <- subset(df, split == FALSE)

mlm1 <- lm(cbind(Team1_Score, Team2_Score) ~ .- Result, data = final.train)
summary(mlm1)

head(resid(mlm1))
coef(mlm1)

predicted <- predict(mlm1,final.test)
predicted


newdata = data.frame(Team1="Kansas City Chiefs", Loc = "HOME", Team2 = "San Francisco 49ers", Team1_Passing = 239, Team1_Rushing = 116, Team1_Turnovers = 1, Team2_Passing= 233 , Team2_Rushing = 114, Team2_Turnovers=1, Result = "W")
newdata
str(newdata)

newdata$Team1_Passing <- as.integer(newdata$Team1_Passing)
newdata$Team1_Rushing <- as.integer(newdata$Team1_Rushing)
newdata$Team1_Turnovers <- as.integer(newdata$Team1_Turnovers)
newdata$Team2_Passing <- as.integer(newdata$Team2_Passing)
newdata$Team2_Rushing <- as.integer(newdata$Team2_Rushing)
newdata$Team2_Turnovers <- as.integer(newdata$Team2_Turnovers)
str(newdata)

predict(mlm1, newdata)

str(df)

newdata1 = data.frame(Team1="San Francisco 49ers", Loc = "HOME", Team2 = "Kansas City Chiefs", Team1_Passing = 233, Team1_Rushing = 114, Team1_Turnovers = 1, Team2_Passing= 239 , Team2_Rushing = 116, Team2_Turnovers=1, Result ="W" )
newdata1
str(newdata1)

newdata1$Team1_Passing <- as.integer(newdata1$Team1_Passing)
newdata1$Team1_Rushing <- as.integer(newdata1$Team1_Rushing)
newdata1$Team1_Turnovers <- as.integer(newdata1$Team1_Turnovers)
newdata1$Team2_Passing <- as.integer(newdata1$Team2_Passing)
newdata1$Team2_Rushing <- as.integer(newdata1$Team2_Rushing)
newdata1$Team2_Turnovers <- as.integer(newdata1$Team2_Turnovers)
str(newdata1)

predict(mlm1, newdata1)



install.packages("randomForest")
library(randomForest)

forest <- randomForest(Result ~ ., data = final.train, importance = TRUE)


forest1 <- randomForest(Result ~ ., data = final.train)
predicted <- predict(forest,final.test)
predicted1 <- predict(forest1,final.test)

table(final.test$Result, predicted)

forest$confusion
forest1$confusion
forest$importance

newdata = data.frame(Team1="Kansas City Chiefs", Loc = "HOME", Team2 = "San Francisco 49ers", Team1_Score = 24,Team2_Score= 23, Team1_Passing = 239, Team1_Rushing = 116, Team1_Turnovers = 1, Team2_Passing= 233 , Team2_Rushing = 114, Team2_Turnovers=1, Result ="")
newdata
str(newdata)

newdata$Team1_Score <- as.integer(newdata$Team1_Score)
newdata$Team2_Score <- as.integer(newdata$Team2_Score)
newdata$Team1_Passing <- as.integer(newdata$Team1_Passing)
newdata$Team1_Rushing <- as.integer(newdata$Team1_Rushing)
newdata$Team1_Turnovers <- as.integer(newdata$Team1_Turnovers)
newdata$Team2_Passing <- as.integer(newdata$Team2_Passing)
newdata$Team2_Rushing <- as.integer(newdata$Team2_Rushing)
newdata$Team2_Turnovers <- as.integer(newdata$Team2_Turnovers)
str(newdata)

xtest <- rbind(final.train[1, ] , newdata)
xtest <- xtest[-1,]
predict(forest, xtest)

str(df)

newdata1 = data.frame(Team1="San Francisco 49ers", Loc = "HOME", Team2 = "Kansas City Chiefs", Team1_Score = 23,Team2_Score= 24, Team1_Passing = 233, Team1_Rushing = 114, Team1_Turnovers = 1, Team2_Passing= 239 , Team2_Rushing = 116, Team2_Turnovers=1, Result ="" )
newdata1
str(newdata1)

newdata1$Team1_Score <- as.integer(newdata1$Team1_Score)
newdata1$Team2_Score <- as.integer(newdata1$Team2_Score)
newdata1$Team1_Passing <- as.integer(newdata1$Team1_Passing)
newdata1$Team1_Rushing <- as.integer(newdata1$Team1_Rushing)
newdata1$Team1_Turnovers <- as.integer(newdata1$Team1_Turnovers)
newdata1$Team2_Passing <- as.integer(newdata1$Team2_Passing)
newdata1$Team2_Rushing <- as.integer(newdata1$Team2_Rushing)
newdata1$Team2_Turnovers <- as.integer(newdata1$Team2_Turnovers)
str(newdata1)

xtest1 <- rbind(final.train[1, ] , newdata1)
xtest1 <- xtest1[-1,]
predict(forest, xtest1)

colnames(final.train)

final.log.model <- glm(Result ~ ., family = binomial, data = final.train, maxit =100)
summary(final.log.model)
fitted.probabilities <- predict(final.log.model, final.test, type= 'response')
fitted.results <- ifelse(fitted.probabilities > 0.5,1,0)

#CONFUSION MATRIX
table(final.test$Result, fitted.probabilities > 0.5)

pred <- predict(final.log.model, xtest, type= 'response')
res <- ifelse(pred > 0.5,1,0)
res

pred1 <- predict(final.log.model, xtest1, type= 'response')
res1 <- ifelse(pred1 > 0.5,1,0)
res1

pred <- predict(final.log.model,xtest)
pred1 <- predict(final.log.model,xtest1) 

probs <- exp(pred)/(1+exp(pred))
probs

probs1 <- exp(pred1)/(1+exp(pred1))
probs1

