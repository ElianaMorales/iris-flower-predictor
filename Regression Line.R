data("iris")
head(iris)

#Create a linear regression model
model <- lm(
  formula = Petal.Width ~ Petal.Length,
  data = iris)

#Summarize the model
summary(model)

#Draw a regression line on plot
lines(
  x = iris$Petal.Length,
  y = model$fitted.values,
  col = "blue",
  lwd = 3)

#Get the correlation coefficient
cor(
  x = iris$Petal.Length,
  y = iris$Petal.Width)