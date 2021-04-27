library("dplyr")

#Set Working Directory
setwd("C:/Users/elian/Documents/DSR")

#Read a tab-delimited data file
cars <- read.table(
  file = "Cars.txt",
  header = TRUE,
  sep = "\t", #esto le indica que esta separado por tabs
  quote = "\"" #esto le dice a R que cualquier cosa entre comillas debe ser interpretado como texto, incluido caracteres especiales
)
head(cars)

#Select a subset of columns
temporary <- select(
  .data = cars,
  Transmission,
  Cylinders,
  Fuel.Economy
)
#Inspect the results
head(temporary)

#Filter a subset of rows
temporary <- filter(
  .data = temporary,
  Transmission == "Automatic"
)
head(temporary)

#Compute a new column
temporary <- mutate(
  .data = temporary, #sets the parameter as 'temporary'
  Consumption = Fuel.Economy * 0.425
)
head(temporary)

#Group by a column
temporary <- group_by(
  .data = temporary,
  Cylinders
)
head(temporary)

#Aggregate based on groups
temporary <- summarize(
  .data = temporary,
  Avg.Consumption = mean(Consumption))

head(temporary)

#Arrange the rows in descending order
temporary <- arrange(
  .data = temporary,
  desc(Avg.Consumption)
)
head(temporary)

#Convert tibble to data frame
efficiency <- as.data.frame(temporary)

#inspect the result
print(efficiency)

#Chain methods together
efficiency <- cars %>%
  select(Fuel.Economy,Cylinders, Transmission) %>%
  filter(Transmission == "Automatic") %>%
  mutate(Consumption = Fuel.Economy * 0.425)

cars <- read.csv("Cars.csv")
head(cars)

#Create a frequency table
table(cars$Transmission)

#Get the minimum value
min(cars$Fuel.Economy)

#Get the maximum value
max(cars$Fuel.Economy)

#Get the average value
mean(cars$Fuel.Economy)

#Get the median calue
median(cars$Fuel.Economy)

#Get the quartiles minimo-primer quartil-mediana-tercer quartil-maximo
quantile(cars$Fuel.Economy)

#Get the standard deviation
sd(cars$Fuel.Economy)

#Get the total value
sum(cars$Fuel.Economy)

#Get the correlation coefficient

cor(
  x=cars$Cylinders,
  y=cars$Fuel.Economy
)

#Summarize an entire table
summary(cars)


library(ggplot2)

ggplot(
  data = cars,
  aes(x = Transmission))+ #aesthetics of the chart
  geom_bar()+
  ggtitle("Count of cars Transmission Type")
  xlab("Transmission Type")
  ylab("Count of the Cars")
  
ggplot(
  data = cars,
  aes(x = Fuel.Economy)) +
  geom_histogram(
    bins = 10) +
  ggtitle("Dsitribution of Fuel Economy") +
  xlab("Fuel Economy (mpg)") +
  ylab("Count of Cars")

#Create a density plot
ggplot(
  data = cars,
  aes(x = Fuel.Economy)) +
  geom_density() +
  ggtitle("Dsitribution of Fuel Economy") +
  xlab("Fuel Economy (mpg)") +
  ylab("Density")

#Create a scatterplot

ggplot(
  data = cars,
  aes(x = Cylinders,
      y = Fuel.Economy)) +
  geom_point() +
  ggtitle("Fuel Economy by Cylinders") +
  xlab("Number of cylinders") +
  ylab("Fuel Economy (mpg)")
