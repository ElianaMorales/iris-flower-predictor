library(caret)
library(ggplot2)
library(tree)
data("iris")
head(iris)

#Set a seed to make randomness reproducable
set.seed(42)

#Randomly sample 100 of 150 row indexes
indexes <- sample(
  x = 1:150,
  size = 100
)
#Inspect the random indexes
print(indexes)

#Create a training set from indexes
train <- iris[indexes, ]

#Create a test set from remaining indexes
test <- iris[-indexes,]
library(tree)

#Train a decision tree model
model <- tree(
  formula = Species ~ ., #el punto incluye todos los datos en df
  data = train
)

#Inspect the model
summary(model)
#Visualize the decision tree model
plot(model)
title("Decision Tree")
text(model)
library(RColorBrewer)

#Create a color palette
palette <- brewer.pal(3, "Set2")

#Crate a scatterplot colored by species
plot(
  x = iris$Petal.Length,
  y = iris$Petal.Width,
  pch = 19,
  col = palette[as.numeric(iris$Species)],
  main = "Iris Petal Lenght vs. Width",
  xlab = "Petal Length (cm)",
  ylab = "Petal Width (cm)"
)

#Decision tree
gg.partition.tree <- function (tree, label = "yval", ordvars, ...) 
{
  ptXlines <- function(x, v, xrange, xcoord = NULL, ycoord = NULL, 
                       tvar, i = 1L) {
    if (v[i] == "<leaf>") {
      y1 <- (xrange[1L] + xrange[3L])/2
      y2 <- (xrange[2L] + xrange[4L])/2
      return(list(xcoord = xcoord, ycoord = c(ycoord, y1, 
                                              y2), i = i))
    }
    if (v[i] == tvar[1L]) {
      xcoord <- c(xcoord, x[i], xrange[2L], x[i], xrange[4L])
      xr <- xrange
      xr[3L] <- x[i]
      ll2 <- Recall(x, v, xr, xcoord, ycoord, tvar, i + 
                      1L)
      xr <- xrange
      xr[1L] <- x[i]
      return(Recall(x, v, xr, ll2$xcoord, ll2$ycoord, tvar, 
                    ll2$i + 1L))
    }
    else if (v[i] == tvar[2L]) {
      xcoord <- c(xcoord, xrange[1L], x[i], xrange[3L], 
                  x[i])
      xr <- xrange
      xr[4L] <- x[i]
      ll2 <- Recall(x, v, xr, xcoord, ycoord, tvar, i + 
                      1L)
      xr <- xrange
      xr[2L] <- x[i]
      return(Recall(x, v, xr, ll2$xcoord, ll2$ycoord, tvar, 
                    ll2$i + 1L))
    }
    else stop("wrong variable numbers in tree.")
  }
  if (inherits(tree, "singlenode")) 
    stop("cannot plot singlenode tree")
  if (!inherits(tree, "tree")) 
    stop("not legitimate tree")
  frame <- tree$frame
  leaves <- frame$var == "<leaf>"
  var <- unique(as.character(frame$var[!leaves]))
  if (length(var) > 2L || length(var) < 1L) 
    stop("tree can only have one or two predictors")
  nlevels <- sapply(attr(tree, "xlevels"), length)
  if (any(nlevels[var] > 0L)) 
    stop("tree can only have continuous predictors")
  x <- rep(NA, length(leaves))
  x[!leaves] <- as.double(substring(frame$splits[!leaves, "cutleft"], 
                                    2L, 100L))
  m <- model.frame(tree)
  if (length(var) == 1L) {
    x <- sort(c(range(m[[var]]), x[!leaves]))
    if (is.null(attr(tree, "ylevels"))) 
      y <- frame$yval[leaves]
    else y <- frame$yprob[, 1L]
    y <- c(y, y[length(y)])
    if (add) 
      lines(x, y, type = "s", ...)
    else {
      a <- attributes(attr(m, "terms"))
      yvar <- as.character(a$variables[1 + a$response])
      xo <- m[[yvar]]
      if (is.factor(xo)) 
        ylim <- c(0, 1)
      else ylim <- range(xo)
      plot(x, y, ylab = yvar, xlab = var, type = "s", ylim = ylim, 
           xaxs = "i", ...)
    }
    invisible(list(x = x, y = y))
  }
  else {
    if (!missing(ordvars)) {
      ind <- match(var, ordvars)
      if (any(is.na(ind))) 
        stop("unmatched names in vars")
      var <- ordvars[sort(ind)]
    }
    lab <- frame$yval[leaves]
    if (is.null(frame$yprob)) 
      lab <- format(signif(lab, 3L))
    else if (match(label, attr(tree, "ylevels"), nomatch = 0L)) 
      lab <- format(signif(frame$yprob[leaves, label], 
                           3L))
    rx <- range(m[[var[1L]]])
    rx <- rx + c(-0.025, 0.025) * diff(rx)
    rz <- range(m[[var[2L]]])
    rz <- rz + c(-0.025, 0.025) * diff(rz)
    xrange <- c(rx, rz)[c(1, 3, 2, 4)]
    xcoord <- NULL
    ycoord <- NULL
    xy <- ptXlines(x, frame$var, xrange, xcoord, ycoord, 
                   var)
    xx <- matrix(xy$xcoord, nrow = 4L)
    yy <- matrix(xy$ycoord, nrow = 2L)
    return(
      list(
        annotate(geom="segment", x=xx[1L, ], y=xx[2L, ], xend=xx[3L, ], yend=xx[4L, ]),
        annotate(geom="text", x=yy[1L, ], y=yy[2L, ], label=as.character(lab), ...)
      )
    )
  }
}

#Plot the decision boundaries
ggplot(iris, 
       aes(Petal.Width, Sepal.Width, color=Species)) + 
  geom_point() +
  gg.partition.tree(tree(Species ~ Sepal.Width + Petal.Width, data=iris), 
                    label="Species", color = "purple") 
#Predict with the model
predictions <- predict(
  object = model,
  newdata = test,
  type = "class" #for classification
)

#Create a confusion matrix
table(
  x = predictions,
  y = test$Species
)



#Clasification and regression training
 #Evaluate the prediction results
confusionMatrix(
  data = predictions,
  reference = test$Species
)
head(model)
setwd("C:/Users/elian/Documents/DSR")
#Save the tree model
save(model, file = "Tree.RData")