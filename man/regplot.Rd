\name{regplot}
\alias{regplot}

\title{
Regression graphics
}
\description{
The function generates the scatter plot with the regression equation.
}
\usage{
regplot(data, xlab = NULL, ylab = NULL, poly = 1, position = 6, colors = TRUE, 
mean = TRUE, variable = 1, x.plateau = NULL)
}

\arguments{
  \item{data}{

data is a data.frame

the first column contain the explanatory variable

the others columns contain the responses variables

}
  \item{xlab}{
name of variable x
}
  \item{ylab}{
name of variable y
}
  \item{poly}{

indicates which regression plot

1 = linear (default)

2 = quadratic

3 = linear.plateau

4 = quadratic.plateau

}
  \item{position}{
position of equation in the graph

top=1 

bottomright=2

bottom=3

bottomleft=4

left=5

topleft=6 (default)

topright=7

right=8

center=9
}
  \item{colors}{

TRUE = the line is red (default)

FALSE = the line is black
}
  \item{mean}{

TRUE = scatter plots with averages (default)

FALSE = scatter plots with all data
}
  \item{variable}{

1 = second column of data.frame (default)

2 = third column of data.frame

see examples
}
  \item{x.plateau}{
default is NULL = starting value for the linear plateau will be the point of maximum (or minimum) of the quadratic equation
}
}

\value{
The function generates the scatter plot with the regression equation.
}

\author{
Emmanuel Arnhold <emmanuelarnhold@yahoo.com.br>
}

\seealso{
lm, lme, ea1(easyanova package), pr2, pr2, dplot(ds package)
}
\examples{

# data
data(data5)

# first response variable 
par(mfrow=c(2,2))
regplot(data5, "Variable X", "Variable Y")
regplot(data5, "Variable X", "Variable Y", poly=2)
regplot(data5, "Variable X", "Variable Y", poly=3)
regplot(data5, "Variable X", "Variable Y", poly=4)

# second response variable 
par(mfrow=c(2,2))
regplot(data5, "Variable X", "Variable Y", variable=2)
regplot(data5, "Variable X", "Variable Y", variable=2, poly=2)
regplot(data5, "Variable X", "Variable Y", variable=2,  poly=3)
regplot(data5, "Variable X", "Variable Y", variable=2, poly=4)

# third response variable 
par(mfrow=c(2,2))
regplot(data5, variable=3, colors=FALSE, position=4, mean=FALSE)
regplot(data5, variable=3, poly=2, mean=FALSE)
regplot(data5, variable=3,  poly=3, mean=FALSE)
regplot(data5, variable=3, poly=4, mean=FALSE)

# data
treatments=rep(c(0.5,1,1.5,2,2.5,3), c(3,3,3,3,3,3))
r1=rnorm(18,60,3)
r2=r1*1:18
r3=r1*18:1
r4=r1*c(c(1:10),10,10,10,10,10,10,10,10)
data6=data.frame(treatments,r1,r2,r3, r4)

# graphs
regplot(data6,variable=1, poly=2)
regplot(data6,variable=2, poly=1)
regplot(data6,variable=3, poly=1)
regplot(data6,variable=4, poly=4)
regplot(data6,variable=4, poly=3)
}

