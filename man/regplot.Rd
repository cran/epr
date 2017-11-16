\name{regplot}
\alias{regplot}

\title{
Graphics of the regression
}
\description{
The function generates the scatter plot with the regression equation.
}
\usage{

regplot(data, xlab="Explanatory Variable", ylab="Response Variable", 
position=6, mean=TRUE, digits=4)

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
  
  \item{mean}{

TRUE = scatter plots with averages (default)

FALSE = scatter plots with all data
}
  
 \item{digits}{

number of digits
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

d1=data5[,c(1,2)]
regplot(d1, position=8)

d2=data5[,c(1,3)]
regplot(d2, position=8)

d3=data5[,c(1,4)]
regplot(d3, position=8)


}

