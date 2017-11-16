\name{bl}
\alias{bl}

\title{
Analysis of bronken line regression
}
\description{
The function performs analysis of broken line regression. 
}
\usage{
bl(data, xlab="Explanatory Variable", ylab="Response Variable", position=1)
}

\arguments{
  \item{data}{

data is a data.frame

The first column should contain the treatments (explanatory variable) and the second column the response variable

}

  \item{xlab}{

name of explanatory variable

}

  \item{ylab}{

name of response variable

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

}

\value{
Returns coefficients of the models, t test for coefficients, R squared, adjusted R squared, AIC and BIC, normality test and residuals.
}

\author{
Emmanuel Arnhold <emmanuelarnhold@yahoo.com.br>
}




\seealso{

lm, ea1(easyanova package), pr2, regplot

}

\examples{

x=c(0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08,0.09,0.10)
y=c(5.5,4,3.2,2.1,1,0.1,1.6,2.2,3,5)
y=y/100
data=data.frame(x,y)

### bl(data)



}

