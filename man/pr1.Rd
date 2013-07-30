\name{pr1}
\alias{pr1}

\title{
Analysis of polynomial regression
}
\description{
The function performs analysis of polynomial regression in simple designs with quantitative treatments. 
The function also performs model fits with plateaus (plateaus linear and quadratic).
}
\usage{
pr1(data, plateau = FALSE, x.plateau = NULL)
}

\arguments{
  \item{data}{
data is a data.frame

The first column should contain the treatments (explanatory variable) and the remaining columns the response variables.
}
  \item{plateau}{
FALSE = function returns the linear and quadratic

TRUE = function returns the linear, quadratic, linear.plateau and quadratic.plateau
}
  \item{x.plateau}{
NULL = starting value for the linear plateau will be the point of maximum (or minimum) of the quadratic equation
}
}

\value{
Returns coefficients of the models, t test for coefficients, R squared, adjusted R squared, AIC, BIC and the maximum (or minimum) values of y and critical point of x.
}

\author{
Emmanuel Arnhold <emmanuelarnhold@yahoo.com.br>
}




\seealso{
lm, ea1(easyanova package), pr2, regplot
}
\examples{
# data
data(data5)

# linear and quadratic models
results1=pr1(data5)
results1

# including plateaus models
results2=pr1(data5, plateau=TRUE)
results2

}

