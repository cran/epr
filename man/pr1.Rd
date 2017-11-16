\name{pr1}
\alias{pr1}

\title{
Analysis of polynomial regression
}

\description{

The function performs analysis of polynomial regression in simple designs with quantitative treatments. The function also performs with randon factor in mixed models.

}

\usage{

pr1(data, mixed = FALSE, digits = 6)

}

\arguments{

  \item{data}{
data is a data.frame

The first column should contain the treatments (explanatory variable) and the remaining columns the response variables (fixed model). 

The first column should contain the treatments (explanatory variable), second colunm should contais de random variable and the remaining columns the response variables (mixed model). 

}
  \item{mixed}{
FALSE = fixed model 

TRUE = mixed model
}
  \item{digits}{

6 = defalt (number of digits)

}
}

\value{

Returns coefficients of the models, t test for coefficients, R squared, adjusted R squared, AIC, BIC and the maximum (or minimum) values of y and critical point of x, residuals and normality test.
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

# analysis in completely randomized design
data(data1)
r1=pr2(data1)
names(r1)
r1
r1[1]

pr1(data1)

# analysis in randomized block design
data(data2)
r2=pr2(data2, design=2)
r2

pr1(data2, mixed=TRUE)


}

