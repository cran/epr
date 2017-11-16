\name{r.test}
\alias{r.test}

\title{
Tests for model identity and parameter
}
\description{
The function performs tests of parameters and models.
}
\usage{
r.test(data, digits=6)
}

\arguments{
  \item{data}{
data is a data.frame
The first column should contain the x (explanatory variable) second treatments and the remaining columns the response variables.
}

  \item{digits}{
number of digits (defalt = 6)
}  


}

\value{
Returns coefficients of the models, t test for coefficients and tests for parameters and models.
}

\author{
Emmanuel Arnhold <emmanuelarnhold@yahoo.com.br>
}



\seealso{
lm, ea1(easyanova package), pr2, regplot
}
\examples{
x=c(1,1,1,2,2,2,3,3,3,4,4,4)
y=c(5,5.3,6,8,8.9,12,14,18,25,25,29,32)
t=c("a1","a2","a3","a1","a2","a3","a1","a2","a3","a1","a2","a3")
data=data.frame(x,t,y)

r.test(data)


}

