\name{pr}
\alias{pr}

\title{
Analysis of polynomial regression
}
\description{
The function performs analysis of polynomial regression in simple designs with quantitative treatments.
}
\usage{
pr(data)
}

\arguments{
  \item{data}{

data is a data.frame

data frame with two columns, treatments and response (completely randomized design)

data frame with three columns, treatments, blocks and response (randomized block design)

data frame with four columns, treatments, rows, cols and response (latin square design)

data frame with five columns, treatments, square, rows, cols and response (several latin squares)

}
}
\details{
The response  and the treatments must be numeric. Other variables can be numeric or factors.
}
\value{
Returns analysis of variance, models, t test for coefficients and R squared and adjusted R squared.
}
\references{

KAPS, M. and LAMBERSON, W. R. Biostatistics for Animal Science: an introductory text. 2nd Edition. CABI Publishing, Wallingford, Oxfordshire, UK, 2009. 504p.

SAMPAIO, I. B. M. Estatistica aplicada a experimentacao animal. 3nd Edition. Belo Horizonte: Editora FEPMVZ, Fundacao de Ensino e Pesquisa em Medicina Veterinaria e Zootecnia, 2010. 264p.
}
\author{
Emmanuel Arnhold <emmanuelarnhold@yahoo.com.br>
}

\seealso{
lm, lme(package nlme), ea1(package easyanova)
}

\examples{

# analysis in completely randomized design
data(data1)
r1=pr(data1)
names(r1)
r1
r1[1]

# analysis in randomized block design
data(data2)
r2=pr(data2)
r2

# analysis in latin square design
data(data3)
r3=pr(data3)
r3

# analysis in several latin squares
data(data4)
r4=pr(data4)
r4


a1=r4[[2]][[1]][1]
a2=r4[[2]][[2]][1]

b1=r4[[2]][[1]][2]
b2=r4[[2]][[2]][2]

c=r4[[2]][[2]][3]

f1=function(x){a1+b1*x}
f2=function(x){a2+b2*x+c*x^2}

dat=aggregate(.~treatment, data4, FUN=mean)

#plot(milk_fat~treatment, data=dat, bty="n", xlim=c(3,6), ylim=c(3,5))
#plot(f1, 3.2,5.2, col=3, add=TRUE)
#plot(f2, 3.2,5.2, col=2, lty=2, add=TRUE)
  
}

