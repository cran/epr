\name{epr-package}
\alias{epr-package}
\alias{epr}
\docType{package}
\title{
Easy Polynomial Regression
}
\description{
Performs analysis of polynomial regression in simple designs with quantitative treatments.
}

\details{
\tabular{ll}{
Package: \tab epr\cr
Type: \tab Package\cr
Version: \tab 3.0\cr
Date: \tab 2017-11-14\cr
License: \tab GPL-2\cr
}
}

\author{
Emmanuel Arnhold <emmanuelarnhold@yahoo.com.br>
}
\references{

KAPS, M. and LAMBERSON, W. R. Biostatistics for Animal Science: an introductory text. 2nd Edition. CABI Publishing, Wallingford, Oxfordshire, UK, 2009. 504p.

SAMPAIO, I. B. M. Estatistica aplicada a experimentacao animal. 3nd Edition. Belo Horizonte: Editora FEPMVZ, Fundacao de Ensino e Pesquisa em Medicina Veterinaria e Zootecnia, 2010. 264p.
}

\examples{
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


# analysis in latin square design
data(data3)
r3=pr2(data3, design=3)
r3

# analysis in several latin squares
data(data4)
r4=pr2(data4, design=4)
r4



}
