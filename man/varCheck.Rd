\name{varCheck}

\alias{varCheck}

\title{Checking adequacy of variance function}

\description{According to GLM theory, the true variance function can be estimated as (y-mu)^2/phi. Comparing this quantity with the variance function assumed by the model suggests if the chosen one is appropriate. If the model is correct, points should be arranged near the bisector; otherwise this plot suggests the direction of the error made by chosing the variance function.}

\usage{varCheck(model, xlab, ylab, pch, col, lcol, ...)}

\arguments{
  \item{model}{a model supported by \code{glmxdiag}.}
  \item{xlab}{title for the x axis.}
  \item{ylab}{title for the y axis.}
  \item{pch}{type of points.}
  \item{col}{color of the points.}
  \item{lcol}{color of the bisector.}
  \item{...}{further arguments passed to \code{plot}.}
  
}

\value{
Doesn't return a value, called for side effects.
}

\author{
Giuseppe Reale
}


\examples{

data(stopping, package = 'glmxdiag')
mod <- glm(Distance ~ Speed, family = Gamma(link = 'sqrt'), data = stopping)
varCheck(mod)


}
