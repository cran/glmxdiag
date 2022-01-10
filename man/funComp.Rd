\name{funComp}
\alias{funComp}

\title{
Compatibility of functions
}
\description{
Not all functions inside \code{glmxdiag} are available for quasi-GLM or GLM extensions . The output table helps the user orientate inside the package in order to check which function can be used according to the model under analysis. 
}
\usage{
funComp()
}

\value{
Returns a dataframe. 'X' inside a cell means that the row-function can be used with the specific column-model, otherwise it cannot be used.
}

\author{
Giuseppe Reale
}

\examples{
funComp()
}
