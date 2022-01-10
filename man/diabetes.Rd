\name{diabetes}

\alias{diabetes}
\docType{data}
\title{Diabetes}

\description{
This data comes from a study (Sockett et al, 1987) of the factors affecting patterns of insulin-dependent mellitus in children. The objective is to investigate the dependence of the level of serum C-peptide on the various other factors in order to understand the patterns of residual insulin secretion. The response measurement is the logarithm of C-peptide concentration (pmol/ml) at the diagnosis, and the predictor measurements age and base deficit, a measure of acidity.
}

\usage{data("diabetes")}
\format{
  A data frame with 43 observations on the following 3 variables.
  \describe{
    \item{\code{Age}}{age of the child.}
    \item{\code{Def}}{base deficit, a measure of acidity.}
    \item{\code{C_pep}}{logarithm of C-peptide concentration (pmol/ml).}
  }
}

\source{Generalized Additive Models (p.304) by Hastie & Tibshirani, Chapman & Hall.}