influenceDiag.glm <- function(model, approx = TRUE) {
  fam <- model$family$family
  inf <- influence(model)
  db <- inf$coefficients
  cookd <- cooks.distance(model, infl = inf)
  out <- list(DFbeta = db, cookDist = cookd, leverage = inf$hat, full.beta = coef(model),
              family = fam)
  attr(out, which = 'class') <- 'influence'
  out
  
}



