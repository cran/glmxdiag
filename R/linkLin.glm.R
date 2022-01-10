linkLin.glm <- function(model, smooth = TRUE, xlab, ylab, main, pch, lcol, lwd, ...) {
  eta <- model$linear.predictor
  wres <- residuals(model, type = "working")
  zeta <- eta + wres
  link.name <- model$family$link
  linkLin_plot(zeta, eta, smooth = smooth, xlab, ylab, main, pch, lcol, lwd, link.name, ...)
  
}
