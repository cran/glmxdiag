varCheck.glm <- function(model, xlab, ylab, pch, col, lcol, ...) {
  
  if(missing(xlab)) xlab <- expression((y-hat(mu))^2/hat(phi))
  if(missing(ylab)) ylab <- expression(V(hat(mu)))
  if(missing(pch)) pch <- 16
  if(missing(lcol)) lcol <- 1
  
  
  y <- model$y
  mu <- fitted(model)
  sm <- summary(model)
  phi <- sm$dispersion
  vmu <- model$family$variance
  x.points <- (y-mu)^2/phi
  y.points <- vmu(mu)
  
  if(missing(col)) col <- ifelse(x.points > y.points, 2, 4)
  plot(x.points, y.points, pch = 16, xlab = xlab, ylab = ylab, col = col, ...)
  abline(a = 0, b = 1, col = lcol)
}

