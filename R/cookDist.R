cookDist <- function(object, label.id, n.label.id, xlab, ylab, pos, ...) {
  
  if(!inherits(object, 'influence')) stop('cookDist can only be used with object of class', dQuote('influence'), ', see ?influenceDiag')
  
  cookd <- object$cookDist
  n.obs <- length(cookd)
  index <- seq_len(n.obs)
  
  if(missing(label.id)) label.id <- index
  if(missing(n.label.id)) n.label.id <- 2
  if(missing(xlab)) xlab <- 'Index' 
  if(missing(ylab)) ylab <- ifelse(object$family == 'betabinomial', 
                                   expression((beta - beta(-i)) ~ V(beta)^{-1} ~ (beta - beta(-i))/p),
                                   "Cook's distance")
  if(missing(pos)) pos <- 4
  
  points.lab <- getMaxIndex(cookd, label.id, k = n.label.id)
  
  backup.par <- par(no.readonly = T)
  on.exit(par(backup.par))
  
  par(mgp=c(2,1,0))  
  plot(cookd, xlab = xlab, ylab = ylab, type = "h", ...)
  text(x = index, y = cookd, label = points.lab, pos = pos)
}

