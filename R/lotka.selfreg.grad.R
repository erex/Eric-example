#'  Gradient function for 2-species predator prey model
#' 
#' Simple predator prey system wherein
#' prey grow logistically in the absence of predators and predators 
#' decline exponentially in the absence of prey
#' 
#' @param X is vector containing state variables of model.  In this
#' case it is a two-element vector:
#' \describe{
#'  \item{Food}{size of prey population}
#'  \item{Pred}{size of predator population}
#' }
#' @param t time at which gradient is computed
#' @param vector containing parameters of model
#' \describe{
#'  \item{a}{intrinsic growth rate of Food}
#'  \item{b}{per capita consumption of Food by Pred}
#'  \item{c}{per capita increase in Pred due to eating Food}
#'  \item{d}{intrinsic death rate of Pred}
#' }
#' 
#' @return 
#' gradient vector with rate of change with respect to time of both state variables
#' 
#' 
#' @references
#' Gotelli, N. A primer of ecology, Second edition. Sinauer Associates.

lotka.selfreg.grad <-
function(X,t,p) { 
  Food <- X[1]; Pred <- X[2]
  a <- p[1]; b <- p[2]; c <- p[3]; d <- p[4]; K <- p[5]
  g <- rep(0,2)
  g[1] <- (a*(1-Food/K)-b*Pred)*Food
  g[2] <- (c*Food - d)*Pred
  x <- g[1] + g[2]
  return(g)
}
