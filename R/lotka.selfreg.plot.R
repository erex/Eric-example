#' Plotting routine for 2-species predator prey model
#' 
#' Given a call to \link{lotka.selfreg.grad} this function will produce graphics
#' 
#' @param S0 is vector containing state variables of model.  In this
#' case it is a two-element vector:
#' \describe{
#'  \item{Food}{size of prey population}
#'  \item{Pred}{size of predator population}
#' }
#' @param p vector of parameters
#' @param t0 initial time
#' @param tmax end time for plot
#' @param delta.t time increment

lotka.selfreg.plot <-
function(S0,p,t0,tmax,delta.t=0.1) 
{ 
  library(desolve)
  Food <- S0[1]; Pred <- S0[2]
  a <- p[1]; b <- p[2]; c <- p[3]; d <- p[4]; K <- p[5]
  m <- ode(lotka.selfreg.grad,S0=c(Food,Pred),p=p,0,tmax=tmax,dt=delta.t)
  plot(m$t,m$S[,1],ylim=range(m$S),type='l',xlab="Time",ylab="Food and Pred")
  lines(m$t,m$S[,2],lty=2,col=2)
  legend(tmax/2,max(m$S),legend=c("Food","Pred"),lty=1:2,col=1:2,cex=0.7)
  F.star <- d/c
  P.star <- a/b*(1-d/(c*K))
  abline(h=F.star,col=1)
  abline(h=P.star,col=2)
  plot(m$S[,1],m$S[,2],xlab="Food",ylab="Pred",type='l')
  abline(v=F.star,col=1)
  abline(h=P.star,col=2)
  return(list=c(F.star=F.star,P.star=P.star)) 
}
