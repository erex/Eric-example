\name{lotka.selfreg.plot}
\alias{lotka.selfreg.plot}
\title{Plotting routine for 2-species predator prey model}
\usage{
  lotka.selfreg.plot(S0, p, t0, tmax, delta.t = 0.1)
}
\arguments{
  \item{S0}{is vector containing state variables of model.
  In this case it is a two-element vector: \describe{
  \item{Food}{size of prey population} \item{Pred}{size of
  predator population} }}

  \item{p}{vector of parameters}

  \item{t0}{initial time}

  \item{tmax}{end time for plot}

  \item{delta.t}{time increment}
}
\description{
  Given a call to \link{lotka.selfreg.grad} this function
  will produce graphics
}

