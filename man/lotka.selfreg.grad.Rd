\name{lotka.selfreg.grad}
\alias{lotka.selfreg.grad}
\title{Gradient function for 2-species predator prey model}
\usage{
  lotka.selfreg.grad(X, t, p)
}
\arguments{
  \item{X}{is vector containing state variables of model.
  In this case it is a two-element vector: \describe{
  \item{Food}{size of prey population} \item{Pred}{size of
  predator population} }}

  \item{t}{time at which gradient is computed}

  \item{vector}{containing parameters of model \describe{
  \item{a}{intrinsic growth rate of Food} \item{b}{per
  capita consumption of Food by Pred} \item{c}{per capita
  increase in Pred due to eating Food} \item{d}{intrinsic
  death rate of Pred} }}
}
\value{
  gradient vector with rate of change with respect to time
  of both state variables
}
\description{
  Simple predator prey system wherein prey grow
  logistically in the absence of predators and predators
  decline exponentially in the absence of prey
}
\references{
  Gotelli, N. A primer of ecology, Second edition. Sinauer
  Associates.
}

