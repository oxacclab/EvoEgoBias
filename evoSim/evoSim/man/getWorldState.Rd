% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/evoSim.R
\name{getWorldState}
\alias{getWorldState}
\title{Return a world state for the given \code{generation} and \code{decision}
numbers}
\usage{
getWorldState(modelParams, world)
}
\arguments{
\item{modelParams}{parameters for the model as a named list}

\item{world}{list of world properties (\code{state} obtained from
\code{\link{getWorldState}}, and \code{generation}, and \code{decision}
numbers.)}
}
\value{
a world state to be passed to other decision functions
  \code{\link{getDecision}}, \code{\link{getAdvisor}},
  \code{\link{updateConnections}} as \code{world$state}
}
\description{
By default this function returns \code{runif(1)}, and agents'
  task is to reproduce the value
}
