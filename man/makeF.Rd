% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/makeF.R
\name{makeF}
\alias{makeF}
\title{Creation of a matrix of focal sets}
\usage{
makeF(c, type = c("simple", "full", "pairs"), pairs = NULL, Omega = TRUE)
}
\arguments{
\item{c}{Number of  clusters.}

\item{type}{Type of focal sets ("simple": \eqn{\emptyset}, singletons and \eqn{\Omega};
"full": all \eqn{2^c} subsets of \eqn{\Omega}; "pairs": \eqn{\emptyset}, singletons,
\eqn{\Omega}, and all or selected pairs).}

\item{pairs}{Set of pairs to be included in the focal sets; if NULL, all pairs
are included. Used only if type="pairs".}

\item{Omega}{Logical. If TRUE (default), \eqn{\Omega} is a focal set (for types 'simple' and
'pairs').}
}
\value{
A matrix (f,c) of focal sets.
}
\description{
\code{makeF} creates a matrix of focal sets
}
\examples{
c<-4
## Generation of all 16 focal sets
F<-makeF(c,type='full')
## Generation of focal sets of cardinality 0, 1 and c
F<-makeF(c,type='simple')
## Generation of focal sets of cardinality 0, 1, and 2
F<-makeF(c,type='pairs',Omega=FALSE)
## Generation of focal sets of cardinality 0, 1, and c, plus the pairs (1,2) and (1,3)
F<-makeF(c,type='pairs',pairs=matrix(c(1,2,1,3),nrow=2,byrow=TRUE))

}
