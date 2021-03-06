% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ecm.R
\name{ecm}
\alias{ecm}
\title{Evidential c-means algorithm}
\usage{
ecm(
  x,
  c,
  g0 = NULL,
  type = "full",
  pairs = NULL,
  Omega = TRUE,
  ntrials = 1,
  alpha = 1,
  beta = 2,
  delta = 10,
  epsi = 0.001,
  init = "kmeans",
  disp = TRUE
)
}
\arguments{
\item{x}{input matrix of size n x d, where n is the number of objects and d the number of
attributes.}

\item{c}{Number of  clusters.}

\item{g0}{Initial prototypes, matrix of size c x d. If not supplied, the prototypes are
initialized randomly.}

\item{type}{Type of focal sets ("simple": empty set, singletons and Omega;
"full": all \eqn{2^c} subsets of \eqn{\Omega}; "pairs": \eqn{\emptyset}, singletons,
\eqn{\Omega}, and all
or selected pairs).}

\item{pairs}{Set of pairs to be included in the focal sets; if NULL, all pairs are
included. Used only if type="pairs".}

\item{Omega}{Logical. If TRUE (default), the whole frame is included (for types 'simple' and
'pairs').}

\item{ntrials}{Number of runs of the optimization algorithm (set to 1 if m0 is  supplied).}

\item{alpha}{Exponent of the cardinality in the cost function.}

\item{beta}{Exponent of masses in the cost function.}

\item{delta}{Distance to the empty set.}

\item{epsi}{Minimum amount of improvement.}

\item{init}{Initialization: "kmeans" (default) or "rand" (random).}

\item{disp}{If TRUE (default), intermediate results are displayed.}
}
\value{
The credal partition (an object of class \code{"credpart"}).
}
\description{
\code{ecm} computes a credal partition from a matrix of attribute data using the
Evidential c-means (ECM) algorithm.
}
\details{
ECM is an evidential version algorithm of the Hard c-Means (HCM) and Fuzzy c-Means (FCM)
algorithms. As in HCM and FCM, each cluster is represented by a prototype. However, in ECM,
some sets of clusters are also represented by a prototype, which is defined as the center of mass
of the prototypes in each individual cluster. The algorithm iteratively optimizes a cost
function, with respect to the prototypes and to the credal partition. By default, each mass
function in the credal partition has \eqn{2^c} focal sets, where c is the supplied number of
clusters. We can also limit the number of focal sets to
subsets of clusters with cardinalities 0, 1 and c (recommended if c>=10), or to all or some
selected pairs of clusters.
If  initial prototypes g0 are provided, the number of trials is automatically set to 1.
}
\examples{
## Clustering of the Four-class dataset
\dontrun{
data(fourclass)
x<-fourclass[,1:2]
y<-fourclass[,3]
clus<-ecm(x,c=4,type='full',alpha=1,beta=2,delta=sqrt(20),epsi=1e-3,disp=TRUE)
plot(clus,X=x,mfrow=c(2,2),ytrue=y,Outliers=TRUE,Approx=2)
}
}
\references{
M.-H. Masson and T. Denoeux. ECM: An evidential version of the fuzzy
c-means algorithm. Pattern Recognition, Vol. 41, Issue 4, pages 1384--1397, 2008.
}
\seealso{
\code{\link{makeF}}, \code{\link{extractMass}}, \code{\link{recm}}, \code{\link{cecm}},
\code{\link{plot.credpart}}
}
\author{
Thierry Denoeux (from a MATLAB code written by Marie-Helene Masson).
}
