% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/credal_RI.R
\name{credal_RI}
\alias{credal_RI}
\title{Credal Rand indices}
\usage{
credal_RI(P1, P2, type = "c")
}
\arguments{
\item{P1}{Relational representation of the first credal partition such as generated
by function \code{pairwise_mass}}

\item{P2}{Relational representation of the second credal partition such as generated
by function \code{pairwise_mass}}

\item{type}{"c" for degree of conflict (default), "j" for Jousselme's distance and "b" for 
belief distance.}
}
\value{
The credal Rand index
}
\description{
\code{credal_RI} computes generalizations of the Rand index to compare credal partitions, as defined 
in Denoeux et al (2018).
}
\details{
In Denoeux et al. (2018), two generalizations of the Rand index for comparing credal partitions
are defined: one is based on distances between mass function, the other one is based on distances.
In the latter case, two distances are proposed: Jousselme's distance and the L1 distance between 
belief functions. These three indices can be computed by function \code{credal_RI}.
}
\examples{
## Butterfly data
data(butterfly)
clus1<-kevclus(butterfly,c=2) 
P1<-pairwise_mass(clus1)
clus2<-ecm(butterfly,c=2)
P2<-pairwise_mass(clus2)
RI1<-credal_RI(P1,P2,"c")
RI2<-credal_RI(P1,P2,"j")
RI3<-credal_RI(P1,P2,"b")
print(c(RI1,RI2,RI3))

}
\references{
T. Denoeux, S. Li and S. Sriboonchitta. Evaluating and Comparing Soft Partitions: an 
 Approach Based on Dempster-Shafer Theory. IEEE Transactions on Fuzzy Systems, 
 26(3):1231-1244, 2018.
}
\seealso{
\code{\link{nonspecificity}}, \code{\link{pairwise_mass}}
}
