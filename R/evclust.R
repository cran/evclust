#' evclust: A package for evidential clustering
#'
#' Various clustering algorithms that generate a credal partition, i.e., a set of mass
#' functions. Mass functions quantify the cluster-membership uncertainty of the objects.
#' The package consists in five main functions, implementing five different evidential
#' clustering algorithms:
#' \describe{
#' \item{ecm}{Evidential c-means algorithm (Masson and Denoeux, 2008)}
#' \item{recm}{Relational Evidential c-means algorithm (Masson and Denoeux, 2009)}
#' \item{kevclus}{$k$-EVCLUS algorithm (Denoeux and Masson, 2004; Denoeux et al., 2016) }
#' \item{EkNNclus}{E$k$-NNclus algorithm (Denoeux et al., 2015)}
#' \item{cecm}{Constrained Evidential c-means algorithm (Antoine et al, 2012)}
#'}
#'
#' @docType package
#' @name evclust
#'
#' @seealso \code{\link{ecm}}, \code{\link{recm}},
#'\code{\link{cecm}}, \code{\link{kevclus}}, \code{\link{EkNNclus}}.
#'
#' @references
#'
#' V. Antoine, B. Quost, M.-H. Masson and T. Denoeux. CECM: Constrained
#'Evidential C-Means algorithm. Computational Statistics and Data Analysis, Vol. 56,
#'Issue 4, pages 894--914, 2012.
#'
#'T. Denoeux and M.-H. Masson. EVCLUS: Evidential Clustering of Proximity Data.
#'IEEE Transactions on Systems, Man and Cybernetics B, Vol. 34, Issue 1, 95--109, 2004.
#'
#'T. Denoeux, O. Kanjanatarakul and S. Sriboonchitta.
#'EK-NNclus: a clustering procedure based on the evidential K-nearest neighbor rule.
#'Knowledge-Based Systems, Vol. 88, pages 57--69, 2015.
#'
#'T. Denoeux, S. Sriboonchitta and O. Kanjanatarakul. Evidential clustering of large
#'dissimilarity data. Knowledge-Based Systems (accepted for publication),
#'DOI:10.1016/j.knosys.2016.05.043, 2016.
#'
#' M.-H. Masson and T. Denoeux. ECM: An evidential version of the fuzzy c-means algorithm.
#' Pattern Recognition, Vol. 41, Issue 4, pages 1384--1397, 2008.
#'
#' M.-H. Masson and T. Denoeux. RECM: Relational Evidential c-means algorithm.
#'Pattern Recognition Letters, Vol. 30, pages 1015--1026, 2009.
#'
NULL
