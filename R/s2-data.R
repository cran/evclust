#' S2 dataset
#'
#' This dataset contains 5000 two-dimensional vectors grouped in 15 Gaussian clusters.
#'
#' @docType data
#'
#' @usage data(s2)
#'
#' @format A matrix with 5000 rows and two columns.
#'
#' @keywords datasets
#'
#' @references
#' P. Franti and O. Virmajoki. Iterative shrinking method for clustering problems.
#' Pattern Recognition, 39(5):761--775, 2006.
#'
#' T. Denoeux, O. Kanjanatarakul and S. Sriboonchitta.
#'  EK-NNclus: a clustering procedure based on the evidential K-nearest neighbor rule.
#'  Knowledge-Based Systems, Vol. 88, pages 57--69, 2015.
#'
#' T. Denoeux, S. Sriboonchitta and O. Kanjanatarakul. Evidential clustering of large
#'dissimilarity data. Knowledge-Based Systems (accepted for publication),
#'DOI: 10.1016/j.knosys.2016.05.043, 2016.
#'
#' @examples
#' data(s2)
#' plot(s2[,1],s2[,2],xlab=expression(x[1]),ylab=expression(x[2]))
"s2"
