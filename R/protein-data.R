#' Protein dataset
#'
#' This real data set consists of a dissimilarity matrix derived from the structural
#' comparison of 213 protein sequences. Each of these proteins is known to belong to one of four
#' classes of globins: hemoglobin-alpha (HA), hemoglobin-beta (HB), myoglobin (M) and
#' heterogeneous globins (G).
#'
#' @docType data
#'
#' @usage data(protein)
#'
#' @format A list with three elements:
#' \describe{
#' \item{D}{The 213x213 dissimilarity matrix.}
#' \item{class}{A 213-vector containing the class encoded a a factor with four levels:
#' "G", "HA", "HB", "M".}
#' \item{y}{A 213-vector containing the class encoded by an integer between 1 and 4.}
#' }
#'
#' @keywords datasets
#'
#' @references
#' T. Hofmann,and J. Buhmann. Pairwise data clustering by deterministic annealing. IEEE
#' Transactions on Pattern Analysis and Machine Intelligence, 19(1):1--14, 1997.
#'
#' T. Graepel, R. Herbrich, P. Bollmann-Sdorra, and K. Obermayer. Classification on pairwise
#' proximity data. in Advances in Neural Information Processing Systems 11, M. Kearns,
#' S. Solla, and D. Kohn, eds., MIT Press, Cambridge, MA, 438--444, 1999.
#'
#' T. Denoeux and M.-H. Masson. EVCLUS: Evidential Clustering of Proximity Data.
#'IEEE Transactions on Systems, Man and Cybernetics B, Vol. 34, Issue 1, 95--109, 2004.
#'
#'
#' @examples
#' data(protein)
#' z<- cmdscale(protein$D,k=2)  # Multidimensional scaling
#' plot(z[,1],z[,2],xlab=expression(z[1]),ylab=expression(z[2]),pch=protein$y,col=protein$y)
"protein"
