#' Plotting a credal partition
#'
#' Generates plots of a credal partition.
#'
#' This function plots the hard and rough partions (lower and upper approximations) extracted
#' from a credal partition, together with two dimensional attribute data.
#'
#'
#' @param x An object of class \code{"credpart"}, encoding a credal partition.
#' @param X A data matrix. If it has more than two columns (attributes), only the first two
#' columns are used.
#' @param ... Other arguments to be passed to the plot function.
#' @param mfrow A 2-vector defining the number of rows and columns of the plot. If mfrow=c(1,1),
#' only one figure is drawn. Otherwise, mfrow[1] x mfrow[2] should not be less than x, the
#' number of clusters.
#' @param ytrue The vector of true class labels. If not supplied, the hard partition corresponding
#' to the maximum plausibility is used instead.
#' @param Outliers If TRUE, the outliers are plotted, and they are not included in the lower
#' and upper approximations of the clusters.
#' @param Approx If Approx==1 (default), the lower and upper cluster approximations are
#' computed using the interval dominance rule. Otherwise, the maximum mass rule is used.
#' @param cex Size of data points.
#' @param cex_outliers Size of data points for outliers.
#' @param lwd Line width for drawing the lower and upper approximations.
#' @param ask	Logical; if TRUE, the user is asked before each plot.
#'
#' @return The maximum plausibility hard partition, as well as the lower and upper approximations
#' of each cluster are drawn in the two-dimensional space specified by matrix \code{X}. If
#' prototypes are defined (for methods \code{"ecm"} and \code{"cecm"}), they are also
#' represented on the plot.  For method \code{"kevclus"},  a second plot with Shepard's diagram (degrees
#' of conflict vs. transformed dissimilarities) is drawn. If input \code{X} is not supplied,
#' and \code{method=="kevclus"}, then only the Shepard diagram is drawn.
#'
#' @export
#' @importFrom graphics abline par plot points polygon
#' @importFrom grDevices chull
#'
#' @seealso \code{\link{extractMass}}, \code{\link{summary.credpart}}, \code{\link{ecm}},
#' \code{\link{recm}}, \code{\link{cecm}}, \code{\link{kevclus}}.
#'
#' @references
#' T. Denoeux and O. Kanjanatarakul. Beyond Fuzzy, Possibilistic and Rough: An
#' Investigation of Belief Functions in Clustering. 8th International conference on soft
#' methods in probability and statistics, Rome, 12-14 September, 2016.
#'
#' M.-H. Masson and T. Denoeux. ECM: An evidential version of the fuzzy c-means algorithm.
#' Pattern Recognition, Vol. 41, Issue 4, pages 1384--1397, 2008.
#'
#'T. Denoeux, S. Sriboonchitta and O. Kanjanatarakul. Evidential clustering of large
#'dissimilarity data. Knowledge-Based Systems, vol. 106, pages 179-195, 2016.
#'
#' Available from \url{https://www.hds.utc.fr/~tdenoeux}.
#' @examples
#' ## Example with Four-class data
#' data("fourclass")
#' x<-fourclass[,1:2]
#' y<-fourclass[,3]
#' c=4
#' ## Running k-EVCLUS with singletons
#' clus<-kevclus(x=x,k=100,c=c,type='simple')
#' ## Plot the results
#' plot(clus,X=x,mfrow=c(2,2),ytrue=y)
plot.credpart <- function(x,X=NULL,...,mfrow=c(1,1),ytrue=NULL,Outliers=TRUE,Approx=1,cex=0.7,
                          cex_outliers=1.3,lwd=2,ask=FALSE){
  clus<-x
  if(!is.null(X)){
    x<-X
    y<-ytrue
    par(ask=ask)

    if((mfrow[1]==1) & (mfrow[2]==1)) oneWindow<-TRUE else oneWindow<-FALSE

    if(is.null(y)) y<-clus$y.pl
    outlier<-which(rowSums(clus$Y)==0)
    c<-max(clus$y.pl)

    if(Approx==1){
      lower.approx<-clus$lower.approx.nd
      upper.approx<-clus$upper.approx.nd
    }else{
      lower.approx<-clus$lower.approx
      upper.approx<-clus$upper.approx
    }

    if(Outliers==TRUE){
      for(i in 1:c){
        lower.approx[[i]]<-setdiff(lower.approx[[i]],outlier)
        upper.approx[[i]]<-setdiff(upper.approx[[i]],outlier)
      }
    }

    if(oneWindow){
      plot(x[,1],x[,2],pch=clus$y.pl+1,col=y,cex=cex,xlab=expression(x[1]),
           ylab=expression(x[2]))
      if(Outliers==TRUE) points(x[outlier,1],x[outlier,2],pch=1,cex=cex_outliers)
      if(!is.null(clus$g)) points(clus$g[,1],clus$g[,2],pch=3,cex=cex_outliers)
    } else par(mfrow=mfrow)

    for(i in (1:c)){
      if(!oneWindow){
        plot(x[,1],x[,2],pch=clus$y.pl+1,col=y,cex=cex,xlab=expression(x[1]),
             ylab=expression(x[2]))
        if(Outliers==TRUE) points(x[outlier,1],x[outlier,2],pch=1,cex=cex_outliers)
        if(!is.null(clus$g)) points(clus$g[,1],clus$g[,2],pch=3,cex=cex_outliers)
      }
      xx<-x[lower.approx[[i]],]
      if(oneWindow) icol<-i else icol<-1
      if(nrow(xx)>=3) polygon(xx[chull(xx),],lwd=lwd,border=icol)
      xx<-x[upper.approx[[i]],]
      if(nrow(xx)>=3) polygon(xx[chull(xx),],lty = 2,lwd=lwd,border=icol)
    }
    par(mfrow=c(1,1))
  }

  # Shepard diagram
  if(clus$method=="kevclus"){
    n<-nrow(clus$D)
    if(n>300) symb<-'.' else symb<-1
    plot(as.vector(clus$D),as.vector(clus$Kmat),pty="s",main="Shepard diagram",
         xlab=expression(delta[ij]),ylab=expression(kappa[ij]),pch=symb)
    abline(0,1)
  }
}
