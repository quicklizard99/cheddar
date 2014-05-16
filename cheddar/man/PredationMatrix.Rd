\name{PredationMatrix}
\alias{PredationMatrix}
\title{Predation matrix}
\description{Returns a predation matrix.}

\usage{
PredationMatrix(community, weight=NULL)
}

\arguments{
  \item{community}{an object of class \code{Community}.}
  \item{weight}{either the name of a first-class link property or the name of 
    a function that meets the specification of the \code{link.properties} 
    parameter of \code{TLPS}.}
}

\details{Returns a square matrix with \code{NumberOfNodes} rows and columns. 
If \code{weight} is \code{NULL} then a binary matrix, in which elements are 
either 0 or 1, is returned; 1 indicates a trophic link from a resource 
(row) to a consumer (column). If \code{weight} is not \code{NULL} then elements 
of the returned matrix will be set to the values given by \code{weight}. 
Row names and column names of the returned \code{matrix} are node names. 
}

\value{A square \code{matrix}.}

\author{Lawrence Hudson}

\seealso{\code{\link{PlotPredationMatrix}}, 
         \code{\link{TLPS}}, 
         \code{\link{NumberOfNodes}}, 
         \code{\link{NumberOfTrophicLinks}}, 
         \code{\link{ResourcesByNode}}, 
         \code{\link{ConsumersByNode}},
         \code{\link{PredationMatrixToLinks}}}

\examples{
data(TL84)

# A square matrix of NumberOfNodes rows and columns
dim(PredationMatrix(TL84))
NumberOfNodes(TL84)

# Should contain NumberOfTrophicLinks links
sum(PredationMatrix(TL84))
NumberOfTrophicLinks(TL84)

# Compare an unweighted matrix and a matrix weighted by diet fraction
data(Benguela)

PredationMatrix(Benguela)
PredationMatrix(Benguela, weight='diet.fraction')
}
\keyword{utilities}

