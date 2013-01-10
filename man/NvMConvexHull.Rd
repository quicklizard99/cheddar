\name{NvMConvexHull}
\alias{NvMConvexHull}
\title{NvMConvexHull}
\description{Compute the convex hull around log-transformed $N$ versus $M$ 
data.}

\usage{
NvMConvexHull(community)
}

\arguments{
  \item{community}{an object of class \code{Community}.}
}

\details{
}

\references{
}

\value{A \code{list} containing the values
  \item{points}{The matrix of the points that make up the convex hull.}
  \item{area}{The area within the convex hull.}
}

\author{Lawrence Hudson}

\seealso{\code{\link{Community}}, \code{\link{PlotNvM}}}

\examples{
data(TL84)
PlotNvM(TL84)
convex.hull <- NvMConvexHull(TL84)
polygon(convex.hull$points, lwd=2, border='red')
convex.hull$area
}

\keyword{utilities}
