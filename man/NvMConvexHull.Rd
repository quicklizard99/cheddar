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

\details{Returns the points and area of the minimum convex hull (a polygon in 
log10-transformed numerical abundance versus log10-transformed body mass space) 
that bounds all the species within the community.
}

\references{Leaper, R. and Raffaelli, D. (1999) Defining the abundance
body-size constraint space: data from a real food web. Ecology Letters 
\bold{2}, 3, 191--199.
}

\value{A \code{list} containing the values
  \item{nodes}{The names of the nodes that make up the convex hull.}
  \item{points}{A matrix containing columns `x' and `y' that contain the 
  coordinates of the points that make up the convex hull.}
  \item{area}{The area within the convex hull.}
}

\author{Lawrence Hudson}

\seealso{\code{\link{Community}}, \code{\link{PlotNvM}}, \code{\link{chull}}}

\examples{
data(TL84)

# Compute convex hull
convex.hull <- NvMConvexHull(TL84)

# The nodes that form the hull
convex.hull$nodes

# The area of the hull
convex.hull$area

# Plot the hull in red around the nodes
PlotNvM(TL84)
polygon(convex.hull$points, lwd=2, border='red')
}

\keyword{utilities}
