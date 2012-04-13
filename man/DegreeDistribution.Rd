\name{DegreeDistribution}
\alias{DegreeDistribution}
\title{Node degree distribution}
\description{Node degree distribution.}

\usage{DegreeDistribution(community, cumulative=FALSE)}

\arguments{
  \item{community}{an object of class \code{Community}.}
  \item{cumulative}{logical - if \code{TRUE} the cumulative degree 
    distribution is returned.}
}

\details{Returns a vector of proportions of nodes with 
\code{0:max(Degree(community))} trophic links. 
}

\value{A vector of numbers.}

\author{Lawrence Hudson}

\seealso{\code{\link{Degree}}, \code{\link{PlotDegreeDistribution}}}

\examples{
data(TL84)
DegreeDistribution(TL84)
DegreeDistribution(TL84, cumulative=TRUE)
}
\keyword{utilities}

