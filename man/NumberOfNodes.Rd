\name{NumberOfNodes}
\alias{NumberOfNodes}
\title{Number of nodes}
\description{The number of nodes in the community.}
\usage{NumberOfNodes(community)}
\arguments{
  \item{community}{an object of class \code{Community}.}
}

\value{An integer.}

\author{Lawrence Hudson}

\seealso{\code{\link{Community}}, \code{\link{NPS}}}

\examples{
data(TL84)
NumberOfNodes(TL84)
}

\keyword{utilities}
