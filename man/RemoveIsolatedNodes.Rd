\name{RemoveIsolatedNodes}
\alias{RemoveIsolatedNodes}
\title{Remove isolated nodes}
\description{Remove isolated nodes.}
\usage{RemoveIsolatedNodes(community, title)}

\arguments{
  \item{community}{an object of class \code{Community}.}
  \item{title}{a title for the new community.}
}

\details{Returns a new \code{Community} with isolated nodes removed.
}

\value{A new object of class \code{Community}.}
\author{Lawrence Hudson}

\seealso{\code{\link{Community}}, 
         \code{\link{IsIsolatedNode}}}

\examples{
data(TL84)
IsolatedNodes(TL84)

TL84.no.isolated <- RemoveIsolatedNodes(TL84)
IsolatedNodes(TL84.no.isolated)
}

\keyword{utilities}
