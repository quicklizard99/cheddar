\name{IsCannibal}
\alias{IsCannibal}
\alias{Cannibals}
\alias{FractionCannibalistic}
\title{Cannibalistic nodes}
\description{Nodes that consume themselves in the food web.}

\usage{
IsCannibal(community)
Cannibals(community)
FractionCannibalistic(community)
}

\arguments{
  \item{community}{an object of class \code{Community}.}
}

\details{\code{IsCannibal} returns a vector of \code{logical} of length 
\code{NumberOfNodes}; values are \code{TRUE} for nodes consume themselves. 
\code{Cannibals} returns the names of nodes for which \code{IsCannibals} 
returns \code{TRUE}. \code{FractionCannibalistic} returns the proportion of 
nodes for which \code{IsCannibal} returns \code{TRUE}
}

\value{Either a \code{logical} vector of length \code{NumberOfNodes} or a 
vector of names.
}

\author{Lawrence Hudson}

\seealso{\code{\link{RemoveCannibalisticLinks}}, 
         \code{\link{NumberOfNodes}}, 
         \code{\link{PredationMatrix}}, 
         \code{\link{Degree}}, 
         \code{\link{InDegree}}, 
         \code{\link{OutDegree}}, 
         \code{\link{ResourcesByNode}}, 
         \code{\link{ConsumersByNode}}, 
         \code{\link{ResourcesOfNodes}}, 
         \code{\link{ConsumersOfNodes}}}

\examples{
data(TL84)

IsCannibal(TL84)
Cannibals(TL84)
FractionCannibalistic(TL84)
}

\keyword{utilities}
