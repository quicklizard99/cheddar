\name{ShortestPaths}
\alias{ShortestPaths}
\alias{CharacteristicPathLength}
\title{Path lengths}
\description{Functions that compute the shortest trophic paths between nodes.}
\usage{ShortestPaths(community, weight.by=NULL)
CharacteristicPathLength(community)}

\arguments{
  \item{community}{an object of class \code{Community}.}
  \item{weight.by}{the name of a property by which to weight paths.}
}

\details{ShortestPaths uses Dijkstra's algorithm to compute the number 
of trophic links between each pair of nodes in the food web. 
\code{CharacteristicPathLength} returns the mean of path lengths. }

\value{A square \code{matrix} with \code{NumberOfNodes} rows and columns or 
a single number.}

\author{Lawrence Hudson}

\seealso{\code{\link{PredationMatrix}}, \code{\link{NPS}}}

\references{Williams, R.J. and Berlow, E.L. and Dunne, J.A. and Barab{\'a}si, 
A.L. and Martinez, N.D. (2002) Two degrees of separation in complex food webs. 
Proceedings of the National Academy of Sciences of the United States of America 
\bold{99}, 20, 12913--12916}

\examples{
data(Benguela)

# Compare weighted and unweighted
ShortestPaths(Benguela)
ShortestPaths(Benguela, weight.by='diet.fraction')

CharacteristicPathLength(Benguela)
}
\keyword{utilities}

