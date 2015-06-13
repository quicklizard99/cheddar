\name{NumberOfNodes}
\alias{NumberOfNodes}
\alias{NumberOfNodesByClass}
\alias{FractionOfNodesByClass}
\title{Number of nodes}
\description{Functions that return the number of nodes in the community.}

\usage{
NumberOfNodes(community)
NumberOfNodesByClass(community, class)
FractionOfNodesByClass(community, class)
}

\arguments{
  \item{community}{an object of class \code{Community}.}
  \item{class}{the property over which \code{fn} is applied. Defaults to 
  'category' if the community has a node property with that name.}
}

\value{\code{NumberOfNodes} returns a single number. 
  \code{NumberOfNodesByClass} and \code{FractionOfNodesByClass} both return 
  a value for each \code{class}.}

\author{Lawrence Hudson}

\seealso{\code{\link{Community}}, \code{\link{NPS}}}

\examples{
data(TL84)
NumberOfNodes(TL84)
NumberOfNodesByClass(TL84)
FractionOfNodesByClass(TL84)
}

\keyword{utilities}
