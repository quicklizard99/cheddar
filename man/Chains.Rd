\name{Chains}
\alias{Chains}
\alias{is.Chains}
\alias{ChainLength}
\title{\code{Chains} object}
\description{An object that represents trophic chains.}

\usage{ChainLength(chains)}

\arguments{
  \item{chains}{an object of class \code{Chains}.}
}

\details{\code{Chains} objects are a subclass of \code{data.frame} 
that contain an attribute indicating which columns represented links in chains 
and which are chains or node properties. \code{Chains} objects are returned by 
\code{ThreeNodeChains} and \code{TrophicChains}. 

\code{ChainLength} returns the length of each chain in \code{chains}.
}

\value{A vector of numbers.}

\author{Lawrence Hudson}

\seealso{\code{\link{ThreeNodeChains}}, \code{\link{TrophicChains}}}

\examples{
data(TL84)

chains <- TrophicChains(TL84, node.properties='M')
class(chains)
is.Chains(chains)
head(chains)
ChainLength(chains)
}
\keyword{utilities}
