\name{TrophicChains}
\alias{TrophicChains}
\title{Trophic chains}
\description{Enumerates every trophic chain in a food web.}
\usage{TrophicChains(community, node.properties = NULL, chain.properties = NULL, 
                     max.chains)}

\arguments{
  \item{community}{an object of class \code{Community}.}
  \item{node.properties}{the names of the node properties to return. Should 
    meet the criteria of the \code{properties} parameter of \code{NPS}.}
  \item{chain.properties}{the names of chain properties to return.}
  \item{max.chains}{an integer. Default 1e5.}
}

\details{Enumerates every trophic chain in the food-web and returns a 
\code{Chains} object containing any requested node and trophic-link columns.
An error is raised if there are more than \code{max.chains} chains in the 
food web.}

\value{An object of class \code{Chains}.}

\author{Lawrence Hudson}

\seealso{\code{\link{TLPS}}, \code{\link{ThreeNodeChains}}, 
         \code{\link{Chains}}}

\examples{
data(TL84)

dim(TrophicChains(TL84))

# M of nodes
head(TrophicChains(TL84, node.properties='M'))

# M and N of nodes
head(TrophicChains(TL84, node.properties=c('M','N')))

# Skipwith Pond has more than 1e5 unique chains
data(SkipwithPond)
# This fails:
\dontrun{dim(TrophicChains(SkipwithPond))}
# This succeeds
dim(TrophicChains(SkipwithPond, max.chains=3e6))
}
\keyword{utilities}

