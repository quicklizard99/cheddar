\name{TrophicChains}
\alias{TrophicChains}
\title{Trophic chains}
\description{Enumerates every trophic chain in a food web.}
\usage{
TrophicChains(community, node.properties = NULL, chain.properties = NULL)
}

\arguments{
  \item{community}{an object of class \code{Community}.}
  \item{node.properties}{the names of the node properties to return. Should 
    meet the criteria of the \code{properties} parameter of \code{NPS}.}
  \item{chain.properties}{the names of chain properties to return.}
}

\details{Enumerates every trophic chain in the food-web and returns a 
\code{Chains} object containing any requested node and trophic-link columns.

If your analysis requires only simple statistics about trophic chains, the 
\code{TrophicChainsStats} function is more suitable as it is much faster and 
requires less memory than \code{TrophicChains}. This is particularly true 
for communities that contain a large number of trophic chains, such as the 
\code{SkipwithPond} dataset, which has more than $10^5$ unique chains.
}

\value{A \code{data.frame}.}

\author{Lawrence Hudson}

\seealso{\code{\link{TLPS}}, \code{\link{ThreeNodeChains}}, 
         \code{\link{TrophicChainsStats}}, \code{\link{SkipwithPond}}}

\examples{
data(TL84)

dim(TrophicChains(TL84))

# M of nodes
head(TrophicChains(TL84, node.properties='M'))

# M and N of nodes
head(TrophicChains(TL84, node.properties=c('M','N')))

# Skipwith Pond has more than 10e5 unique chains
data(SkipwithPond)
# Not all systems will be able to allocate the memory required to hold the 
# chains
\dontrun{dim(TrophicChains(SkipwithPond))}
}
\keyword{utilities}

