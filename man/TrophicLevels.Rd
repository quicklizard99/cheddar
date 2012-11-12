\name{TrophicLevels}
\alias{PreyAveragedTrophicLevel}
\alias{FlowBasedTrophicLevel}
\alias{TrophicLevels}
\alias{ShortestTrophicLevel}
\alias{ShortWeightedTrophicLevel}
\alias{LongestTrophicLevel}
\alias{LongWeightedTrophicLevel}
\alias{ChainAveragedTrophicLevel}
\alias{TrophicHeight}
\title{Trophic levels}
\description{Functions that compute different measures of trophic level.}
\usage{
PreyAveragedTrophicLevel(community, include.isolated=TRUE)
FlowBasedTrophicLevel(community, weight.by, include.isolated=TRUE)
ShortestTrophicLevel(community, include.isolated=TRUE)
ShortWeightedTrophicLevel(community, include.isolated=TRUE)
LongestTrophicLevel(community, include.isolated=TRUE)
LongWeightedTrophicLevel(community, include.isolated=TRUE)
ChainAveragedTrophicLevel(community, include.isolated=TRUE)
TrophicHeight(community, include.isolated=TRUE)

TrophicLevels(community, weight.by=NULL, include.isolated=TRUE)
}

\arguments{
  \item{community}{an object of class \code{Community}.}
  \item{include.isolated}{if \code{FALSE} then nodes for which 
    \code{IsIsolatedNode} is \code{TRUE} are given a trophic level of 
    \code{NA}.}
  \item{weight.by}{the name of a node property, either first-class or computed, 
    by which to weight flow-based trophic level. Must satisfy the criteria 
    of the \code{properties} parameters of \code{NPS}.}
}

\details{Trophic level is a measure of a node's `distance' from the primary 
producers in the community and hence indicates how many steps matter, and hence 
energy, has been through to reach that node. Each function (with the exception 
of \code{TrophicLevels}) returns a vector containing a different measure of 
trophic level. These functions follow the definitions of Williams and Martinez 
(2004).

\code{PreyAveragedTrophicLevel} returns 1 plus the mean trophic level of the 
node's resources, using the matrix inversion method of Levine (1980) that is 
very fast and accounts for flow through loops. If this matrix inversion fails, 
there is an important problem with the network topology. For a food web to be 
energetically feasible, every node must be connected to a basal node. When the 
inversion fails it is because there is at least one node that has no connection 
to a basal node. \code{FlowBasedTrophicLevel} also implements the matrix 
inversion technique and uses the \code{weight.by} node property to provide an 
estimate of energy flow through each trophic link.

\code{ShortestTrophicLevel}, \code{ShortWeightedTrophicLevel}, 
\code{LongestTrophicLevel}, \code{LongWeightedTrophicLevel} 
and \code{ChainAveragedTrophicLevel} compute trophic level by 
examining the position of each node in every food chain in which it appears. 
\code{ShortestTrophicLevel} returns 1 plus the shortest chain length from a 
node to a basal species. \code{ShortWeightedTrophicLevel} returns the average 
of \code{ShortestTrophicLevel} and \code{PreyAveragedTrophicLevel}. 
\code{LongestTrophicLevel} is the longest chain length from each node to a 
basal species. \code{LongWeightedTrophicLevel} is the average of 
\code{LongestTrophicLevel} and \code{PreyAveragedTrophicLevel}. 
\code{ChainAveragedTrophicLevel} is 1 plus the average chain length of all 
paths from each node to a basal species. These five functions each 
enumerate every unique food chain (using \code{TrophicChainsStats}), which can 
be lengthy for complex food webs. If more than one of these five measures of 
trophic level is required, it will be faster to use the \code{TrophicLevels} 
convenience function, which enumerates unique food chains only once and returns 
a matrix containing every measure of trophic level in columns `ShortestTL', 
`ShortWeightedTL', `LongestTL', `LongWeightedTL', `ChainAveragedTL', 
`PreyAveragedTL' and, if \code{weight.by} is given, `FlowBasedTL'.

Jonsson et al (2005) defined `trophic height' to be the same as 
Williams and Martinez' (2004) chain-averaged trophic level. 
\code{TrophicHeight} is therefore a synonym for 
\code{ChainAveragedTrophicLevel}. 
}

\value{Either a vector of length \code{NumberOfNodes} or a \code{matrix} with 
\code{NumberOfNodes} rows.}

\references{Jonsson, T. and Cohen, J. E. and Carpenter, S. R. (2005) 
Food webs, body size, and species abundance in ecological community 
description. Advances in Ecological Research \bold{36}, 1--84.

Levine, S (1980) Several measures of trophic structure applicable to complex 
food webs. Journal of Theoretical Biology \bold{83}, 195--207.

Williams, R. J. and Martinez, N. D. (2004) Limits to Trophic Levels 
and Omnivory in Complex Food Webs: Theory and Data. American Naturalist 
\bold{163}, 63, 458--468.}

\author{Lawrence Hudson and Rich Williams}

\seealso{\code{\link{IsIsolatedNode}}, \code{\link{NPS}}, 
         \code{\link{TrophicChains}}, \code{\link{TrophicChainsStats}}, 
q         \code{\link{PredationMatrix}}}

\examples{
data(TL84)

# Six different measures of trophic level
TrophicLevels(TL84)

# The Benguela data contains diet.fraction
data(Benguela)

# Compare prey-averaged and flow-based
cbind(pa=PreyAveragedTrophicLevel(Benguela), 
      fb=FlowBasedTrophicLevel(Benguela, weight.by='diet.fraction'))
}
\keyword{utilities}

