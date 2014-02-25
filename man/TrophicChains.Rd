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
\code{data.frame} containing any requested node and trophic-link columns.

Some network properties and analyses require knowledge of every unique path 
- `trophic chain' - through the food-web. A trophic chain starts at a basal 
node (\code{BasalNodes}) and ends when it is not possible to 
add nodes that are not already in the chain, so loops and cannibalism are 
ignored. For communities that have one or more top-level nodes 
(\code{TopLevelNodes}) each trophic chain will end with a top-level node. 

If your analysis requires only simple statistics about trophic chains, the 
\code{TrophicChainsStats} function is more suitable as it is much faster and 
requires less memory than \code{TrophicChains}. This is particularly true 
for communities that contain a large number of trophic chains, such as the 
\code{SkipwithPond} dataset, which has more than $10^5$ unique chains.

It will not be possible to compute, within reasonable time and available system 
memory, trophic chains for food webs with a large number of nodes and/or 
trophic links. \code{TrophicChains} will raises an error `Unable to compute 
paths' for these food webs. The `Large numbers of trophic chains' section of 
the `Community' vignette explains this in more detail.
}

\value{A \code{data.frame}.}

\author{Lawrence Hudson}

\seealso{\code{\link{BasalNodes}}, \code{\link{TopLevelNodes}}, 
         \code{\link{TLPS}}, \code{\link{ThreeNodeChains}}, 
         \code{\link{TrophicChainsStats}}, \code{\link{SkipwithPond}}}

\examples{
data(TL84)

tc <- TrophicChains(TL84)

# Every chain starts with a basal node
BasalNodes(TL84)
first <- tc[,1]
all(IsBasalNode(TL84)[unique(first)])

# TL84 has a single top-level consumer - every trophic chain ends with this 
# consumer
TopLevelNodes(TL84)
# Get the last node in each chain
last <- apply(tc, 1, function(row) row[max(which(""!=row))])
unique(last)

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

