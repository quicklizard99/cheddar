\name{TrophicChainsStats}
\alias{TrophicChainsStats}
\title{Trophic chains statistics}
\description{Computes simple statistics about every trophic chain in a food 
  web.}
\usage{
TrophicChainsStats(community)
}

\arguments{
  \item{community}{an object of class \code{Community}.}
}

\details{Enumerates every trophic chain in the food-web and returns a 
\code{list} object containing some simple statistics. 
If your analysis requires only simple statistics about trophic chains then 
this function is more suitable than \code{TrophicChains} 
because it is faster and requires less memory.
}

\value{A list containing:
\item{chain.lengths}{The number of nodes in each trophic chain.}
\item{node.pos.counts}{A \code{matrix} of \code{NumberOfNodes} rows and 
  \code{1+max(chain.lengths)} columns. Elements are the number of chains in 
  which a node appear in that position.}
}

\author{Lawrence Hudson}

\seealso{\code{\link{TrophicChains}}, \code{\link{NumberOfNodes}}, 
         \code{\link{IsolatedNodes}}, \code{\link{BasalNodes}},
         \code{\link{IntermediateNodes}}, \code{\link{TopLevelNodes}}}

\examples{
data(TL84)
chain.stats <- TrophicChainsStats(TL84)

# The length of every chain
length(chain.stats$chain.lengths)    # 5,988 chains
summary(chain.stats$chain.lengths)

# The number of chains in which a node appears in that position in a chain
chain.stats$node.pos.counts

# Basal nodes only have counts in the first column.
chain.stats$node.pos.counts[BasalNodes(TL84),]

# Consumers only have counts in columns two and up.
chain.stats$node.pos.counts[c(IntermediateNodes(TL84),TopLevelNodes(TL84)),]

# All counts are zero for isolated nodes IsolatedNodes.
chain.stats$node.pos.counts[IsolatedNodes(TL84),]
}
\keyword{utilities}

