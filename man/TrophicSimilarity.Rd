\name{TrophicSimilarity}
\alias{TrophicSimilarity}
\alias{MeanMaximumTrophicSimilarity}
\title{Trophic similarity}
\description{A measure of trophic overlap between nodes in a community.
}

\usage{
TrophicSimilarity(community)
MeanMaximumTrophicSimilarity(community) 
}

\arguments{
  \item{community}{an object of class \code{Community}.}
}

\details{\code{TrophicSimilarity} computes `trophic similarity' (I) as defined 
by Martinez (1991). For each pair of nodes, I = c/(a+b+c), where a is the 
number of resources and consumers unique to one node, b is number of resources 
and consumers unique to the other node and c is the number of resources and 
consumers common to both nodes. Where two nodes have exactly the same set of 
resources and consumers, I = 1. Where two nodes have no resources or consumers 
in common, I = 0.

Williams and Martinez (2000) defined the mean maximum trophic similarity as the 
sum of the largest value in each column of I (excluding the diagonal), divided 
by the number of nodes.
}

\value{\code{TrophicSimilarity} returns a \code{matrix} with 
\code{NumberOfNodes} rows and columns. \code{MeanMaximumSimilarity} returns 
a number.}

\references{Martinez, N. D. (1991) Ecological Monographs \bold{61}, 367--392.

Williams, R. J. and Martinez, N. D. (2000) Nature \bold{404} 180--182.
}

\author{Lawrence Hudson}

\seealso{\code{\link{PredationMatrix}}, \code{\link{NumberOfNodes}}, 
         \code{\link{TrophicSpecies}}}

\examples{
data(TL84)

I <- TrophicSimilarity(TL84)
I

MeanMaximumTrophicSimilarity(TL84)
}
\keyword{utilities}
