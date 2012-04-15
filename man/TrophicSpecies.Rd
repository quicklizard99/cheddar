\name{TrophicSpecies}
\alias{TrophicSpecies}
\title{Trophic species}
\description{A function that computes trophic species numbers.}
\usage{TrophicSpecies(community, include.isolated=TRUE)}

\arguments{
  \item{community}{an object of class \code{Community}.}
  \item{include.isolated}{if \code{TRUE} then nodes for which 
    \code{IsIsolatedNode} is \code{TRUE} are given their own trophic species 
    number. If \code{FALSE} the isolated species are assigned a trophic species 
    of \code{NA}.}
}

\details{Returns a vector containing the trophic species number of each
node in the community. Nodes with identical sets of prey and predators
are given the same trophic species number.}

\value{A vector of length \code{NumberOfNodes}.}

\references{
Briand, F and Cohen, J. E. 1984 Community food webs have scale-invariant 
structure Nature \bold{307}, 264--267.

Pimm, S. L. and Lawton, J. H. and Cohen, J. E. 1991 Food web patterns and their 
consequences Nature \bold{350}, 669--674.

Williams, R. J. and Martinez, N. D. 2000 Simple rules yield complex food webs
\bold{404}, 180--183.

Jonsson, T. and Cohen, J. E. and Carpenter, S. R. 2005 
Food webs, body size, and species abundance in ecological community 
description. Advances in Ecological Research \bold{36}, 1--84.}

\author{Lawrence Hudson}

\seealso{\code{\link{Community}}, \code{\link{IsIsolatedNode}}, 
         \code{\link{NPS}}, \code{\link{LumpTrophicSpecies}}, 
         \code{\link{NumberOfNodes}}}

\examples{
data(TL84)

# Isolated nodes assigned their own trophic species number
TrophicSpecies(TL84)

# Isolated nodes assigned a trophic species of NA
TrophicSpecies(TL84, include.isolated=FALSE)

# Compare including and excluding isolated nodes
NPS(TL84, list(TS1='TrophicSpecies', 
               TS2=list('TrophicSpecies', include.isolated=FALSE), 
               Iso='IsIsolatedNode'))
}
\keyword{utilities}

