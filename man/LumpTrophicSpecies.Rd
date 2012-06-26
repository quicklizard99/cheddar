\name{LumpTrophicSpecies}
\alias{LumpTrophicSpecies}
\title{Lump trophic species}
\description{Lump trophic species.}
\usage{LumpTrophicSpecies(community, include.isolated=TRUE, title=NULL, \dots)}

\arguments{
  \item{community}{an object of class \code{Community}.}
  \item{include.isolated}{if \code{TRUE} then nodes for which 
    \code{IsIsolatedNode} is \code{TRUE} are given their own trophic species 
    number. If \code{FALSE} the isolated species are assigned a trophic species 
    of \code{NA}.}
  \item{title}{the title of the new \code{Community}.}
  \item{\dots}{other parameters to \code{LumpNodes}.}
}

\details{Aggregates nodes that share identical sets of prey and predators.}

\value{A \code{Community}.}

\references{
Briand, F and Cohen, J. E. 1984 Community food webs have scale-invariant 
structure Nature \bold{307}, 264--267.

Jonsson, T. and Cohen, J. E. and Carpenter, S. R. 2005 
Food webs, body size, and species abundance in ecological community 
description. Advances in Ecological Research \bold{36}, 1--84.

Pimm, S. L. and Lawton, J. H. and Cohen, J. E. 1991 Food web patterns and their 
consequences Nature \bold{350}, 669--674.

Williams, R. J. and Martinez, N. D. 2000 Simple rules yield complex food webs
\bold{404}, 180--183.}

\author{Lawrence Hudson}

\seealso{\code{\link{TrophicSpecies}}, \code{\link{LumpNodes}}, 
         \code{\link{IsIsolatedNode}}}

\examples{
data(TL84)
NumberOfNodes(TL84)

TL84.lumped <- LumpTrophicSpecies(TL84)

length(unique(TrophicSpecies(TL84)))    # 22 trophic species in TL84...
NumberOfNodes(TL84.lumped)              # ... and 22 nodes in the lumped web
}
\keyword{utilities}

