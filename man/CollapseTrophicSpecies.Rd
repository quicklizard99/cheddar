\name{CollapseTrophicSpecies}
\alias{CollapseTrophicSpecies}
\title{Collapse trophic species}
\description{Collapse trophic species.}
\usage{CollapseTrophicSpecies(community, include.isolated=TRUE, title=NULL, 
                              \dots)}

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

\references{Jonsson, T. and Cohen, J. E. and Carpenter, S. R. 2005 
Food webs, body size, and species abundance in ecological community 
description. Advances in Ecological Research \bold{36}, 1--84.}

\author{Lawrence Hudson}

\seealso{\code{\link{IsIsolatedNode}}, \code{\link{LumpNodes}}, 
         \code{\link{TrophicSpecies}}}

\examples{
data(TL84)

TL84.ts <- CollapseTrophicSpecies(TL84)
TrophicSpecies(TL84.ts)
}
\keyword{utilities}

