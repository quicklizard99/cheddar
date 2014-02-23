\name{ThreeNodeChains}
\alias{ThreeNodeChains}
\title{Three-node chains}
\description{Enumerates every three-node chain in a food web.}
\usage{
ThreeNodeChains(community, exclude.loops=FALSE, node.properties=NULL, 
                chain.properties=NULL)
}

\arguments{
  \item{community}{an object of class \code{Community}.}
  \item{exclude.loops}{logical - should loops A -> B -> A be included?}
  \item{node.properties}{the names of the node properties to return. Should 
    meet the criteria of the \code{properties} parameter of \code{NPS}.}
  \item{chain.properties}{the names of chain properties to return.}
}

\details{Enumerates every three-node chain in the food-web and returns a 
\code{Chains} object containing the columns bottom, intermediate and 
top and any requested node and trophic-link columns.}

\value{A \code{data.frame}.}

\author{Lawrence Hudson}

\seealso{\code{\link{TLPS}}, \code{\link{TrophicChains}}}

\examples{
data(TL84)

nrow(ThreeNodeChains(TL84))
nrow(ThreeNodeChains(TL84, exclude.loops=TRUE))

# bottom, intermediate and top
head(ThreeNodeChains(TL84))

# bottom, intermediate, top, bottom.M, intermediate.M and top.M
head(ThreeNodeChains(TL84, node.properties='M'))

# As above with the addition of bottom.N, intermediate.N and top.N
head(ThreeNodeChains(TL84, node.properties=c('M','N')))
}
\keyword{utilities}

