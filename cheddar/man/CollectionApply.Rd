\name{CollectionApply}
\alias{CollectionApply}
\title{Collection apply}
\description{Apply a function to every \code{Community} in a 
    \code{CommunityCollection}. Works the same as \code{lapply} but returns a 
    \code{CommunityCollection} rather than a \code{list}.}

\usage{CollectionApply(collection, f, \dots)}

\arguments{
  \item{collection}{an object of class \code{CommunityCollection}.}
  \item{f}{a function to be applied to each \code{Community}.}
  \item{\dots}{optional arguments passed to \code{f}.}
}

\value{A new object of class \code{CommunityCollection}.}

\author{Lawrence Hudson}

\seealso{\code{\link{CommunityCollection}}, \code{\link{lapply}}}

\examples{
data(pHWebs)

# Remove isolated nodes from each community
CollectionCPS(pHWebs, 'FractionIsolatedNodes')
pHWebs.no.iso <- CollectionApply(pHWebs, RemoveIsolatedNodes)
CollectionCPS(pHWebs.no.iso, 'FractionIsolatedNodes')


# Remove cannibalistic links from each community
sapply(pHWebs, function(community) length(Cannibals(community)))
pHWebs.no.can <- CollectionApply(pHWebs, RemoveCannibalisticLinks)
sapply(pHWebs.no.can, function(community) length(Cannibals(community)))


# Order the nodes each community by body mass
head(CollectionNPS(pHWebs))
pHWebs.by.M <- CollectionApply(pHWebs, OrderCommunity, 'M')
head(CollectionNPS(pHWebs.by.M))
}
\keyword{utilities}

