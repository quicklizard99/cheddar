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
  \item{\dots}{values passed to \code{AggregateCommunities}.}
}

\value{A new object of class \code{CommunityCollection}.}

\author{Lawrence Hudson}

\seealso{\code{\link{CommunityCollection}}, \code{\link{lapply}}}
\examples{
data(pHWebs)
CollectionCPS(pHWebs, 'FractionIsolatedNodes')

pHWebs.no.iso <- CollectionApply(pHWebs, RemoveIsolatedNodes)
CollectionCPS(pHWebs.no.iso, 'FractionIsolatedNodes')
}
\keyword{utilities}

