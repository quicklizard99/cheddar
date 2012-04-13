\name{OrderCollection}
\alias{OrderCollection}
\title{Order a collection of communities}
\description{Order a \code{CommunityCollection}}
\usage{OrderCollection(collection, ..., decreasing=FALSE)}

\arguments{
  \item{collection}{an object of class \code{CommunityCollection}.}
  \item{\dots}{the names of properties by which to order the communities.}
  \item{decreasing}{logical.}
}

\value{A \code{CommunityCollection}.}

\author{Lawrence Hudson}

\seealso{\code{\link{CommunityCollection}}, 
         \code{\link{order}}, 
         \code{\link{CollectionCPS}}}

\examples{
data(pHWebs)
CollectionCPS(pHWebs, c('pH', 'NumberOfNodes'))

# Order by name
pHWebs.name <- OrderCollection(pHWebs, 'title')
CollectionCPS(pHWebs.name, c('pH', 'NumberOfNodes'))

# Order by decreasing pH
pHWebs.decreasing.pH <- OrderCollection(pHWebs, 'pH', decreasing=TRUE)
CollectionCPS(pHWebs.decreasing.pH, c('pH', 'NumberOfNodes'))

# Order by increasing diversity
pHWebs.increasing.S <- OrderCollection(pHWebs, 'NumberOfNodes')
CollectionCPS(pHWebs.increasing.S, c('pH', 'NumberOfNodes'))
}
\keyword{utilities}

