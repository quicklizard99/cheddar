\name{CollectionTLPS}
\alias{CollectionTLPS}
\title{Collection trophic-link properties}
\description{Returns a \code{data.frame} of first-class and computed 
trophic-link properties of communities in a \code{CommunityCollection}.}
\usage{CollectionTLPS(collection, node.properties=NULL, link.properties=NULL)}

\arguments{
  \item{collection}{an object of class \code{CommunityCollection}.}
  \item{node.properties}{the names of the node properties to return. Should 
    meet the critera of the \code{node.properties} parameter of 
    \code{TLPS}.}
  \item{link.properties}{the names of the trophic link properties to return. 
    Should meet the critera of the \code{link.properties} parameter of 
    \code{TLPS}.}
}

\details{This function is named CollectionTLPS for Collection Trophic Link 
PropertieS. It returns a \code{data.frame} containing the columns `resource' 
and `consumer' and any requested node and trophic-link properties. 
If \code{node.properties} and \code{link.properties} are both \code{NULL} then 
all first-class trophic-link properties are included in the returned 
\code{data.frame}.
}

\value{A \code{data.frame}.}

\author{Lawrence Hudson}

\seealso{\code{\link{TLPS}}, \code{\link{CommunityCollection}}}

\examples{
data(pHWebs)

# Just community, resource and consumer
head(CollectionTLPS(pHWebs), 10)

# The M of the resource and consumer in each link
head(CollectionTLPS(pHWebs, node.properties='M'), 10)
}
\keyword{utilities}

