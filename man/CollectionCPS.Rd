\name{CollectionCPS}
\alias{CollectionCPS}
\title{Collection community properties}
\description{Returns a \code{data.frame} of first-class and computed properties 
of communities in a \code{CommunityCollection}.}
\usage{CollectionCPS(collection, properties=NULL)}

\arguments{
  \item{collection}{an object of class \code{CommunityCollection}.}
  \item{properties}{the names of the properties to be returned.}
}

\details{This function is named CollectionCPS for Collection Community 
PropertieS. If \code{properties} is \code{NULL}, all first-class properties 
are included in the returned \code{data.frame}.
}
\value{A \code{data.frame}.}

\author{Lawrence Hudson}

\seealso{\code{\link{CPS}}, \code{\link{CommunityPropertyNames}}, 
         \code{\link{CommunityCollection}}}

\examples{
data(pHWebs)

CollectionCPS(pHWebs)

# pH and a computed property
CollectionCPS(pHWebs, c('pH', 'NumberOfNodes'))

# A shorter name for the 'NumberOfNodes' column
CollectionCPS(pHWebs, c('pH', S='NumberOfNodes'))

# A function that returns more than one value
CollectionCPS(pHWebs, 'SumBiomassByClass')

# A shorted name for the 'SumBiomassByClass' columns
CollectionCPS(pHWebs, c(B='SumBiomassByClass'))
}
\keyword{utilities}
