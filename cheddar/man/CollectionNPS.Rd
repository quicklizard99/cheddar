\name{CollectionNPS}
\alias{CollectionNPS}
\title{Collection node properties}
\description{Returns a \code{data.frame} of first-class and computed node 
properties of communities in a \code{CommunityCollection}.}
\usage{CollectionNPS(collection, properties=NULL)}

\arguments{
  \item{collection}{an object of class \code{CommunityCollection}.}
  \item{properties}{names of the properties. These can be names of first-class 
    properties and names of functions. Names must meet the criteria of 
    the \code{properties} parameter of \code{NPS}.}
}

\details{This function is named CollectionNPS for Collection Node PropertieS.
If \code{properties} is \code{NULL}, all first-class node properties are 
included in the returned \code{data.frame}.
}

\value{A \code{data.frame}.}
\author{Lawrence Hudson}

\seealso{\code{\link{NPS}}, \code{\link{CommunityCollection}}}

\examples{
data(pHWebs) 
head(CollectionNPS(pHWebs), 10)

head(CollectionNPS(pHWebs, 'M'), 10)

# Biomass is a function
head(CollectionNPS(pHWebs, 'Biomass'), 10)

head(CollectionNPS(pHWebs, c(B='Biomass')), 10)

# Several first-class and computed properties
head(CollectionNPS(pHWebs, c('M', 'N', B='Biomass', 'TrophicSpecies', 
                             TL='PreyAveragedTrophicLevel')), 10)

# Pass parameters to functions
head(CollectionNPS(pHWebs, 
                   list(TS1='TrophicSpecies', 
                        TS2=list('TrophicSpecies', include.isolated=FALSE), 
                        Iso='IsIsolatedNode')), 10)
}
\keyword{utilities}
