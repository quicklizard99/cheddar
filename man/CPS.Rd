\name{CPS}
\alias{CPS}
\title{Community properties}
\description{Returns a list of first-class and computed community properties.}
\usage{CPS(community, properties = NULL)}
\arguments{
  \item{community}{an object of class \code{Community}.}
  \item{properties}{the names of the properties to be returned.}
}

\details{This function is named CPS for Community PropertieS. 
If properties is \code{NULL}, all properties are returned. If properties is 
not \code{NULL} then a list containing that subset of community properties is 
returned. Elements will be \code{NA} for values of \code{property} not in 
\code{CommunityPropertyNames}.
}

\value{A \code{list}.}

\author{Lawrence Hudson}

\seealso{\code{\link{Community}}, \code{\link{CP}}, 
         \code{\link{CommunityPropertyNames}}}

\examples{
data(TL84)

# All properties
CPS(TL84)

# Just lat and long
CPS(TL84, c('lat', 'long'))

# lat and long and number of nodes
CPS(TL84, c('lat', 'long', 'NumberOfNodes'))

# lat and long and number of nodes, renamed
CPS(TL84, c('lat', 'long', S='NumberOfNodes'))

# 'not a property' is NA
CPS(TL84, c('lat', 'long', S='NumberOfNodes', 'not a property'))
}
\keyword{utilities}
