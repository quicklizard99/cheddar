\name{CP}
\alias{CP}
\title{Single community properties}
\description{Returns a single community property or \code{NA} if property is 
not in \code{CommunityPropertyNames}.}

\usage{CP(community, property)}
\arguments{
  \item{community}{an object of class \code{Community}.}
  \item{property}{the name of the community property to be returned.}
}

\details{This function is named CP for Community Property.}

\value{A single community property.}

\author{Lawrence Hudson}

\seealso{\code{\link{Community}}, \code{\link{CPS}}, 
         \code{\link{CommunityPropertyNames}}}

\examples{
data(TL84)
CP(TL84, 'title')

CP(TL84, 'lat')

CP(TL84, 'M.units')

# Returns a vector of NA
CP(TL84, 'not a property')
}

\keyword{utilities}
