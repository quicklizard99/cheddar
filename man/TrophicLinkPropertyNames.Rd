\name{TrophicLinkPropertyNames}
\alias{TrophicLinkPropertyNames}
\title{Trophic link property names}
\description{Returns the names of the first-class trophic link properties in a 
community.}
\usage{TrophicLinkPropertyNames(community)}

\arguments{
  \item{community}{an object of class \code{Community}.}
}

\details{The names 'resource' and 'consumer' are always returned.}

\value{Two or more characters.}

\author{Lawrence Hudson}

\seealso{\code{\link{Community}}, \code{\link{TLP}}, \code{\link{TLPS}}}

\examples{
data(TL84, SkipwithPond)

# Just 'resource' and 'consumer'
TrophicLinkPropertyNames(TL84) 

# Just 'resource', 'consumer', 'link.evidence' and 'link.life.stage'
TrophicLinkPropertyNames(SkipwithPond)
}
\keyword{utilities}

