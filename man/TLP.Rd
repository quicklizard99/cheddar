\name{TLP}
\alias{TLP}
\title{A single trophic-link property}
\description{Returns a single trophic-link property.}
\usage{TLP(community, property)}

\arguments{
  \item{community}{an object of class \code{Community}.}
  \item{property}{the name of the property to return.}
}

\details{This function is named TLP for Trophic Link Property. It returns a 
vector containing the value of property for every trophic link. The returned 
vector is all \code{NA} if there is no trophic-link property with that name.
}

\value{A vector of length \code{NumberOfTrophicLinks}.}

\author{Lawrence Hudson}

\seealso{\code{\link{TrophicLinkPropertyNames}}, \code{\link{TLPS}}, 
         \code{\link{NumberOfTrophicLinks}}}

\examples{
# Skipwith Pond has a first-class property called link.evidence
data(SkipwithPond)
TLP(SkipwithPond, 'link.evidence') 

# Benguela has a first-class property called diet.fraction
data(Benguela) 
TLP(Benguela, 'diet.fraction') 

# All NA
TLP(SkipwithPond, 'not a property') 
}
\keyword{utilities}

