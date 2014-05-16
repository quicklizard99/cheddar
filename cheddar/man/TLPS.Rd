\name{TLPS}
\alias{TLPS}
\title{Trophic-link properties}
\description{Returns a \code{data.frame} of first-class and computed 
trophic-link properties.}
\usage{TLPS(community, node.properties=NULL, link.properties=NULL)}

\arguments{
  \item{community}{an object of class \code{Community}.}
  \item{node.properties}{the names of the node properties to return. Should 
    meet the criteria of the \code{properties} parameter of \code{NPS}.}
  \item{link.properties}{the names of link properties. These can be names of 
    first-class properties (returned by \code{TrophicLinkPropertyNames}) and 
    names of functions that take a \code{Community} object as the only 
    parameter and return either a vector of length \code{NumberOfTrophicLinks} 
    or a \code{matrix} or \code{data.frame} with \code{NumberOfTrophicLinks} 
    rows.}
}

\details{This function is named TLPS for Trophic Link PropertieS. It returns 
a \code{data.frame} containing the columns 'resource' and 'consumer' 
and any requested properties.}

\value{A \code{data.frame} with \code{NumberOfTrophicLinks} rows.}

\author{Lawrence Hudson}

\seealso{\code{\link{TrophicLinkPropertyNames}}, \code{\link{TLP}}, 
         \code{\link{NumberOfTrophicLinks}}, \code{\link{NPS}}, 
         \code{\link{Log10RCMRatio}}, 
         \code{\link{ThreeNodeChains}}, \code{\link{TrophicChains}}}

\examples{
data(TL84)

# Just resource and consumer
head(TLPS(TL84))

# resource, consumer, resource.M and consumer.M
head(TLPS(TL84, node.properties='M'))

# Log10RCMRatio returns log10-transformed resource.M / consumer.M
head(TLPS(TL84, node.properties='M', link.properties='Log10RCMRatio'))

# Skipwith Pond has link.evidence and link.life.stage first-class properties
data(SkipwithPond)
head(TLPS(SkipwithPond))

# resource, consumer and link.evidence
head(TLPS(SkipwithPond, link.properties='link.evidence'))

# Skipwith Pond has diet.fraction first-class property
data(Benguela)
head(TLPS(Benguela))
}
\keyword{utilities}

