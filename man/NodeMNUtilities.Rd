\name{Body mass, numerical abundance and biomass abundance}
\alias{Log10M}
\alias{Log10N}
\alias{Biomass}
\alias{Log10Biomass}
\alias{Log10MNBiomass}
\alias{RCMRatio}
\alias{Log10RCMRatio}
\alias{CRMRatio}
\alias{Log10CRMRatio}
\title{Body mass, numerical abundance and biomass abundance}
\description{Convenience functions for accessing log10-transformed body mass, 
M, numerical abundance, N, and biomass abundance, B.}

\usage{
Log10M(community)
Log10N(community)
Biomass(community)
Log10Biomass(community)
Log10MNBiomass(community)

RCMRatio(community)
Log10RCMRatio(community)
CRMRatio(community)
Log10CRMRatio(community)
}

\arguments{
  \item{community}{an object of class \code{Community}.}
}

\details{\code{Log10M}, \code{Log10N}, \code{Biomass}, \code{Log10Biomass} and 
each return a value per node. \code{Log10MNBiomass} returns a matrix with a row 
per node and columns `Log10M', `Log10N' and `Log10Biomass'. These functions 
are all suitable for use with \code{NPS}.

\code{RCMRatio} returns the ratio between the resource and consumer body mass 
for every trophic link. \code{Log10RCMRatio} returns the same data 
log10-transformed. \code{CRMRatio} and \code{Log10CRMRatio} are analagous 
functions that return the ratio between the consumer and resource body mass. 
These functions are all suitable for use with \code{TLPS}.
}

\value{A vector of length \code{NumberOfNodes} or a vector of length 
\code{NumberOfTrophicLinks}}

\author{Lawrence Hudson}

\seealso{\code{\link{NumberOfNodes}}, \code{\link{NPS}}, 
         \code{\link{NumberOfTrophicLinks}}, \code{\link{TLPS}}}

\examples{
data(TL84)

NPS(TL84, c('M', 'Log10M', 'N', 'Log10N', 'Biomass', 'Log10Biomass'))

NPS(TL84, 'Log10MNBiomass')

TLPS(TL84, link.properties=c('Log10RCMRatio', 'Log10CRMRatio'))
}
\keyword{utilities}

