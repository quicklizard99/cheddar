\name{Degree}
\alias{Degree}
\alias{InDegree}
\alias{OutDegree}
\alias{TrophicGenerality}
\alias{TrophicVulnerability}
\alias{NumberOfResources}
\alias{NumberOfConsumers}
\alias{NormalisedTrophicGenerality}
\alias{NormalisedTrophicVulnerability}

\title{Node degree}
\description{The number of trophic links in to and out of nodes in a 
\code{Community}.}

\usage{
Degree(community)
InDegree(community)
TrophicGenerality(community)
NumberOfResources(community)
OutDegree(community)
TrophicVulnerability(community)
NumberOfConsumers(community)

NormalisedTrophicGenerality(community)
NormalisedTrophicVulnerability(community)
}

\arguments{
  \item{community}{an object of class \code{Community}.}
}

\details{\code{InDegree} and \code{OutDegree} return the number of trophic 
links in-to and out-of each node. \code{Degree} returns \code{InDegree} + 
\code{OutDegree}. \code{TrophicGenerality} and \code{NumberOfResources} 
are synonyms for \code{InDegree}. \code{TrophicVulnerability} and 
\code{NumberOfResources} are synonyms for \code{OutDegree}.

\code{NormalisedTrophicGenerality} and \code{NormalisedTrophicVulnerability} 
return the containing the number of resources and consumer of each node, 
normalised with respect to \code{LinkageDensity}. The mean of the values 
returned by both \code{NormalisedTrophicGenerality} and 
\code{NormalisedTrophicVulnerability} is 1, making their standard 
deviations comparable across different food webs.
}

\value{A vector of length \code{NumberOfNodes}.}

\references{Williams, R. J. and Martinez, N. D. (2000) Simple rules yield 
    complex food webs. Nature \bold{404}, 180--183.}

\author{Lawrence Hudson}

\seealso{\code{\link{Community}}, \code{\link{NumberOfNodes}}, 
         \code{\link{LinkageDensity}}, \code{\link{DirectedConnectance}}, 
         \code{\link{DegreeDistribution}}}

\examples{
data(TL84)

d <- Degree(TL84)
i <- InDegree(TL84)
o <- OutDegree(TL84)

# This equality is always TRUE for all food webs
all(d == i+o)

ntg <- NormalisedTrophicGenerality(TL84)
mean(ntg)    # Equals 1
ntv <- NormalisedTrophicVulnerability(TL84)
mean(ntv)    # Equals 1
}
\keyword{utilities}

