\name{NumberOfTrophicLinks}
\alias{NumberOfTrophicLinks}
\alias{LinkageDensity}
\alias{DirectedConnectance}
\title{Number of trophic links}
\description{The number of trophic links in \code{Community}.}

\usage{
NumberOfTrophicLinks(community) 
LinkageDensity(community) 
DirectedConnectance(community) 
}

\arguments{
  \item{community}{an object of class \code{Community}.}
}

\details{\code{NumberOfTrophicLinks} returns the total number of links in the 
web, including cannibalistic links.

\code{LinkageDensity} returns the \code{NumberOfTrophicLinks} / 
\code{NumberOfNodes}, including cannibalistic links and isolated nodes.

\code{DirectedConnectance} returns \code{NumberOfTrophicLinks} / 
\code{NumberOfNodes}^2, including cannibalistic links and isolated nodes.
}

\value{A single number.}

\references{Martinez, N. D. 1991 Artifacts or attributes? Effects of resolution 
on the Little Rock Lake food web. Ecological Monographs \bold{61}, 367--392.}

\author{Lawrence Hudson}

\seealso{\code{\link{NumberOfNodes}}}

\examples{
data(TL84)

NumberOfTrophicLinks(TL84)
LinkageDensity(TL84)
DirectedConnectance(TL84)
}
\keyword{utilities}

