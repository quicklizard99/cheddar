\name{ResourceLargerThanConsumer}
\alias{ResourceLargerThanConsumer}
\title{Resource larger than consumer}
\description{Trophic links in which the resource has a larger body mass than 
the consumer.}
\usage{ResourceLargerThanConsumer(community)}

\arguments{
  \item{community}{an object of class \code{Community}}
}
\details{Returns a \code{data.frame} with columns `resource', `consumer', 
`resource.M' and `consumer.M'.}

\value{A \code{data.frame}}

\author{Lawrence Hudson}

\seealso{\code{\link{Community}}}
\examples{
data(TL84)

ResourceLargerThanConsumer(TL84)

# Highlight trophic links
PlotNvM(TL84, highlight.links=ResourceLargerThanConsumer)
}
\keyword{utilities}

