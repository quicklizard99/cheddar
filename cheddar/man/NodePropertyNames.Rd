\name{NodePropertyNames}
\alias{NodePropertyNames}
\title{A vector of names of node properties}
\description{Returns a vector of names of node properties.}
\usage{NodePropertyNames(community)}

\arguments{
  \item{community}{an object of class \code{Community}.}
}

\value{A vector of the names of node properties.}

\author{Lawrence Hudson}

\seealso{\code{\link{NP}}, \code{\link{NPS}}}

\examples{
data(TL84) 
NodePropertyNames(TL84) 
}
\keyword{utilities}

