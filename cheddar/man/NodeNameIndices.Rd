\name{NodeNameIndices}
\alias{NodeNameIndices}
\title{Node name indices}
\description{Node name indices.}
\usage{NodeNameIndices(community, nodes)}

\arguments{
  \item{community}{an object of class \code{Community}.}
  \item{nodes}{node names.}
}

\details{Returns integer indices of names in \code{nodes}.}

\value{A vector of integers}

\author{Lawrence Hudson}

\seealso{\code{\link{Community}}}

\examples{
data(TL84)

NodeNameIndices(TL84, 'Umbra limi')
NodeNameIndices(TL84, c('Nostoc sp.','Umbra limi'))
}
\keyword{utilities}

