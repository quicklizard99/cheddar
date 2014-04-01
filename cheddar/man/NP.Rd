\name{NP}
\alias{NP}
\title{Single node properties}
\description{Returns a node property.}
\usage{NP(community, property)}

\arguments{
  \item{community}{an object of class \code{Community}.}
  \item{property}{the name of the property to return.}
}

\details{This function is named NP for Node Property. It returns a vector 
containing the value of \code{property} for every node. The returned vector 
is named by node. If the name is not a property, a vector of \code{NA} is 
returned.}

\value{A vector of length \code{NumberOfNodes}.}

\author{Lawrence Hudson}

\seealso{\code{\link{NPS}}, \code{\link{NumberOfNodes}}}

\examples{
data(TL84)

NP(TL84, 'M')

# Returns a vector of NA
NP(TL84, 'not a property')
}

\keyword{utilities}
