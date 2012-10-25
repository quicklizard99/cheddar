\name{PredationMatrixToLinks}
\alias{PredationMatrixToLinks}
\title{Predation matrix to trophic links}
\description{A function that converts a predation matrix to a 
\code{data.frame} with the columns `resource' and `consumer'.}

\usage{
PredationMatrixToLinks(pm)
}

\arguments{
  \item{pm}{a square \code{matrix} whose elements are either 0 or 1.}
}

\details{Returns a \code{data.frame} of trophic links contained within the 
\code{pm}. A 1 indicates a trophic link between a resource (row) and consumer 
(column). \code{pm} should have both row names and column names; row names must 
equal column names. The returned \code{data.frame} contains the columns 
`resource' and `consumer'. This function can be useful when importing data in 
to Cheddar.
}

\value{A \code{data.frame}}

\author{Lawrence Hudson}

\seealso{\code{\link{Community}}, 
         \code{\link{PredationMatrix}}, 
         \code{\link{TLPS}}}

\examples{
data(TL84)

links <- PredationMatrixToLinks(PredationMatrix(TL84))
identical(links, TLPS(TL84))    # TRUE

# Create a Cheddar community from an existing predation matrix
node <- c('Leaf', 'Caterpillar', 'Bluetit')
pm <- matrix( c(0, 1, 0,
                0, 0, 1,
                0, 0, 0), ncol=3, dimnames=list(node, node))

community <- Community(nodes=data.frame(node=node),
                       trophic.links=PredationMatrixToLinks(pm),
                       properties=list(title='Test community'))
}

\keyword{utilities}

