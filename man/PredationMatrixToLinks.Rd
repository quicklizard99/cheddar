\name{PredationMatrixToLinks}
\alias{PredationMatrixToLinks}
\title{Predation matrix to trophic links}
\description{A function that converts a predation matrix to a 
\code{data.frame} with the columns `resource' and `consumer'.}

\usage{
PredationMatrixToLinks(pm, link.property=NULL)
}

\arguments{
  \item{pm}{a \code{matrix} or \code{data.frame}.}
  \item{link.property}{either \code{NULL} or a character.}
}

\details{Returns a \code{data.frame} of trophic links contained within
\code{pm}. Non-zero and non-\code{NA} values indicate a trophic link between 
a resource (row) and consumer (column). \code{pm} should have both row names 
and column names. The returned \code{data.frame} will contain the columns 
`resource' and `consumer'. If \code{pm} contains quantitative information 
such as diet fractions or number of observations then you can set 
\code{link.property} to the name of the quantity and the returned 
\code{data.frame} will include a column with that name, that contains link 
strength values extracted from \code{pm}.

If you have existing food-web data in predation-matrix form then this function 
can help to import your data in to Cheddar.
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

# Create a Cheddar community from an existing square predation matrix
node <- c('Leaf', 'Caterpillar', 'Bluetit')
pm <- matrix( c(0, 1, 0,
                0, 0, 1,
                0, 0, 0), 
             ncol=3, byrow=TRUE, dimnames=list(node, node))

community1 <- Community(nodes=data.frame(node=node),
                        trophic.links=PredationMatrixToLinks(pm),
                        properties=list(title='Test community'))
TLPS(community1)

# The same set of trophic links could be represented by a non-square predation 
# matrix
pm <- matrix( c(1, 0,
                0, 1), 
             ncol=2, byrow=TRUE, 
             dimnames=list(node[1:2], node[2:3]))

community2 <- Community(nodes=data.frame(node=node),
                        trophic.links=PredationMatrixToLinks(pm),
                        properties=list(title='Test community'))
TLPS(community2)

all.equal(community1, community2)    # TRUE

# Extract quantitative information
node <- c('Leaf 1', 'Leaf 2', 'Caterpillar 1', 'Caterpillar 2')
pm <- matrix( c(0, 0, 0.4, 0.8, 
                0, 0, 0.6, 0.2, 
                0, 0, 0,   0, 
                0, 0, 0,   0), 
             ncol=4, byrow=TRUE, dimnames=list(node, node))

# A data.frame that has a column called diet.fraction
PredationMatrixToLinks(pm, link.property='diet.fraction')
}

\keyword{utilities}
