\name{RemoveNodes}
\alias{RemoveNodes}
\title{Remove nodes}
\description{Remove one or more nodes.}
\usage{RemoveNodes(community, remove, title, secondary, cascade)}

\arguments{
  \item{community}{an object of class \code{Community}.}
  \item{remove}{a vector of either names or integer indices (but not both) of 
    nodes to be removed.}
  \item{title}{a title for the new community.}
  \item{secondary}{if \code{TRUE}, secondarily extinct nodes are removed.}
  \item{cascade}{if secondary and cascade are both \code{TRUE}, secondary 
    extinctions are propogated.}
}
\details{Returns a new \code{Community} with nodes in \code{remove} removed. 
An error is raised if \code{remove} refers to nodes not in the 
\code{community} of if \code{remove} refers to all nodes in the 
\code{community}. 
}

\value{A new object of class \code{Community}.}
\author{Lawrence Hudson}

\seealso{\code{\link{Community}}}

\examples{
data(TL84)

# Remove node 56 (Umbra limi)
a <- RemoveNodes(TL84, 56)
b <- RemoveNodes(TL84, 'Umbra limi')

# Titles differ
a
b

# Results in an error
\dontrun{RemoveNodes(TL84, 1:NumberOfNodes(TL84))}
}

\keyword{utilities}
