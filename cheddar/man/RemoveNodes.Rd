\name{RemoveNodes}
\alias{RemoveNodes}
\title{Remove nodes}
\description{Remove one or more nodes.}
\usage{RemoveNodes(community, remove, title, 
                   method=c('direct','secondary','cascade'))}

\arguments{
  \item{community}{an object of class \code{Community}.}
  \item{remove}{a vector of either names, integer indices or logicals 
    indicating nodes to be removed.}
  \item{title}{a title for the new community.}
  \item{method}{how species removals are propogated through the food web.}
}
\details{Returns a new \code{Community} with nodes in \code{remove} removed. 
An error is raised if \code{remove} refers to nodes not in the 
\code{community} of if \code{remove} refers to all nodes in the 
\code{community}. 

If \code{method} is `direct', only the nodes in \code{remove} are removed. 
If \code{method} is `secondary', secondarily extinct nodes - those that 
directly consume one or more nodes in `remove' and that no longer have any 
resources (except themselves) after the removal - are also removed. 
If \code{method} is `cascade', a multistep version of `secondary' is applied. 
This has the effect of propogating extinctions though the community - all 
consumers that are ultimately dependent upon all species in `remove', and upon 
no other nodes (except themselves), will be removed.
}

\value{A new object of class \code{Community}.}
\author{Lawrence Hudson}

\seealso{\code{\link{Community}}, \code{\link{BasalNodes}}, 
         \code{\link{IsolatedNodes}}, \code{\link{NumberOfNodes}}}

\examples{
data(TL84)

# Three different ways of removing node 56 (Umbra limi)
a <- RemoveNodes(TL84, 56)
b <- RemoveNodes(TL84, 'Umbra limi')
c <- RemoveNodes(TL84, c(rep(FALSE,55), TRUE))

identical(a,b)  # TRUE
identical(a,c)  # TRUE

# The behaviours of the different methods
NumberOfNodes(TL84)         # 56 nodes in total
length(BasalNodes(TL84))    # 25 basal nodes
length(IsolatedNodes(TL84)) #  6 isolated nodes

RemoveNodes(TL84, BasalNodes(TL84)) # 56 - 25 = 31 nodes remain
RemoveNodes(TL84, BasalNodes(TL84), method='secondary') # 14 nodes remain
RemoveNodes(TL84, BasalNodes(TL84), method='cascade')   # 6 isolated nodes remain

# Results in an error
\dontrun{RemoveNodes(TL84, 1:NumberOfNodes(TL84))}
}

\keyword{utilities}
