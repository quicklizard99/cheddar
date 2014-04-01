\name{ResourcesByNode}
\alias{ResourcesByNode}
\alias{ConsumersByNode}
\alias{ResourcesOfNodes}
\alias{ConsumersOfNodes}
\alias{ResourcesAndConsumersByNode}
\alias{TrophicLinksForNodes}
\title{Resources and consumers of nodes}
\description{Functions that return the resources and consumers of nodes.}

\usage{
ResourcesByNode(community) 
ConsumersByNode(community) 
ResourcesAndConsumersByNode(community)

ResourcesOfNodes(community, nodes) 
ConsumersOfNodes(community, nodes) 

TrophicLinksForNodes(community, nodes, node.properties=NULL, 
                     link.properties=NULL) 
}

\arguments{
  \item{community}{an object of class \code{Community}.}
  \item{nodes}{either the names or integer indices of nodes.}
  \item{node.properties}{passed to \code{TLPS}.}
  \item{link.properties}{passed to \code{TLPS}.}
}

\details{\code{ResourcesByNode}/\code{ConsumersByNode}/
\code{ResourcesAndConsumersByNode} all return a list of length 
\code{NumberOfNodes}; list elements are names of nodes that are 
resources/consumers/resources and/or consumers.

If \code{nodes} is of length one then \code{ResourcesOfNodes} and 
\code{ConsumersOfNodes} return a vector of resources / consumers. If 
\code{nodes} contains more than one value, then a list of vectors is returned. 

\code{TrophicLinksForNodes} returns a \code{data.frame} containing the columns 
`resource' and 'consumer' and a row for each trophic link in-to and out-of 
\code{nodes}.
}

\value{Either a \code{vector}, a \code{list} or a \code{data.frame}}

\author{Lawrence Hudson}

\seealso{\code{\link{TLPS}}, \code{\link{PredationMatrix}}, 
         \code{\link{NumberOfNodes}}}

\examples{
data(TL84)

# A list containing a vector of resources for each node.
ResourcesByNode(TL84)

# A vector of resources of 'Umbra limi'
ResourcesOfNodes(TL84, 'Umbra limi')

# A vector of resources of 'Umbra limi'
ResourcesOfNodes(TL84, 56)

# A list containing vectors of resources for nodes 50:56
ResourcesOfNodes(TL84, 50:56)

# A data.frame containin columns resource and consumer
TrophicLinksForNodes(TL84, 'Umbra limi')

# A data.frame containin columns resource, consumer, resource.M and consumer.M
TrophicLinksForNodes(TL84, 'Umbra limi', node.properties='M')
}
\keyword{utilities}

