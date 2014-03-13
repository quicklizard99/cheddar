\name{Node connectivity}
\alias{IsBasalNode}
\alias{IsTopLevelNode}
\alias{IsIntermediateNode}
\alias{IsIsolatedNode}
\alias{IsConnectedNode}
\alias{IsNonBasalNode}
\alias{IsNonTopLevelNode}
\alias{BasalNodes}
\alias{TopLevelNodes}
\alias{IntermediateNodes}
\alias{IsolatedNodes}
\alias{ConnectedNodes}
\alias{NonBasalNodes}
\alias{NonTopLevelNodes}
\alias{FractionBasalNodes}
\alias{FractionIntermediateNodes}
\alias{FractionTopLevelNodes}
\alias{FractionIsolatedNodes}
\alias{FractionNonConnectedNodes}
\alias{FractionNonBasalNodes}
\alias{FractionConnectedNodes}
\alias{FractionNonTopLevelNodes}
\title{Node connectivity}
\description{Functions that report the connectivity of nodes in a food web.}

\usage{
IsBasalNode(community)
IsTopLevelNode(community) 
IsIntermediateNode(community) 
IsIsolatedNode(community) 
IsConnectedNode(community) 
IsNonBasalNode(community) 
IsNonTopLevelNode(community) 

BasalNodes(community)
TopLevelNodes(community) 
IntermediateNodes(community) 
IsolatedNodes(community) 
ConnectedNodes(community)
NonTopLevelNodes(community) 
NonBasalNodes(community)

FractionBasalNodes(community)
FractionIntermediateNodes(community)
FractionTopLevelNodes(community)
FractionIsolatedNodes(community)
FractionNonBasalNodes(community)
FractionConnectedNodes(community)
FractionNonTopLevelNodes(community)
}

\arguments{
  \item{community}{an object of class \code{Community}.}
}

\details{Each node in a community is defined as: 
\tabular{ll}{
isolated      \tab No resources or consumers, other than possibly itself \cr
basal         \tab No resources and one or more consumers \cr
top-level     \tab One or more resources and no consumers, other than possibly 
                   itself \cr
intermediate  \tab Nodes not fitting any of the above categories
}

These definitions allow the following additional definitions:
\tabular{ll}{
connected     \tab basal, intermediate or top-level \cr
non-basal     \tab isolated, intermediate or top-level \cr
non-top-level \tab isolated, basal or intermediate
}

For each of the above seven definitions, `X', there are three functions: 
\code{IsX}, \code{X} and \code{FractionX}. 
The first returns a vector of \code{logical} of length \code{NumberOfNodes}; 
values are \code{TRUE} for nodes that fit the definition of `X'.
The second returns the names of nodes for which \code{IsX} returns \code{TRUE}. 
The third returns the proportion of nodes in the community that fit the 
definition of `X'.
}

\value{Either a \code{logical} vector of length \code{NumberOfNodes} or a 
vector of names.}

\author{Lawrence Hudson}

\seealso{\code{\link{NumberOfNodes}}, 
         \code{\link{Cannibals}}, 
         \code{\link{IsCannibal}}, 
         \code{\link{NumberOfTrophicLinks}}, 
         \code{\link{PredationMatrix}}, 
         \code{\link{Degree}}, 
         \code{\link{InDegree}}, 
         \code{\link{OutDegree}}, 
         \code{\link{ResourcesByNode}}, 
         \code{\link{ConsumersByNode}}, 
         \code{\link{ResourcesOfNodes}}, 
         \code{\link{ConsumersOfNodes}}}

\examples{
data(TL84)

# Assemble a table of node connectivity. Only one of each of the following 
# four properties is TRUE for each node.
connectivity <- NPS(TL84, c('IsBasalNode', 'IsIsolatedNode', 
                            'IsIntermediateNode', 'IsTopLevelNode'))
connectivity

# Each row sums to 1, confirming that exactly one of the columns in each row 
# is TRUE.
all(1==rowSums(connectivity))

# These summations are 1
sum(FractionBasalNodes(TL84), 
    FractionIntermediateNodes(TL84), 
    FractionTopLevelNodes(TL84), 
    FractionIsolatedNodes(TL84))

sum(FractionConnectedNodes(TL84), 
    FractionIsolatedNodes(TL84))

sum(FractionBasalNodes(TL84), 
    FractionNonBasalNodes(TL84))

sum(FractionTopLevelNodes(TL84), 
    FractionNonTopLevelNodes(TL84))
}

\keyword{utilities}

