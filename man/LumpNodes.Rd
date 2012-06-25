\name{LumpNodes}
\alias{LumpNodes}
\title{Lump nodes}
\description{A function that lumps together nodes in a \code{Community}.}
\usage{
LumpNodes(community, 
          lump, 
          title = NULL, 
          weight.by = 'N')}

\arguments{
  \item{community}{an object of class \code{Community}}
  \item{lump}{a vector of of length \code{NumberOfNodes} containing names of 
    lumped nodes. Nodes with the same value of \code{lump} will be merged.}
  \item{title}{the title of the new \code{Community}.}
  \item{weight.by}{the name of a column by which to take weighted mean of 
    numeric values.}
}

\value{A new object of class \code{Community}.}

\author{Lawrence Hudson}

\seealso{\code{\link{LumpTrophicSpecies}}, 
         \code{\link{IsIsolatedNode}}, \code{\link{IsolatedNodes}}}

\examples{
data(TL84)

# Lump together isolated nodes in TL84
length(which(IsIsolatedNode(TL84)))  # 6 isolated species
IsolatedNodes(TL84)                  # Names of isolated nodes

lump <- NP(TL84, 'node')             # Existing node names

# Give isolated nodes the same lump value
lump[IsolatedNodes(TL84)] <- 'Isolated nodes lumped together'
TL84.lumped <- LumpNodes(TL84, lump)

NumberOfNodes(TL84)         # 56 nodes in unlumped web
NumberOfNodes(TL84.lumped)  # 51 nodes in lumped web

IsolatedNodes(TL84.lumped)  # A single node


# This trivial example shows that no nodes are lumped if values in lump are 
# unique to each node
lump <- NP(TL84, 'node')
identical(TL84, LumpNodes(TL84, lump, title=CP(TL84, 'title')))


# Ythan Estuary contains two species that are split in to adult and 
# juvenile forms. The example below lumps these in to single species.
data(YthanEstuary)

# The names of nodes in YthanEstuary
lump <- NP(YthanEstuary, 'node')

# European flounder:
# "Platichthys flesus" and "Platichthys flesus (juvenile)"
# Lump these in to one node
lump["Platichthys flesus (juvenile)"==lump] <- "Platichthys flesus"

# Common eider:
# "Somateria mollissima" and "Somateria mollissima (juvenile)"
# Lump these in to one node
lump["Somateria mollissima (juvenile)"==lump] <- "Somateria mollissima"
YthanEstuary.lumped <- LumpNodes(YthanEstuary, lump)

# Plot the original community and the community with lumped nodes
par(mfrow=c(1,2))
plot(YthanEstuary, highlight.nodes=c("Platichthys flesus", 
                                     "Platichthys flesus (juvenile)", 
                                     "Somateria mollissima", 
                                     "Somateria mollissima (juvenile)"))
plot(YthanEstuary.lumped, highlight.nodes=c("Platichthys flesus", 
                                            "Somateria mollissima"))
}
\keyword{utilities}

