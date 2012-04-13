\name{MergeNodes}
\alias{MergeNodes}
\title{Merge nodes}
\description{A function that merges together nodes in a \code{Community}.}
\usage{MergeNodes(community, merge, merged.name, title = NULL, ...)}

\arguments{
  \item{community}{an object of class \code{Community}.}
  \item{merge}{the names of nodes to merge.}
  \item{merged.name}{the name of the new node.}
  \item{title}{the title of the new \code{Community}.}
  \item{\dots}{other parameters passed to \code{LumpNodes}.}
}

\details{Nodes given in \code{merge} are lumped together in to a new node 
named \code{merged.name}.}

\value{A new object of class \code{Community}.}

\author{Lawrence Hudson}

\seealso{\code{\link{LumpNodes}}}
\examples{
data(YthanEstuary)

# Ythan Estuary contains two species that are split into adult and 
# juvenile forms. 
# Merge these into single nodes.

# Merge the two European flounder nodes
to.merge1 <- c("Platichthys flesus", "Platichthys flesus (juvenile)")
merged1 <-     "Platichthys flesus"
YE.merged <- MergeNodes(YthanEstuary, to.merge1, merged1)

# Merge the two Common Eider nodes
to.merge2 <- c("Somateria mollissima", "Somateria mollissima (juvenile)")
merged2 <-     "Somateria mollissima"
YE.merged <- MergeNodes(YE.merged, to.merge2, merged2)

# Plot merged and merged
par(mfrow=c(1,2))
plot(YthanEstuary, highlight.nodes=c(to.merge1,to.merge2))
plot(YE.merged, highlight.nodes=c(merged1,merged2), main=YthanEstuary$title)
}
\keyword{utilities}

