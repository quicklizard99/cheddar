\name{LumpNodes}
\alias{LumpNodes}
\title{Lump nodes}
\description{A function that lumps together nodes in a \code{Community}.}
\usage{LumpNodes(community, 
       lump, 
       lumped.names = NULL, 
       title = NULL, 
       class.behaviour = list(integer = MeanNaRm, 
                              numeric = MeanNaRm, 
                              character = JoinUnique, 
                              logical = JoinUnique), 
       column.behaviour = NULL)}

\arguments{
  \item{community}{an object of class \code{Community}}
  \item{lump}{a vector of number of length \code{NumberOfNodes}. Nodes with  
    the same number are lumped together. Values should be continuous, starting 
    at 1.}
  \item{lumped.names}{a vector of names of the lumped nodes. If \code{NULL}, 
    names default to `Lumped node' suffixed by numbers in \code{lump}.}
  \item{title}{the title of the new \code{Community}.}
  \item{class.behaviour}{the names of functions that are used to aggregate 
    values.}
  \item{column.behaviour}{the names of functions that are used to aggregate 
    values.}
}

\value{A new object of class \code{Community}.}

\author{Lawrence Hudson}

\seealso{\code{\link{MergeNodes}}}

\examples{
}
\keyword{utilities}

