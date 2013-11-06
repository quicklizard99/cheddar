\name{SiteBySpeciesMatrix}
\alias{SiteBySpeciesMatrix}
\title{Community collection site by species matrix}
\description{Returns a matrix with a column per community and a row per unique 
node within communities in the collection.}
\usage{SiteBySpeciesMatrix(collection, abundance=NULL, na.missing=FALSE)}

\arguments{
  \item{collection}{an object of class \code{CommunityCollection}.}
  \item{abundance}{the name of a node property that provides abundance values. 
    This can be the name of a first-class propery or the name of a function. 
    The name must meet the criteria of the \code{properties} parameter of 
    \code{NPS}.}
  \item{na.missing}{if \code{TRUE} nodes that are absent from the community 
    will be represented by \code{NA}; if \code{FALSE}, these nodes are 
    represented by 0.}
}

\details{If \code{abundance} is \code{NULL}, the returned \code{matrix} 
indicates presence (1) or absence (0 or \code{NA} - see \code{na.missing}) 
of nodes. If \code{abundance} is given, values are the abundances of nodes,  
or 0 or \code{NA} where nodes are absent.
}

\value{A \code{matrix}.}

\author{Lawrence Hudson}

\seealso{\code{\link{CommunityCollection}}, \code{\link{NPS}}, 
         \code{\link{CollectionCPS}}, \code{\link{Biomass}}, 
         \code{\link{Log10Biomass}}, \code{\link{matrix}}}

\examples{
data(pHWebs)

# If abundance is NULL, you get a presence/absence matrix:
SiteBySpeciesMatrix(pHWebs)

# Numerical abundance
SiteBySpeciesMatrix(pHWebs, 'N')

# Biomass abundance
SiteBySpeciesMatrix(pHWebs, 'Biomass')

# Log10 biomass abundance
SiteBySpeciesMatrix(pHWebs, 'Log10Biomass')

# Example showing how to model biomass in term of pH using vegan's rda function
m <- SiteBySpeciesMatrix(pHWebs, 'Biomass')

# Some nodes (e.g. CPOM) do not have a biomass. The rows in m for these nodes 
# will contain all NA. Rows containing all NA will upset vegan's rda function 
# so these rows must be removed.
m <- m[apply(m, 1, function(row) all(!is.na(row))),]

# Tranpose to get row per site - the format required by vegan's rda function
m <- t(m)

# Matrix (with a row per site) of variables on the right hand side of the 
# model equation
variables <- CollectionCPS(pHWebs)

\dontrun{library(vegan)}
\dontrun{res <- rda(m~pH,variables)}
}
\keyword{utilities}
