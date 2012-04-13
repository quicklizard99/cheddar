\name{NPS}
\alias{NPS}
\title{Node properties}
\description{Returns a \code{data.frame} of first-class and computed node 
properties.}
\usage{NPS(community, properties = NULL)}

\arguments{
  \item{community}{an object of class \code{Community}.}
  \item{properties}{the names of node properties. These can be names of 
first-class properties (returned by \code{NodePropertyNames}) and names of 
functions that take a \code{Community} object as the only parameter and return 
either a vector of length \code{NumberOfNodes} or a \code{matrix} or 
\code{data.frame} with \code{NumberOfNodes} rows.}
}

\details{This function is named NPS for Node Properties. It returns a 
\code{data.frame} containing the column `node' and any requested properties. 
If \code{properties} is \code{NULL}, all first-class node properties are 
included in the returned \code{data.frame}.
}

\value{A \code{data.frame} with \code{NumberOfNodes} rows.}

\author{Lawrence Hudson}

\seealso{\code{\link{NP}}, \code{\link{NumberOfNodes}}}

\examples{
data(TL84) 
NPS(TL84) 

NPS(TL84, 'M') 

# Biomass is a function
NPS(TL84, 'Biomass') 

NPS(TL84, c(B='Biomass'))

# Several first-class and computed properties
NPS(TL84, c('M', 'N', B='Biomass', 'TrophicSpecies', 
            TL='PreyAveragedTrophicLevel'))

# Pass parameters to functions
NPS(TL84, list(TS1='TrophicSpecies', 
               TS2=list('TrophicSpecies', include.isolated=FALSE), 
               Iso='IsIsolatedNode'))
}
\keyword{utilities}
