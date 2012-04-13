\name{subset.CommunityCollection}
\alias{subset.CommunityCollection}
\title{A subset of a collection of communities}
\description{A subset of a \code{CommunityCollection}.}

\usage{\method{subset}{CommunityCollection}(x, subset, properties=NULL, \dots)}
\arguments{
  \item{x}{An object of class \code{CommunityCollection}}
  \item{subset}{logical expression indicating communities to keep.}
  \item{properties}{The names of properties passed to \code{CollectionCPS}.}
  \item{\dots}{further arguments passed to other methods.}
}

\details{\code{CollectionCPS} is used to gather \code{properties}. 
\code{properties} should contain the names of properties required to evaluate 
\code{subset}. If \code{properties} is \code{NULL}, all first-class properties 
are available to the \code{subset} expression. Returns a new 
\code{CommunityCollection} or \code{NULL} if no communities in \code{x} meet 
the criteria in \code{subset}.}

\value{A new object of class \code{CommunityCollection} or \code{NULL}.}

\author{Lawrence Hudson}
\seealso{\code{\link{CommunityCollection}}, \code{\link{CollectionCPS}}, 
         \code{\link{subset}}}

\examples{
data(pHWebs)

# Two communities have pH>7
subset(pHWebs, pH>7)

# No communities have pH>10 so this returns NULL
subset(pHWebs, pH>7)

# Get a subset based on a computed property
subset(pHWebs, S>50, properties=c(S='NumberOfNodes'))

# X is not a property so this raises an error
\dontrun{subset(pHWebs, X==1)}
}
\keyword{utilities}
