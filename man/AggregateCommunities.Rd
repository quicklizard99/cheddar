\name{AggregateCommunities}
\alias{AggregateCommunities}
\alias{AggregateCommunitiesBy}
\title{Aggregate communities}

\description{Functions that aggregate communities in a collection.}

\usage{
AggregateCommunities(collection, 
                     aggregate = names(collection), 
                     weight.by='N',
                     title = NULL)

AggregateCommunitiesBy(collection, aggregate.by, \dots)
}

\arguments{
  \item{collection}{an object of class \code{CommunityCollection}.}
  \item{aggregate}{the names of the communities to aggregate.}
  \item{weight.by}{the name of a column by which to compute weighted mean of 
    numeric values.}
  \item{title}{the title of the new \code{Community}.}
  \item{aggregate.by}{the name of a community property, either first-class or
    computed, over which to aggregate.}
  \item{\dots}{values passed to \code{AggregateCommunities}.}
}

\details{\code{AggregateCommunities} combines communities given in 
\code{aggregate} in to a single new \code{Community}. 

Nodes that appear in one or more of the communities in \code{aggregate} 
are combined into a single node in the returned community. 
The way in which numeric node properties are aggregated is governed by the 
\code{weight.by} parameter. 
If \code{weight.by} is \code{NULL} or is not the name of a node property, 
the arithmetic mean is computed for each numeric node property. 
If \code{weight.by} is the name of a node property, that property is used to 
compute weighted means of the other numeric node properties; the 
arithmetic mean of \code{weight.by} is computed. This scheme means that 
if a community contains both N and M, aggregation using 
\code{weight.by='N'} results in the arithmetic mean of N and the 
N-weighted mean of M for each node.
Node properties that are characters or logicals are aggregated by joining 
unique values with a `,'. Empty character strings are ignored. 
Species that are not present in some communities in the collection are 
assumed to have a value of \code{0} for all numeric node properties, 
an empty string (\code{''}) for all character node properties and 
a value of \code{NA} for all logical node properties.

The returned community contains the union of trophic links for each node. 
Community properties are aggregated by computing the arithmetic mean of 
numeric values and joining unique character and logical values with a `,'.

See the `Aggregating communities' section of the `Collections' vignette for 
a more detailed explanation and examples of how properties are aggregated.

\code{AggregateCommunitiesBy} aggregates by a property of the communities, 
either first-class or computed. If there is more than one unique value of the 
property across the contained communities, a new \code{CommunityCollection} 
object is returned. If there is just one unique value, a single 
\code{Community} is returned.
}

\value{A new object that is either of class \code{Community} or 
\code{CommunityCollection}.}

\author{Lawrence Hudson}

\seealso{\code{\link{CommunityCollection}}, 
         \code{\link{CollectionCPS}}}

\examples{
data(pHWebs)

# An aggregate of 3 communities
AggregateCommunities(pHWebs, c('Old Lodge', 'Afon Hafren', 'Broadstone'))

# The Duddon Pike Beck and Mosedal Beck communities share the same 
# latitude and have pH values of 6.1 and 5.9 respectively.
CollectionCPS(pHWebs[c('Duddon Pike Beck', 'Mosedal Beck')])

# Aggregating by the 'lat' property therefore results in a new collection 
# of nine communities.
CollectionCPS(AggregateCommunitiesBy(pHWebs, 'lat'))

# Would produce an error
\dontrun{AggregateCommunities(pHWebs, c('not a community', 'Afon Hafren'))}
}

\keyword{utilities}
