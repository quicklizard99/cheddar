\name{AggregateCommunities}
\alias{AggregateCommunities}
\alias{AggregateCommunitiesBy}
\title{Aggregate communities}

\description{Functions that aggregate communities in a collection.}

\usage{
AggregateCommunities(collection, 
                     aggregate = names(collection), 
                     class.behaviour = list(integer = MeanNaRm, 
                                            numeric = MeanNaRm, 
                                            character = JoinUnique, 
                                            logical = JoinUnique), 
                     column.behaviour = NULL, 
                     title = NULL)

AggregateCommunitiesBy(collection, aggregate.by, \dots)
}

\arguments{
  \item{collection}{an object of class \code{CommunityCollection}.}
  \item{aggregate}{the names of the communities to aggregate.}
  \item{class.behaviour}{a named list of functions that are used to aggregate 
    values. Names should be the names of R classes (integer, character etc). 
    Functions should accept a single vector of values and should return a 
    single value.}
  \item{column.behaviour}{either NULL or a named list of functions giving 
    column-specific behaviour.}
  \item{title}{the title of the new \code{Community}.}
  \item{aggregate.by}{the name of a community property, either first-class or
    computed, over which to aggregate.}
  \item{\dots}{values passed to \code{AggregateCommunities}.}
}

\details{\code{AggregateCommunities} combines communities given in 
\code{aggregate} into a single new \code{Community}. 

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
         \code{\link{AggregateHelpers}}, 
         \code{\link{CollectionCPS}}}

\examples{
data(pHWebs)

# An aggregate of 3 communities
AggregateCommunities(pHWebs, c('Old Lodge', 'Afon Hafren', 'Broadstone'))

# Use median body mass rather than mean body mass
AggregateCommunities(pHWebs, c('Old Lodge', 'Afon Hafren', 'Broadstone'), 
                     column.behaviour=list(M=median))

# An error
\dontrun{AggregateCommunities(pHWebs, c('not a community', 'Afon Hafren'))}

# The Duddon Pike Beck and Mosedal Beck communities share the same 
# latitude and have pH values of 6.1 and 5.9 respectively.
CollectionCPS(pHWebs[c('Duddon Pike Beck', 'Mosedal Beck')])

# Aggregating by the 'lat' property therefore results in a new collection 
# of nine communities.
CollectionCPS(AggregateCommunitiesBy(pHWebs, 'lat'))
}

\keyword{utilities}
