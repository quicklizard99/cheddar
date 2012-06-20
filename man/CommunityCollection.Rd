\name{CommunityCollection}
\alias{CommunityCollection}
\alias{is.CommunityCollection}
\alias{plot.CommunityCollection}
\alias{print.CommunityCollection}
\alias{summary.CommunityCollection}
\alias{$<-.CommunityCollection}
\alias{[<-.CommunityCollection}
\alias{[[<-.CommunityCollection}
\alias{dim<-.CommunityCollection}
\alias{length<-.CommunityCollection}
\alias{levels<-.CommunityCollection}
\alias{names<-.CommunityCollection}
\alias{[.CommunityCollection}
\title{Collections of communities}

\description{Collections of communities}

\usage{CommunityCollection(communities)
\method{print}{CommunityCollection}(x, \dots) 
\method{plot}{CommunityCollection}(x, ncol=min(length(x),5), by.col=TRUE, 
     plot.fn=plot, \dots)
\method{summary}{CommunityCollection}(object, \dots) 
}

\arguments{
  \item{communities}{a \code{list} of \code{Community} objects.}
  \item{x}{an object of class \code{CommunityCollection}.}
  \item{object}{an object of class \code{CommunityCollection}.}
  \item{ncol}{the number of columns in the plot.}
  \item{by.col}{logical - if \code{TRUE} communities are plotted along columns.}
  \item{plot.fn}{a plot function that accepts a \code{Community} obejct .}
  \item{\dots}{further arguments passed to other methods.}
}

\details{Constructs a new \code{CommunityCollection} from a list of existing 
\code{Community} objects. \code{CommunityCollection} is a subclass of 
\code{list}. \code{CommunityCollection} objects can not be modified directly.

An error is raised if any \code{Community} objects in \code{communities} share 
the same `title' community property.
An error is also raised if the \code{Community} objects in \code{communities} 
do not all have the same value of the community properties `M.units' and 
`N.units'.
\code{CommunityCollection} places no restrictions on other properties. 
For example, all of the ten communities with the \code{\link{pHWebs}} 
collection has a valid pH property but this is not enforced by 
\code{CommunityCollection} - it would be possible for a \code{Community} 
within a collection to not have a pH property, to have a pH of \code{NA} or 
even to have an invalid pH, for example a negative value. 

\code{Community} supports standard generic functions \code{plot}, \code{print}, 
\code{subset} and \code{summary}.
}

\value{A new object of class \code{CommunityCollection}.}
\author{Lawrence Hudson}

\seealso{\code{\link{Community}}, 
         \code{\link{CollectionCPS}}
         \code{\link{CollectionNPS}}
         \code{\link{CollectionTLPS}}, 
         \code{\link{OrderCollection}}, 
         \code{\link{subset.CommunityCollection}}, 
         \code{\link{AggregateCommunitiesBy}},
         \code{\link{AggregateCommunities}}, 
         \code{\link{pHWebs}}}

\examples{
# 10 stream webs sampled over a wide pH gradient
data(pHWebs)
pHWebs


# Eyeball the webs
plot(pHWebs)

# Consistent axis limits
plot(pHWebs, xlim=c(-14,6), ylim=c(-3,13))

# Different plot function
plot(pHWebs, plot.fn=PlotWebByLevel, ylim=c(1,4.5))


# list-like operations
length(pHWebs)
sapply(pHWebs, 'NumberOfTrophicLinks')
pHWebs[['Broadstone']]  # Access the Community

# A new CommunityCollection containing every other ph web
pHWebs[seq(1, 10, by=2)]

# A new CommunityCollection containing two webs
pHWebs[c('Old Lodge','Bere Stream')]


# CollectionCPS gets community properties
CollectionCPS(pHWebs)   # Webs are sorted by increasing pH

# Order by decreasing pH
pHWebs.decreasing.pH <- OrderCollection(pHWebs, 'pH', decreasing=TRUE)
CollectionCPS(pHWebs.decreasing.pH)

# Order by name
pHWebs.name <- OrderCollection(pHWebs, 'title')
CollectionCPS(pHWebs.name, c('pH', 'NumberOfNodes'))

# The following will always be TRUE.
all(FALSE==duplicated(names(pHWebs)))


# A new collection of the two Tuesday Lake communities
data(TL84, TL86)
BothTL <- CommunityCollection(list(TL84, TL86))
CollectionCPS(BothTL)


# You can't modify CommunityCollections
\dontrun{pHWebs[1] <- 'silly'}
}

\keyword{utilities}
