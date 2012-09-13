\name{NvMLinearRegressions}
\alias{NvMLinearRegressions}
\title{NvMLinearRegressions}
\alias{NvMSlope}
\alias{NvMIntercept}
\alias{NvMSlopeAndIntercept}
\alias{NvMSlopeByClass}
\alias{NvMInterceptByClass}
\alias{NvMSlopeAndInterceptByClass}
\description{Creation and analysis of linear regressions fitted to log10-
  transformed numerical abundance versus log10-transformed body mass.}

\usage{
NvMLinearRegressions(community, class)

NvMSlope(community)
NvMIntercept(community)
NvMSlopeAndIntercept(community)

NvMSlopeByClass(community, class)
NvMInterceptByClass(community, class)
NvMSlopeAndInterceptByClass(community, class)
}

\arguments{
  \item{community}{an object of class \code{Community}.}
  \item{class}{the property over which linear regressions are fitted. Defaults 
  to 'category' if the community has a node property with that name.}
}

\value{\code{NvMLinearRegressions} returns a \code{list} of \code{lm} 
objects, one for each \code{class} and one fitted to all data. 
The list will contain \code{NULL} if it is not possible to fit a 
linear regression to that class; this will happen for classes that contain 
just a single node or that contain all or all but one nodes where \code{X} 
and/or \code{Y} is \code{NA}.

\code{NvMSlope}, \code{NvMIntercept} and \code{NvMSlopeAndIntercept} return 
the slope, intercept and both, respectively, of a single linear regression 
fitted to all data. 
\code{NvMSlopeByClass}, \code{NvMInterceptByClass} and 
\code{NvMSlopeAndInterceptByClass} return the slope, intercept and both, 
respectively, of linear regressions fitteed to each \code{class} and one 
to all data. For classes where  it is not possible to fit a linear 
regression (for the  reasons given above), \code{NvMSlopeByClass}, 
\code{NvMInterceptByClass} and \code{NvMSlopeAndInterceptByClass} will 
return \code{NA}.
}

\author{Lawrence Hudson}

\seealso{\code{\link{Community}}, 
         \code{\link{CommunityCollection}}
         \code{\link{ApplyByClass}}}

\examples{
data(TL84)

models <- NvMLinearRegressions(TL84)

# 'all', 'producer', 'invertebrate', 'vert.ecto'
names(models)

# Extract slopes and intercepts
sapply(models, coef)


# Slopes and intercepts through all data for each web in the pHWebs 
# collection
data(pHWebs)
CollectionCPS(pHWebs, properties=c('NvMSlope'))
CollectionCPS(pHWebs, properties=c('NvMIntercept'))
CollectionCPS(pHWebs, properties=c('NvMSlopeAndIntercept'))

# Slopes and intercepts through each category for each web in pHWebs
CollectionCPS(pHWebs, properties=c('NvMSlopeAndInterceptByClass'))
}
\keyword{utilities}

