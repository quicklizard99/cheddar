\name{NvMLinearRegressions}
\alias{NvMLinearRegressions}
\alias{NvMSlope}
\title{NvMLinearRegressions}
\description{Returns a list of linear regressions fitted to log10-transformed 
    numerical abundance versus body mass.}

\usage{NvMLinearRegressions(community, class)
NvMSlope(community)
}

\arguments{
  \item{community}{an object of class \code{Community}.}
  \item{class}{the property over which linear regressions are fitted. Defaults 
  to 'category' if the community has a node property with that name.}
}

\details{\code{NvMLinearRegressions} returns a \code{list} of \code{lm} 
objects. \code{NvMSlope} returns the slope of a regression through all data.
}

\value{A list of \code{lm} objects named by unique values of \code{class} or 
a single number.}

\author{Lawrence Hudson}

\seealso{\code{\link{Community}}, 
         \code{\link{ApplyByClass}}, 
         \code{\link{NvMLinearRegressions}}}

\examples{
data(TL84)

models <- NvMLinearRegressions(TL84)

# 'all', 'producer', 'invertebrate', 'vert.ecto'
names(models)

# Extract slopes and intercepts
sapply(models, coef)
}
\keyword{utilities}

