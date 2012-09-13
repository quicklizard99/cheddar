\name{LinearRegressionByClass}
\alias{LinearRegressionByClass}
\title{Linear regression by class}
\description{Fit linear regressions to node data by class.}
\usage{LinearRegressionByClass(community, X, Y, class)}

\arguments{
  \item{community}{an object of class \code{Community}.}
  \item{X}{Independent variable. A property name that must meet the criteria of 
           the \code{properties} parameter of \code{NPS}.}
  \item{Y}{Dependent variable. A property name that must meet the criteria of 
           the \code{properties} parameter of \code{NPS}.}
  \item{class}{The property over which linear regressions are fitted.}
}

\details{A linear model is fitted through all data points and through each 
subset of the data given by \code{class}. A list of \code{lm} objects is 
returned. The list will contain \code{NULL} if it is not possible to fit a 
linear regression to that class; this will happen for classes that contain 
just a single node or that contain all or all but one nodes where \code{X} 
and/or \code{Y} is \code{NA}.
}

\value{A \code{list} of \code{lm} objects.}

\author{Lawrence Hudson}

\seealso{\code{\link{Community}}, 
         \code{\link{ApplyByClass}}, 
         \code{\link{NPS}}, 
         \code{\link{NvMLinearRegressions}}, 
         \code{\link{lm}}}

\examples{
data(TL84)

# Regressions fitted to log10(Biomass) versus log10(M) data.
models <- LinearRegressionByClass(TL84, 'Log10M', 'Log10Biomass', 
                                  'category')

# 'all', 'producer', 'invertebrate', 'vert.ecto'
names(models)

# Extract slopes and intercepts
sapply(models, coef)
}
\keyword{utilities}

