\name{PlotHelpers}
\alias{Log10BLabel}
\alias{Log10MLabel}
\alias{Log10NLabel}
\alias{DefaultCategoryColours}
\alias{DefaultCategoryLabelColours}
\alias{DefaultCategorySymbols}
\alias{DefaultLinkColour}
\alias{LMabline}
\alias{PlotLinearModels}
\alias{FormatLM}
\alias{PlaceMissingPoints}
\alias{PlaceMissingPoints}
\title{Plot helpers}
\description{Functions that are useful for customising plots and for creating 
your own plot functions.}

\usage{
Log10BLabel(community, name = "italic(B)", units = with(CPS(community), paste(M.units, "~", N.units)))
Log10MLabel(community, name = "italic(M)", units = CPS(community)$M.units)
Log10NLabel(community, name = "italic(N)", units = CPS(community)$N.units)

DefaultCategoryColours()
DefaultCategoryLabelColours()
DefaultCategorySymbols()
DefaultLinkColour()

PlaceMissingPoints(x, xlim, y, ylim)

LMabline(model, ...)
PlotLinearModels(models, colour.spec, col = NULL, ...)
FormatLM(model, slope.95.ci = FALSE, ci.plus.minus.style = FALSE, 
         r = FALSE, r.squared = TRUE, model.var.names = TRUE) 
}

\arguments{
  \item{community}{an object of class \code{Community}.}
  \item{name}{the name that appears in the label.}
  \item{units}{the units that appears in the label.}
  \item{x}{x values.}
  \item{y}{y values.}
  \item{xlim}{limits of the x axis.} 
  \item{ylim}{limits of the y axis.} 
  \item{models}{a \code{list} of \code{lm} objects that are plotted.}
  \item{colour.spec}{either \code{NULL} or a named vector that maps values 
    of \code{colour.by} to plotting values; defaults to the vector returned 
    by \code{DefaultCategoryColours}.}
  \item{col}{plot colours.}
  \item{model}{an \code{lm} object for which a textual description is 
    assembled.}
  \item{slope.95.ci}{logical - if \code{TRUE} then the 95\% confidence 
    intervals are included in the description.}
  \item{ci.plus.minus.style}{logical - if \code{TRUE} then the 95\% confidence 
    intervals are shown by a `plus-minus' sign. If \code{FALSE} then the 
    confidence intervals are shown by an upper and lower bound.}
  \item{r}{logical - if \code{TRUE} the `r' is included in the description.}
  \item{r.squared}{ - if \code{TRUE} the `r squared' is included in the 
    description.}
  \item{model.var.names}{logical - if \code{TRUE} then the names of the 
    dependent and independent variables fitted in the model are included 
    in the description. If \code{FALSE}, the names `x' and `y' are used.}
  \item{\dots}{other values passed to plotting functions.}
}

\author{Lawrence Hudson}

\seealso{\code{\link{Community}}, 
         \code{\link{DefaultCategoryColours}},
         \code{\link{NvMLinearRegressions}}, 
         \code{\link{LinearRegressionByClass}}, 
         \code{\link{lm}}}

\keyword{hplot}

