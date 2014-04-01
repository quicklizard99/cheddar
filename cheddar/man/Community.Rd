\name{Community}
\alias{Community}
\alias{is.Community}
\alias{plot.Community}
\alias{print.Community}
\alias{summary.Community}
\alias{$<-.Community}
\alias{[<-.Community}
\alias{[[<-.Community}
\alias{dim<-.Community}
\alias{length<-.Community}
\alias{levels<-.Community}
\alias{names<-.Community}
\title{Community}
\description{Creates and returns a new object that represents an ecological 
community.}

\usage{
Community(nodes, properties, trophic.links = NULL)
\method{print}{Community}(x, \dots) 
\method{plot}{Community}(x, \dots) 
\method{summary}{Community}(object, \dots) 
}

\arguments{
  \item{nodes}{a \code{data.frame} containing one row per node. A column called 
    `node' is mandatory and must contain node names. An error is raised if any 
    node names are duplicated. Whitespace is stripped from the beginning and 
    end of node names. If provided, columns called `M' and/or `N' must 
    represent mean body mass and mean numerical abundance respectively. All 
    values in `M' and `N' must be either NA or greater than 0 and less than 
    infinity. If the columns `M' and/or `N' are in nodes, values named 
    `M.units' and/or `N.units' must be provided in \code{properties}.}
  \item{properties}{a list of properties the community as a whole. All elements 
    must be named and must be of length one.}
  \item{trophic.links}{\code{NULL} or a \code{data.frame} or \code{matrix} of 
    trophic link properties. If not \code{NULL}, columns called `resource' and 
    `consumer' must be given and these should contain node names. An error is 
    raised if any names in resource or consumer are not in \code{nodes$node}. 
    Whitespace is stripped from the beginning and end of all names in 
    `resource' and `consumer'. Other columns are taken to be properties of 
    links. An error is raised if any links appear more than once.}
  \item{x}{an object of class \code{Community}.}
  \item{object}{an object of class \code{Community}.}
  \item{\dots}{further arguments passed to other methods.}
}

\details{The most convenient way to import community data in to Cheddar is 
to put data in to CSV files and use the \code{LoadCommunity} function.

Many of Cheddar's plot and analysis functions make use of the `category' node 
property by default, following previously-used metabolic groupings 
(Yodzis and Innes, 1992). The column nodes$category is optional but, if given, 
it should contain one of `producer', `invertebrate', `vert.ecto', `vert.endo' 
or should be an empty string.

\code{Community} supports standard generic functions \code{plot}, 
\code{print}, and \code{summary}.
}

\value{A new object of class \code{Community}.}

\references{Yodzis, P. and Innes, S. (1992) Body size and 
resource-consumer dynamics. The American Naturalist \bold{139}, 1151--1175.}

\author{Lawrence Hudson}

\seealso{\code{\link{CPS}}, 
         \code{\link{NPS}}, 
         \code{\link{TLPS}}, 
         \code{\link{LoadCommunity}}
         \code{\link{SaveCommunity}}}

\examples{
data(TL84)
TL84

# Node properties
NPS(TL84)

# Trophic-link properties
TLPS(TL84)

# Eyeball the data
plot(TL84)

# A different plot function
PlotWebByLevel(TL84)

# Construct a new community.
# TL84.new is an exact copy of TL84
TL84.new <- Community(properties=CPS(TL84), 
                      nodes=NPS(TL84),
                      trophic.links=TLPS(TL84))
identical(TL84, TL84.new)

# A copy of TL84 without trophic links
TL84.no.links <- Community(properties=CPS(TL84), 
                           nodes=NPS(TL84))
NumberOfTrophicLinks(TL84.no.links)

# A community with 10 species and no properties
test <- Community(nodes=data.frame(node=paste('Species', 1:10)), 
                  properties=list(title='Test community'))
test
NPS(test)
}
\keyword{utilities}
