\name{LoadCommunity}
\alias{LoadCommunity}
\alias{SaveCommunity}
\title{Loading and saving Community objects}
\description{
\code{LoadCommunity} and \code{SaveCommunity} are functions for loading and 
saving \code{Community} objects to CSV files.}
\usage{
LoadCommunity(dir, fn='read.csv', \dots)
SaveCommunity(community, dir, fn='write.csv', na='', \dots)}

\arguments{
  \item{community}{an object of class \code{Community}.}
  \item{dir}{a directory.}
  \item{fn}{the name of an R function that loads/saves CSV files.}
  \item{na}{the string to use for missing values in the data; see 
  \code{write.csv}.}
  \item{\dots}{other values to \code{fn}.}
}

\details{Data are stored in CSV (Comma-Separated Value) files in \code{dir}. 
Properties of any aspect of the community (nodes, links or the whole community) 
can be added simply by adding columns to the relevant CSV file. 
The data-quality checks defined by \code{Community} are applied by 
\code{LoadCommunity}. The \code{fn} and \code{dots} arguments can be used to 
read/write files in a range of formats.

\code{properties.csv} defines items applicable to the community as a 
whole, such as sampling date, lat & long or altitude and environmental 
variables such as temperature or pH. This file must contain a column called 
`title'.

\code{nodes.csv} should contain the list of nodes and together with any 
associated properties such as mean body mass, mean numerical abundance and 
classification. This file must contain a column called `node' that must contain 
node names. Many of Cheddar's plot and analysis functions make use of the 
`category' node property by default, following previously-used metabolic 
groupings (Yodzis & Innes, 1992). The `category' column of \code{nodes.csv} 
is optional but, if given, it should contain one of `producer', 
`invertebrate', `vert.ecto', `vert.endo' or should be an empty string.

\code{trophic.links.csv} is optional. It defines trophic links in columns 
`resource' and `consumer', which should be names of nodes. Properties 
of trophic links such as evidence for the presence of the link (e.g. 
empirically observed or inferred from literature) can be added to this file.
}

\value{\code{LoadCommunity} returns a new \code{Community}.}

\references{Yodzis, P. and Innes, S. (1992) Body size and 
resource-consumer dynamics. The American Naturalist \bold{139}, 1151--1175.}

\author{Lawrence Hudson}

\seealso{\code{\link{Community}},
         \code{\link{read.csv}},
         \code{\link{write.csv}}}

\examples{
data(TL84)
temp.path <- tempfile()
SaveCommunity(TL84, temp.path)
TL84.loaded <- LoadCommunity(temp.path)
unlink(temp.path, recursive=TRUE)
identical(TL84, TL84.loaded)    # TRUE
}
\keyword{utilities}
