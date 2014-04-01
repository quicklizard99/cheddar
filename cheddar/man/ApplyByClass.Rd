\name{ApplyByClass}
\alias{ApplyByClass}
\alias{SumBiomassByClass}
\alias{SumMByClass}
\alias{SumNByClass}
\title{Apply by class}
\description{Apply functions to a group of values given by a node property.}

\usage{
ApplyByClass(community, property, class, fn, \dots)
SumMByClass(community, class, na.rm=FALSE)
SumNByClass(community, class, na.rm=FALSE)
SumBiomassByClass(community, class, na.rm=FALSE)
}

\arguments{
  \item{community}{an object of class \code{Community}.}
  \item{property}{the property to which \code{fn} is applied.}
  \item{class}{the property over which \code{fn} is applied. Defaults to 
  'category' if the community has a node property with that name.}
  \item{fn}{a function.}
  \item{na.rm}{logical - if \code{TRUE} then \code{NA} values are removed.}
  \item{\dots}{Other parameters to \code{fn}.}
}

\details{\code{ApplyByClass} applies \code{fn} to \code{property} by class. 
\code{property} and \code{class} should both be names that meet the criteria 
of the \code{properties} argument of \code{NPS}.

\code{SumMByClass}, \code{SumNByClass} and \code{SumBiomassByClass} are 
convenient wrapper around \code{ApplyByClass}.
}

\value{A vector or list of values, named by unique values of \code{class}.}

\author{Lawrence Hudson}

\seealso{\code{\link{Community}}, 
         \code{\link{NPS}}}

\examples{
data(TL84)

# Sum body mass by category
ApplyByClass(TL84, 'M', 'category', sum)

# A more convenient way to sum body mass by category
SumMByClass(TL84)

# Sum body mass by kingdom. The 'Unclassified flagellates' node does not have a 
# kingdom, so we get a value labelled '<unnamed>'.
SumMByClass(TL84, 'kingdom')

# Maximum body mass by category
ApplyByClass(TL84, 'M', 'category', max)

# A list of min and max M
ApplyByClass(TL84, 'M', 'category', range)

# A list of min and max M by kingom
ApplyByClass(TL84, 'M', 'kingdom', range)

# The same values as a matrix
do.call('rbind', ApplyByClass(TL84, 'M', 'kingdom', range))

# Broadstone Stream has some nodes in every category without M so all returned 
# values are NA.
data(BroadstoneStream)
SumMByClass(BroadstoneStream)

# Get rid of the NA values
SumMByClass(BroadstoneStream, na.rm=TRUE)
}
\keyword{utilities}

