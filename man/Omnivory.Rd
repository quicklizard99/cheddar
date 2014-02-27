\name{Omnivory}
\alias{IsOmnivore}
\alias{Omnivores}
\alias{FractionOmnivorous}
\alias{Omnivory}
\title{Omnivory}
\description{Nodes that consume themselves in the food web.}

\usage{
IsOmnivore(community, level=PreyAveragedTrophicLevel)
Omnivores(community, \dots)
FractionOmnivorous(community, \dots)
Omnivory(community, \dots)
}

\arguments{
  \item{community}{an object of class \code{Community}.}
  \item{level}{a function that returns the trophic level of each node in 
   \code{community}.}
  \item{\dots}{other values to \code{IsOmnivore}.}
}

\details{Omnivores are those nodes that consume two or more species and have a 
non-integer trophic level (Polis 1991). \code{IsOmnivore} returns a vector of 
\code{logical} of length \code{NumberOfNodes}; values are \code{TRUE} for 
nodes that are omnivorous. \code{Omnivores} returns the names of nodes for 
which \code{IsOmnivore} returns \code{TRUE}. \code{FractionOmnivorous} and 
\code{Omnivory} both return the proportion of nodes for which \code{IsOmnivore} 
returns \code{TRUE}. 

Many researchers have used chain-averaged trophic level when computing omnivory 
(Polis, 1991; Bersier et al 2002). Computing chain-averaged trophic level
requires enumerating every unique food chain - this can be lengthy for complex 
food webs so the default function used by \code{IsOmnivore} is 
\code{PreyAveragedTrophicLevel}. Omnivory values obtained using these two 
methods might differ slightly.
}

\value{Either a \code{logical} vector of length \code{NumberOfNodes} or a 
vector of names.
}

\references{Polis, G. A. (1991) Complex desert food webs: an empirical critique 
of food web theory. American Naturalist \code{138}, 123--155.

Bersier, L. and Banasek-Richter, C. and Cattin, M. (2002) Ecology 
\bold{80} 2394--2407.
}

\author{Lawrence Hudson}

\seealso{\code{\link{NumberOfNodes}}, \code{\link{PreyAveragedTrophicLevel}}, 
         \code{\link{ChainAveragedTrophicLevel}}}

\examples{
data(TL84)

IsOmnivore(TL84)
Omnivores(TL84)
Omnivory(TL84)

# Omnivory values found using PreyAveragedTrophicLevel and 
# ChainAveragedTrophicLevel differ for ChesapeakeBay
data(ChesapeakeBay)
Omnivory(ChesapeakeBay)
Omnivory(ChesapeakeBay, level=ChainAveragedTrophicLevel)
}

\keyword{utilities}
