\name{OrderCommunity}
\alias{OrderCommunity}
\title{Order a community}
\description{Order a \code{Community}.}
\usage{OrderCommunity(community, \dots, decreasing=FALSE, na.last = TRUE, 
                      new.order=NULL, title=NULL)}

\arguments{
  \item{community}{an object of class \code{Community}.}
  \item{\dots}{the names of properties by which to order the communities.}
  \item{decreasing}{logical.}
  \item{na.last}{logical.}
  \item{new.order}{a vector of either node integer indices or node names 
    giving the order.}
  \item{title}{the title of the new \code{Community}.}
}

\details{Returns a new \code{Community} object. \code{dots} can contain 
any name that meets the criteria of the \code{properties} parameter of 
\code{NPS}. If \code{new.order} is \code{NULL} then \code{\dots} and optionally 
\code{decreasing} are used to compute the new node ordering. 

Different node orders will yield different \code{SumDietGaps} and 
\code{SumConsumerGaps} (e.g.\ Stouffer et al 2006, Zook et al 2011).
}

\value{A \code{Community}.}

\references{Stouffer, D.B. and Camacho, J. and Amaral, L.A.N. (2006) 
Proceedings of the National Academy of Sciences of the United States of 
America \bold{103}, 50, 19015--19020.

Zook, A.E. and Eklof, A. and Jacob, U. and Allesina, S. (2011) 
Journal of Theoretical Biology \bold{271}, 1 106--113.
}

\author{Lawrence Hudson}

\seealso{\code{\link{Community}}, 
         \code{\link{order}}, 
         \code{\link{Intervality}}, 
         \code{\link{CollectionNPS}}, 
         \code{\link{PreyAveragedTrophicLevel}},
         \code{\link{PlotPredationMatrix}}}

\examples{
data(TL84)
NPS(TL84)

# Order by increasing M
TL84.increasing.M <- OrderCommunity(TL84, 'M', title='Increasing M')
NPS(TL84.increasing.M)

# Order by decreasing M
TL84.decreasing.M <- OrderCommunity(TL84, 'M', decreasing=TRUE)
NPS(TL84.decreasing.M)

# Order by increasing M and N
TL84.increasing.MN <- OrderCommunity(TL84, 'M', 'N')
NPS(TL84.increasing.MN)

# Reverse existing order
TL84.reversed <- OrderCommunity(TL84, new.order=56:1)
NPS(TL84.reversed)

# Sort alphabetically by category and by increasing M within each category
TL84.category <- OrderCommunity(TL84, 'category', 'M')

# Increasing trophic level, then randomly sorted within trophic level
new.order <- order(PreyAveragedTrophicLevel(TL84), sample(1:56))
TL84.increasing.TL <- OrderCommunity(TL84, new.order=new.order, 
                                     title='Increasing TL')
NPS(TL84.increasing.TL)

# Graphically show the effect of different orders
par(mfrow=c(1,2))
PlotPredationMatrix(TL84.increasing.M)
PlotPredationMatrix(TL84.increasing.TL)
}

\keyword{utilities}

