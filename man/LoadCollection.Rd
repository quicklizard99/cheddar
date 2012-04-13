\name{LoadCollection}
\alias{LoadCollection}
\alias{SaveCollection}
\title{Loading and saving CommunityCollection objects}
\description{
\code{LoadCollection} and \code{SaveCollection} are functions 
for loading and saving code{CommunityCollection} objects to text files.
}
\usage{
LoadCollection(dir)
SaveCollection(collection, dir)}

\arguments{
  \item{collection}{an object of class \code{CommunityCollection}.}
  \item{dir}{a directory.}
}

\details{The \code{Community} objects in \code{collection} are saved to a 
directory named \code{communities} inside \code{dir}. The order of the 
collection is not saved. Any existing data in \code{dir} is ignored.}

\value{\code{LoadCollection} returns a new \code{CommunityCollection}.}

\author{Lawrence Hudson}

\seealso{\code{\link{CommunityCollection}}, 
         \code{\link{OrderCollection}}}

\examples{
data(pHWebs)
SaveCollection(pHWebs, '~/pHWebs')
pHWebs.loaded <- LoadCollection('~/pHWebs')
pHWebs.loaded <- OrderCollection(pHWebs.loaded, 'pH')
identical(pHWebs, pHWebs.loaded)    # TRUE
}
\keyword{utilities}

