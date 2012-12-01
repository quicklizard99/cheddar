\name{LoadCollection}
\alias{LoadCollection}
\alias{SaveCollection}
\title{Loading and saving CommunityCollection objects}
\description{
\code{LoadCollection} and \code{SaveCollection} are functions 
for loading and saving code{CommunityCollection} objects to text files.
}
\usage{
LoadCollection(dir, \dots)
SaveCollection(collection, dir, \dots)}

\arguments{
  \item{collection}{an object of class \code{CommunityCollection}.}
  \item{dir}{a directory.}
  \item{\dots}{other values to \code{LoadCommunity} or \code{SaveCommunity}.}
}

\details{The \code{Community} objects in \code{collection} are saved to a 
directory named \code{communities} inside \code{dir}. The order of the 
collection is not saved. Any existing data in \code{dir} is ignored.}

\value{\code{LoadCollection} returns a new \code{CommunityCollection}.}

\author{Lawrence Hudson}

\seealso{\code{\link{CommunityCollection}}, 
         \code{\link{OrderCollection}}, 
         \code{\link{LoadCommunity}},
         \code{\link{SaveCommunity}}}

\examples{
data(pHWebs)
temp.path <- tempfile()
SaveCollection(pHWebs, temp.path)
pHWebs.loaded <- LoadCollection(temp.path)
pHWebs.loaded <- OrderCollection(pHWebs.loaded, 'pH')
unlink(temp.path, recursive=TRUE)
identical(pHWebs, pHWebs.loaded)    # TRUE
}
\keyword{utilities}
