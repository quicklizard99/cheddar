# Artificial communities using structural models

# TODO Generalize NicheModelLinks to cater for cascade etc?

# TODO Allow generator function to return node, links and/or community 
# properties? This would allow:
#   * A generator that just produced M and/or N
#   * NicheModelLinks (and other structural models) to return optionally return 
#   node properties n,c,r

# TODO ComunityFactory to take node, link and community properties?

# TODO ComunityFactory to take functions to generate node and link properties?

# TODO Links generators to return information about generated communities, e.g. 
#      niche positions, centres and ranges.

# TODO niche.positions should be sorted?

# TODO Implement other bits of niche model?
# "Occasionally, model-generated webs contain completely disconnected species 
# or trophically identical species. Such species are eliminated and replaced
# until the web is free of such species. The species with the smallest ni has 
# ri = 0 so that every web has at least one basal species.

# TODO A function that returns a list of artificial communities for each 
# community in a collection - CollectionLike?

# TODO More structural models
# Cascade
# Cohen, J. E., and C. M. Newman. 1985. A stochastic theory of community food 
# webs. I. Models and aggregated data. Proceedings of the Royal Society of 
# London Series B 224:421–448.

# Generalized cascade model
# Stouffer DB, Camacho J, Guimera` R, Ng CA, Amaral LAN (2005) Ecology 
# 86:1301–1311.

# Generalized niche model
# Stouffer, D. B., J. Camacho, and L. A. N. Amaral. 2006. A robust measure of 
# food web intervality. Proceedings of the National Academy of Science USA 
# 103:19015–19020.

# Nested-hierarchy model
# Cattin M-F, Bersier L-F, Banasˇek-Richter C, Baltensperger R, Gabriel J-P 
# (2004) Nature 427:835–839

# Minimum potential niche model (Allesina et al. 2008)
# Allesina, S., D. Alonso, and M. Pascual. 2008. A general model for food web 
# structure. Science 320:658–661.

# Relaxed niche model (Williams and Martinez 2008)
# Williams, R. J., and N. D. Martinez. 2008. Success and its limits among 
# structural models of complex food webs. Journal of Animal Ecology 77:512–519.

# ADBM
# Petchey, O. L., A. P. Beckerman, J. O. Riede, and P. H. Warren. 2008. Size, 
# foraging, and food web structure. Proceedings of the National Academy of 
# Science USA 105:4191–4196


RandomLinks <- function(pool, n, C=0.15)
{
    # Pool - node names
    # n - number of webs
    # C - directed connectance
    # A list of n webs with C*S*S randomly assigned links.
    stopifnot(0<C && C<1)
    stopifnot(0<length(pool))
    stopifnot(length(pool)==length(unique(pool)))
    stopifnot(0<n)

    S <- length(pool)
    # sample.int rounds size down, so a size of 258.999999 would result in 258 
    # values. Round to size to nearest int to avoid this.
    size <- round(C*S*S, 0)
    rows <- replicate(n, sample.int(S*S, size), simplify=FALSE)
    possible <- data.frame(resource=rep(pool, each=S), consumer=pool, 
                           stringsAsFactors=FALSE)
    return (lapply(rows, function(r) possible[r,]))
}

NicheModelLinks1 <- function(pool, n, C=0.15, niche.positions=NULL, 
                            probabilistic=FALSE)
{
    # First idea - generate values per web

    # Returns a list of n webs generated using the niche model
    # pool - node names
    # n - number of webs
    # C - directed connectance
    # niche.positions - either NULL or length(pool) real numbers between 0 and 1
    stopifnot(0<C && C<1)
    stopifnot(0<length(pool))
    stopifnot(length(pool)==length(unique(pool)))
    stopifnot(0<n)
    stopifnot(is.null(niche.positions) || 
              (length(niche.positions)==length(pool) && 
               !any(is.na(niche.positions)) &&
               all(0<=niche.positions & niche.positions<=1)))
    S <- length(pool)

    possible <- data.frame(resource=pool, consumer=rep(pool, each=S), 
                           stringsAsFactors=FALSE)

    res <- vector("list", n)    # Container for output
    for(index in 1:n)
    {
        # A new set of niche positions for each community, if not provided
        if(is.null(niche.positions)) niche.positions <- sort(runif(S))

        # Feeding ranges - beta distribution with α=1 and β=1/(2*C)-1
        r <- niche.positions * rbeta(S, 1, 1/(2*C)-1)

        # Set range of species with the smallest niche position to 0 to ensure 
        # that there is at least one basal species
        r[which.min(niche.positions)] <- 0

        # Diet positions - can't go above niche position or greater than 1
        c <- runif(S, min=r/2, max=pmin(niche.positions, 1-r/2))

        # If this fails, some logic is incorrect somewhere
        #stopifnot(all(c<=niche.positions))

        # Niche positions repeated along rows
        if(probabilistic)
        {
            pm <- exp(-sweep(outer(niche.positions, c, '-'), MARGIN=2, r/2, "/")^2)>runif(S*S)
        }
        else
        {
            # Compare niche positions to upper and lower bounds feeding ranges
            pm <- outer(niche.positions, c-r/2, '>=') & 
                  outer(niche.positions, c+r/2, '<=')
        }

        res[[index]] <- possible[which(pm),]
    }

    return (res)
}

NicheModelLinks2 <- function(pool, n, C=0.15, niche.positions=NULL, 
                            probabilistic=FALSE)
{
    # Second idea - generate all values up front

    # Returns a list of n webs generated using the niche model
    # pool - node names
    # n - number of webs
    # C - directed connectance
    # niche.positions - either NULL or length(pool) real numbers between 0 and 1
    stopifnot(0<C && C<1)
    stopifnot(0<length(pool))
    stopifnot(length(pool)==length(unique(pool)))
    stopifnot(0<n)
    stopifnot(is.null(niche.positions) || 
              (length(niche.positions)==length(pool) && 
               !any(is.na(niche.positions)) &&
               all(0<=niche.positions & niche.positions<=1)))
    S <- length(pool)

    possible <- data.frame(resource=pool, consumer=rep(pool, each=S), 
                           stringsAsFactors=FALSE)

    if(is.null(niche.positions))
    {
        # A new set of niche positions for each community, if not provided - a 
        # matrix of n rows and S cols.
        niche.positions <- t(replicate(n, sort(runif(S))))
        smallest.niche.index <- 1
    }
    else
    {
        smallest.niche.index <- which.min(niche.positions)
        niche.positions <- matrix(niche.positions, ncol=S, nrow=n, byrow=TRUE)
    }

    # Feeding ranges - a matrix of n rows and S cols.
    r <- niche.positions * matrix(rbeta(n*S, 1, 1/(2*C)-1), ncol=S)

    # Set range of species with the smallest niche position to 0 to ensure 
    # that there is at least one basal species. This is the first column.
    r[,smallest.niche.index] <- 0

    if(probabilistic)
    {
        probabilistic.runif <- matrix(runif(n*S*S), ncol=S)
    }

    # Diet positions - a matrix of n rows and S cols. Prevent range from 
    # being greater than niche position or greater than 1.
    c <- matrix(runif(n*S, min=r/2, max=pmin(niche.positions, 1-r/2)), ncol=S)

    if(probabilistic)
    {
        fn <- function(index)
        {
            pm <- exp(-sweep(outer(niche.positions[index,], c[index,], '-'), 2, r[index,]/2, "/")^2)>probabilistic.runif[index,]
            return (possible[which(pm),])
        }
    }
    else
    {
        fn <- function(index)
        {
            # Compare niche positions to upper and lower bounds feeding ranges
            pm <- outer(niche.positions[index,], c[index,]-r[index,]/2, '>=') & 
                  outer(niche.positions[index,], c[index,]+r[index,]/2, '<=')
            return (possible[which(pm),])
        }
    }

    return (return (lapply(1:n, fn)))
}

NicheModelLinks <- NicheModelLinks2

CommunityFactory <- function(S, node=paste('Node', 1:S), 
                             generator=NicheModelLinks, n=1, accept=NULL, 
                             energetically.feasible=TRUE, 
                             trace.progress=FALSE, 
                             trusted=TRUE, ...)
{
    # Returns a collection of artificially generated communities
    # S - number of nodes in the pool
    # node - node names; if given must be of length S
    # generator - function used to generate sets of trophic links
    # n - number of communities to generate
    # accept - either NULL or a function that takes a community as its only 
    # argument and returns a bool
    # energetically.feasible - if TRUE, communities that are not energetically 
    # feasible according to PreyAveragedTrophicLevel are discarded
    # trace.progress - if TRUE, the feedback is printed
    # ... - other arguments to generator

    stopifnot(0<S)
    stopifnot(0<n)
    stopifnot(length(node)==S)

    if(trace.progress)
    {
        tracefn <- cat
    }
    else
    {
        tracefn <- function(...) {}
    }

    # The nodes and properties used by each generated community
    nodes <- data.frame(node=node, row.names=node, stringsAsFactors=FALSE)
    properties <- list(title='Artificial community')

    communities <- NULL
    while(length(communities)<n)
    {
        # Produce communities
        tracefn(paste('Generating', n-length(communities), 'communities\n'))
        args <- c(list(pool=node, n=n-length(communities)), list(...))
        new <- lapply(do.call(generator, args), function(links)
        {
            if(trusted)
            {
                # Avoid the time-consuming validation carried out by Community
                community <- list(nodes=nodes, trophic.links=links, 
                                  properties=properties)
                class(community) <- c('Community', 'list')
                return (community)
            }
            else
            {
                # Validate generated values
                return (Community(nodes=nodes, trophic.links=links, 
                                  properties=properties))
            }
        })

        if(energetically.feasible)
        {
            tracefn('Removing communities that are not energetically feasible\n')
            acceptable <- sapply(new, function(community)
            {
                !(all(is.na(PreyAveragedTrophicLevel(community))))
            })
            new <- new[acceptable]
            tracefn(paste(length(new), 'communities remain\n'))
        }

        if(!is.null(accept) && length(new)>0)
        {
            tracefn('Removing communities that are not acceptable\n')
            acceptable <- sapply(new, accept)
            new <- new[acceptable]
            tracefn(paste(length(new), 'communities remain\n'))
        }

        if(length(new)>0)
        {
            communities <- c(communities, new)
        }
    }

    # Assign sensible titles
    communities <- mapply(title=paste('Artificial community', 1:n), 
                          community=communities, 
                          SIMPLIFY=FALSE, 
                          FUN=function(title, community)
    {
        # Community objects cannot be modified. Get rid of the Community class 
        # so that the title can be assigned.
        class(community) <- 'list'
        community$properties$title <- title
        class(community) <- c('Community', 'list')
        return (community)
    })

    return (CommunityCollection(communities))
}

CommunitiesLike <- function(community, ...)
{
    if(!is.Community(community)) stop('Not a Community')
    if(is.null(TLPS(community))) stop('The community has no trophic links')
    return (CommunityFactory(S=NumberOfNodes(community), 
                             C=DirectedConnectance(community), 
                             node=unname(NP(community, 'node')), 
                             ...))
}
