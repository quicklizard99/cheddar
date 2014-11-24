# Artificial communities using structural models

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


RandomLinks <- function(pool, n, C=0.15, nodes)
{
    # A list of n webs with C*S*S randomly assigned links
    # pool - node names that contains unique node names
    # n - number of webs
    # C - directed connectance
    # nodes - data.frame with a column 'node' that contains unique node names
    if(missing(pool)) pool <- nodes$node
    stopifnot(0<C && C<1)
    stopifnot(0<length(pool))
    stopifnot(length(pool)==length(unique(pool)))
    stopifnot(0<n)

    S <- length(pool)
    # sample.int rounds size down, so a size of 258.999999 would result in 258 
    # values. Round to size to nearest int to avoid this.
    size <- round(C*S*S, 0)
    possible <- data.frame(resource=rep(pool, each=S), consumer=pool, 
                           stringsAsFactors=FALSE)
    rows <- replicate(n, sample.int(S*S, size), simplify=FALSE)
    return (lapply(rows, function(r) possible[r,]))
}

CascadeModelLinks <- function(pool, n, C=0.15, nodes)
{
    # Cascade model of Cohen, J. E., and C. M. Newman. 1985. A stochastic 
    # theory of community food webs. I. Models and aggregated data. Proceedings 
    # of the Royal Society of London Series B 224:421–448.
    if(missing(pool)) pool <- nodes$node
    stopifnot(0<C && C<1)
    stopifnot(0<length(pool))
    stopifnot(length(pool)==length(unique(pool)))
    stopifnot(0<n)
    S <- length(pool)

    possible <- data.frame(resource=pool, consumer=rep(pool, each=S), 
                           stringsAsFactors=FALSE)

    # A matrix of S x S  - TRUE if that link is possible
    p <- matrix(FALSE, ncol=S,  nrow=S)

    # Only links in the upper triangle are possible
    p[upper.tri(p)] <- TRUE

    link.possible <- which(p)

    # The number of possible links
    n.possible <- length(link.possible)

    res <- vector("list", n)    # Container for output
    for(index in 1:n)
    {
        pm <- p
        pm[link.possible] <- 2*C*S/(S-1)>runif(n.possible)
        if(any(pm[link.possible]>0))
        {
            res[[index]] <- possible[which(pm>0),]
        }
        else
        {
            # No links - nothing to do
        }
    }
    return (res)
}

.NicheModelLinks1 <- function(pool, n, C=0.15, niche.positions=NULL, 
                              probabilistic=FALSE, nodes)
{
    # First idea - generate values per web

    # Niche model of Williams, R.J. and Martinez, N.D. (2000) Nature 404, 6447, 
    # 180--183.
    # Probabilistic niche model of Williams, R.J. and Anandanadesan, A. and 
    # Purves, D.W. (2010) PLoS One 5, 8, e12092.

    # Returns a list of n webs generated using the niche model
    # pool - node names
    # n - number of webs
    # C - directed connectance
    # niche.positions - either NULL or length(pool) real numbers between 0 and 1
    if(missing(pool)) pool <- nodes$node
    stopifnot(0<C && C<0.5)
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

        if(any(pm))
        {
            res[[index]] <- possible[which(pm),]
        }
    }

    return (res)
}

.NicheModelLinks2 <- function(pool, n, C=0.15, niche.positions=NULL, 
                              probabilistic=FALSE, nodes)
{
    # Second idea - generate all values up front

    # Niche model of Williams, R.J. and Martinez, N.D. (2000) Nature 404, 6447, 
    # 180--183.
    # Probabilistic niche model of Williams, R.J. and Anandanadesan, A. and 
    # Purves, D.W. (2010) PLoS One 5, 8, e12092.

    # Returns a list of n webs generated using the niche model
    # pool - node names
    # n - number of webs
    # C - directed connectance
    # niche.positions - either NULL or length(pool) real numbers between 0 and 1

    # TODO optional colname in nodes from which to compute niche centres?
    # TODO optional colname in nodes from which to compute niche ranges?

    if(missing(pool)) pool <- nodes$node
    stopifnot(0<C && C<0.5)
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

    return (lapply(1:n, fn))
}

NicheModelLinks <- .NicheModelLinks2

CommunityFactory <- function(S, nodes, generator=NicheModelLinks, n=1, 
                             accept=NULL, energetically.feasible=TRUE, 
                             trace.progress=FALSE, validate=TRUE,
                             properties=NULL, ...)
{
    # Returns a collection of artificially generated communities. 
    # Either S or nodes should be provided - no need to provide both. 
    # S - number of nodes in the generated communities
    # nodes - either node names or a data.frame of node properties that 
    # satisfies the conditions nodes of Community.

    # generator - function used to generate sets of trophic links
    # n - number of communities to generate
    # accept - either NULL or a function that takes a community as its only 
    # argument and returns a logical
    # energetically.feasible - if TRUE, communities that are not energetically 
    # feasible according to PreyAveragedTrophicLevel are discarded
    # trace.progress - if TRUE, the feedback is printed
    # ... - other arguments to generator

    if(missing(nodes))
    {
        nodes <- paste('Node', 1:S)
    }

    if(is.character(nodes))
    {
        nodes <- data.frame(node=nodes, row.names=nodes, stringsAsFactors=FALSE)
    }

    if(missing(S))    S <- nrow(nodes)

    stopifnot(0<S)
    stopifnot(0<n)
    stopifnot(nrow(nodes)==S)

    if(trace.progress)
    {
        tracefn <- cat
    }
    else
    {
        tracefn <- function(...) {}
    }

    if('category' %in% colnames(nodes))
    {
        tracefn('Removing node category')
        nodes$category <- NULL
    }

    # The properties used by each generated community
    if(!is.null(properties) && 'title' %in% names(properties))
    {
        properties$title <- NULL
    }
    properties <- c(properties, list(title='Artificial community'))

    # TODO Faster to use a fixed-length list?
    #      All elements initially NULL
    #      Set infeasable and unacceptable communities to NULL.
    #      Loop while any(sapply(communities, is.null))
    communities <- NULL
    while(length(communities)<n)
    {
        # Produce communities
        tracefn(paste('Generating', n-length(communities), 'communities\n'))
        args <- c(list(nodes=nodes, n=n-length(communities)), list(...))
        new <- lapply(do.call(generator, args), function(links)
        {
            if(validate)
            {
                # Validate generated values
                return (Community(nodes=nodes, trophic.links=links,
                                  properties=properties))
            }
            else
            {
                # Avoid the time-consuming validation carried out by Community
                community <- list(nodes=nodes, trophic.links=links, 
                                  properties=properties)
                class(community) <- c('Community', 'list')
                return (community)
            }
        })

        if(energetically.feasible)
        {
            acceptable <- sapply(new, function(community)
            {
                !(all(is.na(PreyAveragedTrophicLevel(community))))
            })

            if(any(!acceptable))
            {
                new <- new[acceptable]
                tracefn('Removing', sum(!acceptable), 'communities that are',
                        'not energetically feasible.', length(new),
                        'communities remain\n')
            }
        }

        if(!is.null(accept) && length(new)>0)
        {
            acceptable <- sapply(new, accept)
            if(any(!acceptable))
            {
                new <- new[acceptable]
                tracefn('Removing', sum(!acceptable), 'communities that are',
                        'not acceptable.', length(new), 'communities remain\n')
            }
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
