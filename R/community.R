# An ecological community
is.Community <- function(x)
{
    return (inherits(x, 'Community'))
}

"[<-.Community" <- function(x, i, value)
{
    if(!is.Community(x)) stop('Not a Community')
    stop("Can't assign to a Community")
}

"[[<-.Community" <- function(x, i, value)
{
    if(!is.Community(x)) stop('Not a Community')
    stop("Can't assign to a Community")
}

'$<-.Community' <- function(object, x, value)
{
    if(!is.Community(object)) stop('Not a Community')
    stop("Can't assign to a Community")
}

"names<-.Community" <- function(x, value)
{
    if(!is.Community(x)) stop('Not a Community')
    stop("Can't assign to a Community")
}

"length<-.Community" <- function(x, value)
{
    if(!is.Community(x)) stop('Not a Community')
    stop("Can't change length of a Community")
}

"levels<-.Community" <- function(x, value)
{
    if(!is.Community(x)) stop('Not a Community')
    stop("Can't change levels of a Community")
}

"dim<-.Community" <- function(x, value)
{
    if(!is.Community(x)) stop('Not a Community')
    stop("Can't change dim of a Community")
}

Community <- function(nodes, properties, trophic.links=NULL)
{
    # Returns a new community object
    # nodes - data.frame containing one row per node. A column called
    # node is mandatory and must contain node names. 
    # An error is raised if any names are duplicated. 
    # If provided, columns called M and N must represent mean body mass and 
    # mean population density respetively. If M or N columns are in nodes, 
    # the parameters M.units and N.units must be provided in properties.

    # properties - a list of properties the community as a whole. Values must 
    # be named and must be of length one.

    # trophic.links - a data.frame or matrix of trophic link properties. 
    # Columns called resource and consumer are mandatory and should contain 
    # either node numbers or node names. Other columns are taken to be 
    # properties of links.

    CheckDF <- function(param, param.name, illegal.names)
    {
        # Helper that check a data.frame parameter
        if(!class(param) %in% c('matrix', 'data.frame'))
        {
            stop(paste("'", param.name, "' must be a matrix or data.frame", 
                       sep=''))
        }

        if(0==nrow(param))
        {
            stop(paste("'", param.name, "' contains no rows", sep=''))
        }

        if(is.null(colnames(param)))
        {
            stop(paste("'", param.name, "' contains no column names", sep=''))
        }

        if(any(duplicated(colnames(param))))
        {
            bad <- colnames(param)[duplicated(colnames(param))]
            stop(paste('The names [', paste(bad, collapse=','), '] ', 
                       "are duplicated in '", param.name, "'", 
                       sep=''))
        }

        bad <- grep('(resource\\.|consumer\\.).*', colnames(param))
        if(length(bad))
        {
            bad <- colnames(param)[bad]
            stop(paste('The names [', paste(bad, collapse=','), '] ', 
                       "in '", param.name, "' are not allowed. ", 
                       "Names must not start with 'resource.' or ", 
                       "'consumer.'.", 
                       sep=''))
        }

        bad <- colnames(param) %in% illegal.names
        if(any(bad))
        {
            stop(paste("'", param.name, "' must not contain the names [", 
                       paste(colnames(param)[bad], collapse=','), 
                       ']', sep=''))
        }

        return (as.data.frame(param, stringsAsFactors=FALSE, row.names=NULL))
    }

    # Some reserved names
    reserved.node.names <- c('node', 'M', 'N')
    reserved.tl.names <- c('resource', 'consumer')
    reserved.properties.names <- c('title', 'M.units', 'N.units')

    # Check types, lengths and names
    nodes <- CheckDF(nodes, 'nodes', 
                     c(reserved.tl.names, reserved.properties.names))

    if(!is.null(trophic.links))
    {
        if(0!=nrow(trophic.links))
        {
            trophic.links <- CheckDF(trophic.links, 'trophic.links', 
                                     c(reserved.node.names, 
                                       reserved.properties.names))
            rownames(trophic.links) <- NULL
        }
        else
        {
            trophic.links <- NULL
        }
    }

    if(!is.vector(properties) || is.null(names(properties)))
    {
        stop("The 'properties' parameters must be a list with names")
    }

    # Coerce to list
    properties <- as.list(properties)

    # Same basic checks as for nodes and trophic.links
    CheckDF(data.frame(properties, check.names=FALSE), 'properties', 
             c(reserved.tl.names, reserved.node.names))

    # Put title first.
    # WARNING: Must do this after the call to CheckDF() because it has the 
    # effect of removing duplicated 'title'
    properties <- properties[c('title', setdiff(names(properties), 'title'))]

    # All items in properties must be of length 1
    bad.lengths <- 1!=sapply(properties, length)
    if(any(bad.lengths))
    {
        stop(paste('The items [', 
                   paste(names(properties)[bad.lengths], collapse=','), 
                   "] 'properties' are not of length 1", sep=''))
    }

    # Check for names that appear in more than one aspect
    all.names <- c(colnames(nodes), colnames(trophic.links), names(properties))
    if(any(duplicated(all.names)))
    {
        bad <- all.names[duplicated(all.names)]
        stop(paste('The names [', paste(bad, collapse=','), 
                   '] appear in more than one of the parameters ', 
                   "'nodes', 'trophic.links' and 'properties'", 
                   sep=''))
    }

    if(TRUE)
    {
        # Some node checks
        # Node names are mandatory
        if(!'node' %in% colnames(nodes))
        {
            stop("A column called 'node' must be in the nodes data.frame")
        }

        node <- .StripWhitespace(as.character(nodes[,'node']))

        # All nodes must have a name
        if(any(''==node))
        {
            stop('Some node names are empty')
        }

        # Check for case
        check.node <- tolower(node)
        bad.node.names <- duplicated(check.node)
        if(any(bad.node.names))
        {
            stop(paste('The nodes named [', 
                       paste(node[bad.node.names], collapse=','), 
                       '] are duplicated', sep=''))
        }

        # Node names that are numbers are a really bad idea
        nodes.are.integers <- grepl('^\\d+$', node)
        if(any(nodes.are.integers))
        {
            bad <- node[nodes.are.integers]
            stop(paste('The numeric node names [', paste(bad, collapse=','), 
                       '] are not allowed', sep=''))
        }

        # Use node names without whitespace
        nodes$node <- node

        # Put node column first
        nodes <- nodes[,c('node', setdiff(colnames(nodes), 'node')),drop=FALSE]

        # Get rid of factors
        for(n in setdiff(colnames(nodes), 'node'))
        {
            if(is.factor(nodes[,n]))
            {
                nodes[,n] <- as.character(nodes[,n])
            }
        }

        # rowtitles are processed node names
        rownames(nodes) <- nodes$node
    }

    # Validation
    if(is.null(properties$title) || ''==properties$title)
    {
        stop(paste("The 'properties' parameter must contain a character", 
                   "named 'title'"))
    }

    # Check M
    if('M' %in% colnames(nodes))
    {
        M <- nodes[,'M']
        if(!class(M) %in% c('numeric', 'integer'))
        {
            stop('M must contain numbers')
        }

        if(!all( (0<M & M<Inf) | is.na(M)))
        {
            stop('All M values must satisfy the condition 0<M<Inf | is.na(M)')
        }

        if(all(is.na(M)))
        {
            stop('Not all values in M can be NA')
        }

        if(is.null(properties$M.units) || ''==properties$M.units || 
           'character'!=class(properties$M.units))
        {
            stop(paste("The 'properties' parameter must contain an entry", 
                       "named 'M.units'"))
        }
    }

    # Check N
    if('N' %in% colnames(nodes))
    {
        N <- nodes[,'N']
        if(!class(N) %in% c('numeric', 'integer'))
        {
            stop('N must contain numbers')
        }

        if(!all( (0<N & N<Inf) | is.na(N)))
        {
            stop('All N values must satisfy the condition 0<N<Inf | is.na(N)')
        }

        if(all(is.na(N)))
        {
            stop('Not all values in N can be NA')
        }

        if(is.null(properties$N.units) || ''==properties$N.units || 
           'character'!=class(properties$N.units))
        {
            stop(paste("The 'properties' parameter must contain an entry", 
                       "named 'N.units'"))
        }
    }

    # Check trophic.links
    if(!is.null(trophic.links))
    {
        mandatory <- c('resource', 'consumer')
        tl <- trophic.links
        if(!isTRUE(all.equal(mandatory, intersect(mandatory, colnames(tl)))))
        {
            stop(paste('The trophic.links data.frame does not contain columns', 
                       "named 'resource' and 'consumer'"))
        }

        if(0==nrow(tl))
        {
            stop('The trophic.links data.frame does not contain any rows')
        }

        tl[,'resource'] <- .StripWhitespace(tl[,'resource'])
        tl[,'consumer'] <- .StripWhitespace(tl[,'consumer'])

        missing <- setdiff(c(tl[,'resource'],tl[,'consumer']), nodes$node)
        if(0!=length(missing))
        {
            stop(paste('The names [', 
                       paste(missing, collapse=','), 
                       '] in the set of trophic links are not listed in nodes', 
                       sep=''))
        }

        duplicated.rows <- duplicated(tl)
        if(any(duplicated.rows))
        {
            stop(paste(sum(duplicated.rows), ' duplicated trophic links [',
                       paste(tl[duplicated.rows, 'resource'], 
                             tl[duplicated.rows, 'consumer'], 
                             sep=' -> ', collapse=','), ']', sep=''))
        }

        # Put resource and consumer columns first
        tl <- tl[,c('resource', 'consumer', 
                    setdiff(colnames(tl), c('resource', 'consumer'))), 
                    drop=FALSE]

        # Use resource and consumer without whitespace
        trophic.links <- tl
    }

    # Check category
    if('category' %in% colnames(nodes) && !is.null(trophic.links))
    {
        
        # Producers should not be assigned resources
        producers <- nodes[,'category']=='producer'
        producers <- nodes[producers, 'node']
        bad <- is.element(producers, trophic.links[,'consumer'])
        if(any(bad))
        {
            stop(paste('Nodes [', 
                       paste(unique(producers[bad]), collapse=','), 
                       '] have a category of producer but have been ', 
                       "given resources in 'trophic.links'", sep=''))
        }
    }


    self <- list(nodes=nodes, 
                 properties=properties, 
                 trophic.links=trophic.links)
    class(self) <- c('Community', 'list')
    return(self)
}

NodePropertyNames <- function(community)
{
    if(!is.Community(community)) stop('Not a Community')
    return (colnames(community[['nodes']]))
}

NP <- function(community, property)
{
    if(!is.Community(community)) stop('Not a Community')
    # Returns a vector. Returned vector is all NA if there is no node property 
    # with that name.
    stopifnot(1==length(property))
    stopifnot(is.character(property) & property!='')

    np <- NPS(community)

    if(property %in% colnames(np))
    {
        p <- np[,property]
    }
    else
    {
        p <- rep(NA, nrow(np))
    }

    names(p) <- np$node
    return (p)
}

NPS <- function(community, properties=NULL)
{
    if(!is.Community(community)) stop('Not a Community')
    # A data.frame containing node properties
    # properties - a vector of property names. 
    # Elements of properties can be either first-class properties of nodes or 
    # names of functions; functions must take a community as the only 
    # parameter and must return either a vector of length 
    # NumberOfNodes(community) or a matrix/data.frame with 
    # NumberOfNodes(community) rows. 
    # Values for elements thats are neither first-class node properties nor 
    # function names will all be NA.

    res <- community[['nodes']]
    class(res) <- 'data.frame'
    if(!is.null(properties))
    {
        res <- .AssembleProperties(res, properties, community=community)
    }
    return (res)
}

TrophicLinkPropertyNames <- function(community)
{
    if(!is.Community(community)) stop('Not a Community')
    return (colnames(community[['trophic.links']]))
}

TLP <- function(community, property=NULL)
{
    # Returns a vector of length NumberOfTrophicLinks(). Returned vector is 
    # all NA if there is no trophic-link property with that name.
    if(!is.Community(community)) stop('Not a Community')
    stopifnot(1==length(property))
    stopifnot(is.character(property) & property!='')

    tlp <- TLPS(community)

    if(property %in% colnames(tlp))
    {
        p <- tlp[,property]
    }
    else
    {
        p <- rep(NA, nrow(tlp))
    }

    return (p)
}

TLPS <- function(community, node.properties=NULL, link.properties=NULL)
{
    # A data.frame containing trophic links in resource-consumer form and any 
    # associated node.properties and link.properties.
    # If the community has no trophic links returns NULL.
    if(!is.Community(community)) stop('Not a Community')
    tlp <- community[['trophic.links']]
    if(!is.null(tlp))
    {
        class(tlp) <- 'data.frame'
    }

    res <- tlp
    if(!is.null(res))
    {
        if(!is.null(link.properties))
        {
            # Include 'resource' and 'consumer'
            link.properties <- union(c('resource','consumer'), link.properties)
            res <- .AssembleProperties(res, properties=link.properties, 
                                       community=community)
        }

        # Add node properties
        np <- .NodePropertiesOfChains(community, chains=tlp, 
                                      node.properties=node.properties, 
                                      chain.columns=c('resource','consumer'))
        if(!is.null(np))
        {
            res <- cbind(res, np)
        }
    }

    return (res)    
}

CommunityPropertyNames <- function(community)
{
    if(!is.Community(community)) stop('Not a Community')
    return (names(community[['properties']]))
}

CP <- function(community, property)
{
    if(!is.Community(community)) stop('Not a Community')
    stopifnot(1==length(property))
    stopifnot(is.character(property) & property!='')

    return (CPS(community)[[property]])
}

CPS <- function(community, properties=NULL)
{
    if(!is.Community(community)) stop('Not a Community')
    res <- community[['properties']]
    class(res) <- 'list'

    if(!is.null(properties))
    {
        # .AssembleProperties takes and returns a data.frame
        res <- .AssembleProperties(data.frame(res, stringsAsFactors=FALSE), 
                                   properties=properties, 
                                   community=community)
        res <- as.list(res)
    }

    return (res)
}

HasM <- function(community)
{
    return ('M' %in% NodePropertyNames(community))
}

HasN <- function(community)
{
    return ('N' %in% NodePropertyNames(community))
}

HasTrophicLinks <- function(community)
{
    return (!is.null(TLPS(community)))
}

.RequireM <- function(community)
{
    if(!HasM(community))
    {
        stop('This function .Requires body mass (M) data')
    }
}

.RequireN <- function(community)
{
    if(!HasN(community))
    {
        stop('This function .Requires abundance (N) data')
    }
}

.RequireTrophicLinks <- function(community)
{
    if(!HasTrophicLinks(community))
    {
        stop('This function .Requires trophic links')
    }
}

plot.Community <- function(x, ...)
{
    if(!is.Community(x)) stop('Not a Community')
    tl <- !is.null(TLPS(x))
    M <- HasM(x)
    N <- HasN(x)
    if(M && N)
    {
        # If tl is not NULL, the plot will show the food web
        PlotNvM(x, ...)
    }
    else if(tl && M)
    {
        # Predator-prey body mass ratios
        PlotMRvMC(x, ...)
    }
    else if(tl && N)
    {
        # Predator-prey abundance ratios
        PlotNRvNC(x, ...)
    }
    else if(tl)
    {
        PlotWebByLevel(x, ...)
    }
    else if(M)
    {
        PlotMDistribution(x, ...)
    }
    else if(N)
    {
        PlotNDistribution(x, ...)
    }
    else
    {
        stop('The community contains no properties that can be plotted')
    }
}

print.Community <- function(x, ...)
{
    if(!is.Community(x)) stop('Not a Community')
    cat(paste(CPS(x)$title, 'containing', NumberOfNodes(x), 'nodes.\n'))
}

summary.Community <- function(object, ...)
{
    # Returns a vector
    if(!is.Community(object)) stop('Not a Community')

    res <- c(CPS(object), S=NumberOfNodes(object))
    if(HasTrophicLinks(object))
    {
        res <- c(res, 
                 L=NumberOfTrophicLinks(object),
                 'L/S'=LinkageDensity(object), 
                 C=DirectedConnectance(object))
    }

    if(HasM(object))
    {
        summary.M <- summary(NP(object, 'M'))
        names(summary.M) <- paste('M', names(summary.M))
        res <- c(res, summary.M)
    }

    if(HasN(object))
    {
        summary.N <- summary(NP(object, 'N'))
        names(summary.N) <- paste('N', names(summary.N))
        res <- c(res, summary.N)
    }

    if(HasM(object) && HasN(object))
    {
        summary.B <- summary(Biomass(object))
        names(summary.B) <- paste('B', names(summary.B))
        res <- c(res, summary.B)
    }

    return (res)
}

LoadCommunity <- function(dir)
{
    # Loads the community in the given directory and returns an object of 
    # class community
    if(!file.exists(dir) || !file.info(dir)$isdir)
    {
        stop(paste('The community directory [', dir, '] does not exist', 
                   sep=''))
    }        

    nodes <- read.csv(file.path(dir, 'nodes.csv'), 
                                stringsAsFactors=FALSE)

    properties <- read.csv(file.path(dir, 'properties.csv'), 
                           stringsAsFactors=FALSE)
    stopifnot(1==nrow(properties))
    properties <- do.call(list, properties[1,,drop=FALSE])

    trophic.links <- NULL
    if(file.exists(file.path(dir, 'trophic.links.csv')))
    {
        trophic.links <- read.csv(file.path(dir, 'trophic.links.csv'), 
                                  stringsAsFactors=FALSE)
    }

    return (Community(nodes=nodes, trophic.links=trophic.links, 
                      properties=properties))
}

SaveCommunity <- function(community, dir)
{
    if(!is.Community(community)) stop('Not a Community')
    if(!file.exists(dir))
    {
        dir.create(dir, recursive=TRUE)
    }

    write.csv(NPS(community), file=file.path(dir, 'nodes.csv'), 
              row.names=FALSE, na='')

    # Get the predation matrix into a data.frame in the form resource,consumer
    tlp <- TLPS(community)
    if(!is.null(tlp))
    {
        write.csv(as.data.frame(tlp), file.path(dir, 'trophic.links.csv'), 
                  row.names=FALSE, na='')
    }

    write.csv(do.call('cbind.data.frame', CPS(community)), 
              file=file.path(dir, 'properties.csv'), 
              row.names=FALSE)
}

.ResolveToNodeIndices <- function(community, nodes)
{
    # Returns a named vector containing integer indices of the given nodes.
    # nodes can be 
    # integers (an error is raised if any values <1 or >NumberOfNodes) 
    # character (an error is raised if any values are not node names) 
    # or logical (an error is raised if length(nodes)!=NumberOfNodes).
    if(!is.Community(community)) stop('Not a Community')
    if(is.null(nodes))
    {
        return (NULL)
    }
    else if(is.character(nodes))
    {
        return (NodeNameIndices(community, nodes))
    }
    else if(is.logical(nodes))
    {
        stopifnot(length(nodes)==NumberOfNodes(community))
        nodes <- which(nodes)
        names(nodes) <- NP(community, 'node')[nodes]
        return (nodes)
    }
    else
    {
        stopifnot(all(nodes>0 & nodes<=NumberOfNodes(community)))
        names(nodes) <- NP(community, 'node')[nodes]
        return (nodes)
    }
}

NodeNameIndices <- function(community, nodes)
{
    # Returns the integer indices of the given nodes
    if(!is.Community(community)) stop('Not a Community')
    if(0==length(nodes))    return (NULL)

    np <- NPS(community)
    bad <- !nodes %in% np$node
    if(any(bad))
    {
        stop(paste('[', paste(unique(nodes[bad]), collapse=','), 
                   '] are not node names', sep=''))
    }

    return (sapply(nodes, function(s) return (which(s==np$node))))
}

NumberOfNodes <- function(community)
{
    # The number of nodes in the community
    if(!is.Community(community)) stop('Not a Community')
    return (nrow(NPS(community)))
}

RemoveNodes <- function(community, remove, title=NULL)
{
    # Returns a new community which is community with the given nodes removed.
    if(!is.Community(community)) stop('Not a Community')
    if(is.null(remove) || 0==length(remove))
    {
        return (community)
    }

    if(is.null(title))
    {
        title <- paste(CP(community, 'title'),  ' (', 
                       paste(remove, collapse=', '), ' removed)', sep='')
    }

    np <- NPS(community)

    if(is.logical(remove))
    {
        remove <- which(remove)
    }

    if(is.numeric(remove) || is.integer(remove))
    {
        remove <- np$node[remove]
    }

    if(length(remove)==NumberOfNodes(community))
    {
        stop("Removing these nodes would result in an empty community")
    }

    new.nodes <- np[-NodeNameIndices(community, remove),,drop=FALSE]

    new.trophic.links <- TLPS(community)
    if(!is.null(new.trophic.links))
    {
        keep <- !new.trophic.links[,'resource'] %in% remove & 
                !new.trophic.links[,'consumer'] %in% remove
        new.trophic.links <- new.trophic.links[keep,,drop=FALSE]

        # Removing nodes may result in a community with no trophic links
        if(0==nrow(new.trophic.links))
        {
            new.trophic.links <- NULL
        }
    }

    new.properties <- CPS(community)
    new.properties$title <- title

    return (Community(nodes=new.nodes, trophic.links=new.trophic.links, 
                      properties=new.properties))
}

RemoveIsolatedNodes <- function(community, title=NULL)
{
    # Returns a copy of community with isolated nodes removed
    if(!is.Community(community)) stop('Not a Community')
    isolated <- IsolatedNodes(community)
    if(0==length(isolated))
    {
        return (community)
    }

    if(is.null(title))
    {
        title <- paste(CP(community, 'title'), '(isolated nodes removed)')
    }

    return (RemoveNodes(community, isolated, title))
}

LumpNodes <- function(community, lump, title=NULL, 
                       class.behaviour=list(integer=MeanNaRm, 
                                            numeric=MeanNaRm, 
                                            character=JoinUnique, 
                                            logical=JoinUnique), 
                       column.behaviour=NULL)
{
    # Returns a community in which nodes are lumped. 
    #   lump - a vector of of length NumberOfNodes containing names of lumped 
    #          nodes
    #   class.behaviour - 
    #   column.behaviour - 
    if(!is.Community(community)) stop('Not a Community')

    # Must have one entry per node
    stopifnot(is.character(lump))
    stopifnot(length(lump)==NumberOfNodes(community))
    stopifnot(all(nchar(lump)>0))
    names(lump) <- unname(NP(community, 'node'))
    
    # Functions for aggregating values
    stopifnot("function" == unique(lapply(class.behaviour, class)))
    stopifnot(is.null(column.behaviour) || 
              ('list'==class(column.behaviour) && 
               !is.null(names(column.behaviour))))

    if(is.null(title))
    {
        title <- paste(CP(community, 'title'), '(lumped)')
    }

    # Lump node properties
    new.nodes <- .AggregateDataFrame(NPS(community)[,-1], 
                                     lump, 
                                     column.behaviour, 
                                     class.behaviour)
    new.nodes <- cbind(node=unique(lump), new.nodes)

    # Lump trophic links and properties
    old.tlp <- new.tlp <- TLPS(community)
    if(!is.null(old.tlp))
    {
        old.nodes <- NP(community, 'node')
        new.tlp <- new.tlp[,c('resource', 'consumer')]
        for(index in 1:NumberOfNodes(community))
        {
            new.tlp[new.tlp == old.nodes[index]] <- lump[index]
        }

        new.tlp <- new.tlp[!duplicated(new.tlp),,drop=FALSE]
        aggregate.by <- paste(lump[old.tlp$resource], lump[old.tlp$consumer])
        if(ncol(old.tlp)>2)
        {
            # Aggregate columns other than 'resource' and 'consumer'
            select.cols <- !colnames(old.tlp) %in% c('resource', 'consumer')
            old.tlp <- old.tlp[,select.cols,drop=FALSE]
            new.tlp <- cbind(new.tlp, .AggregateDataFrame(old.tlp, 
                                                          aggregate.by, 
                                                          column.behaviour, 
                                                          class.behaviour))
        }
    }

    new.properties <- CPS(community)
    new.properties$title <- title

    return (Community(nodes=new.nodes, trophic.links=new.tlp, 
                      properties=new.properties))
}

LumpTrophicSpecies <- function(community, include.isolated=TRUE, title=NULL,...)
{
    # Returns a community in which nodes are lumped into trophic species.
    if(!is.Community(community)) stop('Not a Community')

    if(is.null(title))
    {
       title <- paste(CP(community, 'title'), '(trophic species lumped)')
    }

    # TrophicSpecies assigns isolated species a trophic species of NA if 
    # include.isolated is FALSE. NA in trophic species numbers causes 
    # problems so these are removed.
    if(!include.isolated)
    {
        community <- RemoveIsolatedNodes(community)
    }

    ts <- TrophicSpecies(community, include.isolated=TRUE)
    return (LumpNodes(community, title=title, 
                      lump=paste('Trophic species', ts), ...))
}

OrderCommunity <- function(community, ..., decreasing=FALSE, na.last=TRUE, 
                           new.order=NULL, title=NULL)
{
    # Returns a new Community with nodes reordered.

    # Dots should contain names of node properties by which to order. 
    # in which case the community is ordered by the order of property values,
    # or a function, which should take a community object as its only parameter 
    # and should return a vector of length NumberOfNodes(community); the 
    # community will be ordered by the order of the returned vector.
    # new.order is ignored if order.by is provided
    if(!is.Community(community)) stop('Not a Community')

    if(is.null(title))
    {
        title <- paste(CP(community, 'title'), '(reordered)')
    }

    if(is.null(new.order))
    {
        order.by <- list(...)
        order.by <- NPS(community, order.by)[,unlist(order.by)]
        if(!is.null(dim(order.by))) order.by <- as.list(order.by)
        else                        order.by <- list(order.by)
        args <- c(order.by, 
                  decreasing=decreasing, 
                  na.last=na.last)
        new.order <- do.call('order', args)
    }
    else if(is.character(new.order))
    {
        new.order <- NodeNameIndices(community, new.order)
    }

    stopifnot(all(0<new.order & new.order<=NumberOfNodes(community)))

    # One values of new.order per node
    stopifnot(length(unique(new.order))==NumberOfNodes(community))
    stopifnot(range(new.order)==c(1,NumberOfNodes(community)))

    nodes <- NPS(community)
    nodes <- nodes[new.order,,drop=FALSE]

    trophic.links <- TLPS(community)

    properties <- CPS(community)
    properties$title <- title
    return (Community(nodes=nodes, trophic.links=trophic.links, 
                      properties=properties))
}

ApplyByClass <- function(community, property, class, fn, ...)
{
    # Applies fn to property by class. property and class should both be names 
    # that meet the criteria of the properties argument of NPS().
    # class defaults to 'category', if the community has a node property 
    # called 'category'.

    if(!is.Community(community))   stop("Not a Community")
    if(missing(class))
    {
        if ("category" %in% NodePropertyNames(community))
        {
            class <- "category"
        }
        else
        {
            class <- NULL
        }
    }
    if(1==length(class)) class <- NPS(community, class)[, 1]
    stopifnot(length(class) == NumberOfNodes(community))
    stopifnot(!all(is.na(class)))
    np <- NPS(community, property)[, property]
    res <- tapply(np, class, fn, ...)
    return(do.call("c", list(res)))
}

LinearRegressionByClass <- function(community, X, Y, class)
{
    # Returns a list of linear regressions through values / node properties 
    # x and y. One model fitted through all points and one per each class. 
    # Points with x and/or y of NA are ignored.

    if(!is.Community(community)) stop('Not a Community')

    if(NumberOfNodes(community)<2)
    {
        return (NULL)
    }

    if(missing(class) && 'category' %in% NodePropertyNames(community))
    {
        class <- 'category'
    }    

    if(!is.null(class) && 1==length(class))   class <- NPS(community, class)[,1]
    X <- NPS(community, X)[,1]
    Y <- NPS(community, Y)[,1]

    stopifnot(is.null(class) || length(class)==NumberOfNodes(community))
    stopifnot(length(X)==NumberOfNodes(community))
    stopifnot(length(Y)==NumberOfNodes(community))

    include <- which(!is.na(X) & !is.na(Y))
    if(0==length(include))
    {
        return (NULL)
    }

    DoLM <- function(indices)
    {
        # Returns a linear model fitted to the given indices
        indices <- intersect(include, indices)
        if(length(indices)>1)
        {
            x <- X[indices]
            y <- Y[indices]
            return (lm(y~x, data=data.frame(x=x, y=y)))
        }
        else
        {
            return (NULL)
        }
    }

    models <- list(all=DoLM(1:length(X)))
    for(klass in unique(class))
    {
        indices <- which(klass==class)
        if(length(indices)>1)
        {
            model <- DoLM(indices)
            if(!is.null(model))
            {
                models[[1+length(models)]] <- model
                names(models)[length(models)] <- klass
            }
        }
    }

    return (models)
}

NumberOfNodesByClass <- function(community, class)
{
    return (ApplyByClass(community, property='node', fn=length, class=class))
}

FractionOfNodesByClass <- function(community, class)
{
    return (NumberOfNodesByClass(community, class) / NumberOfNodes(community))
}
