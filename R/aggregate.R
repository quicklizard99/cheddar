# Functions for aggregate nodes within communities and communities within 
# collections.
MeanNaRm <- function(x)
{
    # The mean of x, NA removed. If all values are NA, NA is returned.
    if(all(is.na(x))) return (NA)
    else              return (mean(x, na.rm=TRUE))
}

MedianNaRm <- function(x)
{
    # The median of x, NA removed. If all values are NA, NA is returned.
    if(all(is.na(x))) return (NA)
    else              return (median(x, na.rm=TRUE))
}

SumNaRm <- function(x)
{
    # The sum of x, NA removed. If all values are NA, NA is returned.
    if(all(is.na(x))) return (NA)
    else              return (sum(x, na.rm=TRUE))
}

MinNaRm <- function(x)
{
    # The min of x, NA removed. If all values are NA, NA is returned.
    if(all(is.na(x))) return (NA)
    else              return (min(x, na.rm=TRUE))
}

MaxNaRm <- function(x)
{
    # The max of x, NA removed. If all values are NA, NA is returned.
    if(all(is.na(x))) return (NA)
    else              return (max(x, na.rm=TRUE))
}

JoinUnique <- function(x)
{
    # Joins together unique values in x with a comma.
    return (paste(unique(x), collapse=','))
}

ExpectSingleUnique <- function(x)
{
    # Raises an error if x contains more than one unique value. Returns the 
    # single unique values otherwise.
    values <- unique(x)
    if(length(values)>1)
    {
        stop(paste('Contains more than one unique value [', 
                   paste(values, collapse=','), ']', sep=''))
    }
    else
    {
        return (values)
    }
}

ExpectSingleUniqueEmptyRm <- function(x)
{
    # Raises an error if x contains more than one unique value, exclusing NA. 
    # Returns the single unique values otherwise.
    values <- unique(x)
    empty <- is.na(values) | ''==values
    if(all(empty))
    {
        return (NA)
    }
    else if(any(empty))
    {
        # The test above is required because this line results in a vector 
        # of length 0 if no values are NA.
        values <- values[-empty]
    }
    else
    {
        return (ExpectSingleUnique(values))
    }
}

.AggregateDataFrame <- function(data, aggregate.by, column.behaviour, 
                                class.behaviour)
{
    if(is.character(aggregate.by))
    {
        # tapply converts aggregate.by to a factor. If aggregate.by is a 
        # character, is will be sorted alphabetically. 
        # This code retains the existing ordering.
        aggregate.by <- factor(aggregate.by, levels=unique(aggregate.by))
    }
    
    new.data <- lapply(colnames(data), function(n) 
    {
        fn <- column.behaviour[[n]]
        if(is.null(fn)) fn <- class.behaviour[[class(data[,n])]]
        if(is.null(fn))
        {
            stop(paste('No function to aggregate node property [', n, '] of ', 
                       'class [', class(data[,n]), ']', sep=''))
                 
        }

        vals <- tryCatch(as.vector(tapply(data[,n], aggregate.by, fn)), 
                         error=function(e) e)

        if('simpleError' %in% class(vals))
        {
            # Raising this error inside tryCatch() results in an ugly 
            # traceback.
            stop(paste('Error aggregating [', n, ']: ', vals$message, sep=''))
        }
        else
        {
            stopifnot(length(vals)==length(unique(aggregate.by)))

            col <- data.frame(vals, stringsAsFactors=FALSE)
            colnames(col) <- n

            return (col)
        }
    })
    new.data <- do.call('cbind', new.data)
    return (new.data)
}

