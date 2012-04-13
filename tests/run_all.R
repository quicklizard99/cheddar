options(warn=2)
library(cheddar)

F <- function(ex)
{
    # A function that expects an exception to be raise when ex is evalutated
    res <- tryCatch(eval(ex), error=function(e) e)
    if(!"error" %in% class(res))
    {
        stop('F: did not raise error\n')
    }
}

# Data for test plans
data(Benguela, BroadstoneStream, SkipwithPond, TL84, TL86, YthanEstuary, pHWebs)

# One species. No properties.
c1 <- Community(nodes=data.frame(node='S'), properties=list(title='c1'))

# One cannibalistic species
c2 <- Community(nodes=data.frame(node='S'), 
                trophic.links=cbind(resource='S', consumer='S'), 
                properties=list(title='c2'))

# Resource-consummer
c3 <- Community(nodes=data.frame(node=c('R','C')), 
                trophic.links=cbind(resource='R', consumer='C'), 
                properties=list(title='c3'))

# Three-species chain
c4 <- Community(nodes=data.frame(node=c('R','C','P')), 
                trophic.links=cbind(resource=c('R','C'), 
                                    consumer=c('C','P')), 
                properties=list(title='c4'))

# IGP
c5 <- Community(nodes=data.frame(node=c('R','C','O')), 
                trophic.links=cbind(resource=c('R','R','C'), 
                                    consumer=c('C','O','O')), 
                properties=list(title='c5'))

# Three species chain with M, N, taxonomy and trophic link properties
c6 <- Community(nodes=data.frame(node=c('R','C','P'), 
                                 M=c(1.5, 5, 100), 
                                 N=c(1000, 10, 5.5), 
                                 order=c('Order 1', 'Order 2', 'Order 2'), 
                                 family=c('Family 1', 'Family 2', 'Family 3'), 
                                 stringsAsFactors=FALSE), 
                trophic.links=data.frame(resource=c('R','C'), 
                                         consumer=c('C','P'),  
                                         link.evidence=c('Inferred', 'Known'), 
                                         link.strength=c(0.5, 0.2), 
                                         stringsAsFactors=FALSE), 
                properties=list(title='c6', M.units='g', 
                                N.units='m^-3'))

RunTests <- function(tests)
{
    # tests should be a vector of function names
    if(0==length(tests))
    {
        stop('No tests to run! Is the working directory cheddar/tests ?')
    }
    else
    {
        failed <- NULL

        for(test in tests)
        {
            cat(paste('Running [', test, ']\n', sep=''))
            res <- tryCatch(do.call(test, args=list()), error=function(e) e)
            if(!is.null(res))
            {
                # Strip whitespace from error message
                res <- gsub('\n', '', res)
                cat(paste('[', test, '] raised unexpected error [', res, ']\n', 
                    sep=''))
                traceback()
                failed <- c(failed, test)
            }
            else
            {
                
            }
        }

        cat(paste(length(tests), 'tests ran.\n'))
        cat(paste(length(tests) - length(failed), 'passed.\n'))
        if(!is.null(failed))
        {
            cat(paste(length(failed), 'failed.\n'))
            stop()
        }
    }
}

# Source all files in this dir except this one
files <- list.files(getwd(), pattern='*R$')
files <- setdiff(files, 'run_all.R')
junk <- sapply(file.path(getwd(), files), source)
RunTests(ls(pattern=glob2rx('Test*')))

