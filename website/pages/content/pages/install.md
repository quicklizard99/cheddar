Title: install
URL: install.html
save_as: install.html

## Released version

The current stable release (currently 0.1-631) is always available on
[CRAN](http://cran.r-project.org/web/packages/cheddar/index.html) and can be
installed in R simply by entering:

    :::S
    install.packages('cheddar')

Cheddar's vignettes show you round the package:

    :::S
    library(cheddar)
    vignette('CheddarQuickstart') # Essential reading
    vignette('Community')         # Also essential
    vignette('PlotsAndStats')
    vignette('Collections')
    vignette('ImportExport')

[Release history](https://github.com/quicklizard99/cheddar/release_history.md).

Send questions, comments or other feedback to me (l.hudson (at) nhm.ac.uk), or 
use the 
[issue tracker](https://github.com/quicklizard99/cheddar/issues).

## Help me get my data into Cheddar!

Researchers typically use their own bespoke data formats and there are probably 
as many data formats as there are researchers. It is therefore extremely hard 
to write a generic 'import my data into Cheddar' function.

If you have community data that you would like to import into Cheddar, please 
contact me (l.hudson (at) nhm.ac.uk) - I will either provide example 
data-import R code for you to modify or will write the required code for you.
