# Website

Source for the [cheddar][cheddar-pages] website hosted on [github-pages][gh-pages].

## cheddar-simple
A [pelican][pelican] theme.

## examples
Generates pages/content/pages/methods.md

## pages
The [pelican][pelican] project

# Install

    pip install -r requirements.txt
    pelican-themes -i cheddar-simple

# Build

* Install the latest release of R 
* Build and install the latest release of the [cheddar R package][cheddar-R]

## Make examples
    cd examples
    ./run_examples.R
    ./render_methods_page.py > ../pages/content/pages/methods.md
    cd ..

## Make pelican pages
    make -C pages html

## Copy example images to pelican output
    mkdir -p pages/output/static/images/
    cp examples/output/*png pages/output/static/images/


## Publish
    ghp-import pages/output
    git push origin gh-pages


[cheddar-pages]: http://quicklizard99.github.io/cheddar/ "Cheddar"
[cheddar-R]: https://github.com/quicklizard99/cheddar "Cheddar"
[pelican]: http://blog.getpelican.com/ "Pelican"
[gh-pages]: http://pages.github.com/ "GitHub Pages"
