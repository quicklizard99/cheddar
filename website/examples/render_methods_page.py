#!/usr/bin/env python
""" Prints methods.md to stdout, rendered by jinja2, using data in examples.csv,
references.csv and files in output/
"""

from unicode_csv import UnicodeDictReader
from jinja2 import Environment, FileSystemLoader

env = Environment(loader=FileSystemLoader('.'), 
                  extensions=['jinja2_highlight.HighlightExtension'])

template = env.get_template('methods.md')
print template.render(methods=UnicodeDictReader(file('examples.csv','rb')), 
                      references=UnicodeDictReader(file('references.csv','rb'))
                     )
