#!/usr/bin/env python
from unicode_csv import UnicodeDictReader
from jinja2 import Environment, FileSystemLoader

env = Environment(loader=FileSystemLoader('.'), 
                  extensions=['jinja2_highlight.HighlightExtension'])

template = env.get_template('methods.md')
print template.render(methods=UnicodeDictReader(file('examples.csv','rb')), 
                      references=UnicodeDictReader(file('references.csv','rb'))
                     )
