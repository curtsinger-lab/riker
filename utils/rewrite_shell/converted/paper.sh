#!/bin/sh

pdflatex  main.tex  
bibtex  main.aux  
pdflatex  main.tex  
pdflatex  main.tex  

pdflatex  extended-abstract.tex  
bibtex  extended-abstract.aux  
pdflatex  extended-abstract.tex  
pdflatex  extended-abstract.tex  
