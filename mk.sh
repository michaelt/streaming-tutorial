#!/usr/bin/env bash

pandoc -r markdown -w latex -s --latex-engine=xelatex -s tutorial.md -o tutorial.pdf 
open tutorial.pdf
rm *.aux *.log *.out *.synctex.gz *.bcf *.bbl *.blg *.ent *.nav *.toc *.vrb *snm
