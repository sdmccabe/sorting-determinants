#!/bin/bash
set -euo pipefail

Rscript src/anes_scratch.R
Rscript src/regression_scratch.R

cd results/appendix
latexmk --xelatex latex_appendix.tex
cd ../..
