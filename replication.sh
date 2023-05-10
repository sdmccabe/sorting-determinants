#!/bin/bash
set -euo pipefail

Rscript src/01-process_anes_data.R
Rscript src/02-run_models.R

cd results/appendix
latexmk --xelatex latex_appendix.tex
cd ../..
