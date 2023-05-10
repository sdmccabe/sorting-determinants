#!/bin/bash
set -euo pipefail

echo "Processing ANES data..." | tee run.log
Rscript src/01-process_anes_data.R 2>&1 | tee -a run.log
echo "Fitting models..." | tee -a run.log
Rscript src/02-run_models.R 2>&1 | tee -a run.log

cd results/appendix
echo "Compiling appendix..." | tee -a ../../run.log
latexmk --xelatex latex_appendix.tex 2>&1 | tee -a ../../run.log
cd ../..
