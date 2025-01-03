#!/bin/bash

source /etc/profile
# Loading the required module
module load anaconda/2023b
source activate bayesR

# Run the script
Rscript -e "targets::tar_make()"
Rscript -e "targets::tar_meta(fields = error, complete_only = TRUE)"
