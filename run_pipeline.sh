#!/bin/bash

# Job Flags
#SBATCH -p mit_normal
#SBATCH -c 8
#SBATCH --mem=64G

module load miniforge
module load gcc
mamba activate cvr-ideals

# Run the script
Rscript -e "targets::tar_make()"
Rscript -e "targets::tar_meta(fields = error, complete_only = TRUE)"
