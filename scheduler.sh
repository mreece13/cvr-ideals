#!/bin/bash

# Job Flags
#SBATCH -p mit_preemptable
#SBATCH -c 80
#SBATCH --mem=160G

module load miniforge/24.3.0-0
module load gcc
mamba activate cvr-ideals

# Run the script
Rscript -e "targets::tar_make()"
Rscript -e "targets::tar_meta(fields = error, complete_only = TRUE) |> dplyr::pull(error)"
