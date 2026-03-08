#!/bin/bash

# Job Flags
#SBATCH -p mit_normal_gpu
#SBATCH -c 16
#SBATCH --mem=160G
#SBATCH -G 1
#SBATCH -t 06:00:00

module load miniforge/24.3.0-0
module load gcc
module load cuda/12.4.0
mamba activate cvr-ideals

echo "Checking GPU availability:"
nvidia-smi
echo "Checking OpenCL:"
clinfo -l

# Run the script
Rscript -e "targets::tar_make(fit_gpu_adams)"
Rscript -e "targets::tar_meta(fields = error, complete_only = TRUE) |> dplyr::pull(error)"
