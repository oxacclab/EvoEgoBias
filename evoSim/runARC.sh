#!/bin/bash

#SBATCH --nodes=1
#SBATCH --time=01:00:00
#SBATCH --job-name=EvoEgoBias

module purge
module load R

Rscript EvoEgoBias/model1.R