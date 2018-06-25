#!/bin/bash

# set the number of nodes
#SBATCH --nodes=1

# set max wallclock time
#SBATCH --time=01:00:00

# set name of job
#SBATCH --job-name=EvoEgoBias

# mail alert at start, end and abortion of execution
#SBATCH --mail-type=ALL

# send mail to this address
#SBATCH --mail-user=matt.jaquiery@psy.ox.ac.uk

module purge
module load R

Rscript EvoEgoBias/model1.R