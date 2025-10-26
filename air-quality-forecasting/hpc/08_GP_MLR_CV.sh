#!/bin/bash
#SBATCH --job-name=GP_MLR_CV
#SBATCH --output=logs/08_GP_MLR_CV_%j.out
#SBATCH --error=logs/08_GP_MLR_CV_%j.err
#SBATCH --time=48:00:00
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=28
#SBATCH --mem=64G
#SBATCH --partition=ada
#SBATCH --mail-type=ALL
#SBATCH --mail-user=gmdsbo006@myuct.ac.za

# Load R module
module load R

# Create logs directory if it doesn't exist
mkdir -p logs

# Run GP cross-validation
echo "Starting GP cross-validation..."
echo "Using $SLURM_CPUS_PER_TASK cores"
Rscript 08_GP_MLR_CV.R

echo "GP cross-validation complete!"
date
