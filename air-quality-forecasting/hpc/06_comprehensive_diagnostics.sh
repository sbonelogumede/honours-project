#!/bin/bash
#SBATCH --job-name=gp_diagnostics
#SBATCH --output=logs/06_diagnostics_%j.out
#SBATCH --error=logs/06_diagnostics_%j.err
#SBATCH --time=01:00:00
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=24G
#SBATCH --partition=ada

# Load R module
module load R

# Create logs directory if it doesn't exist
mkdir -p logs

# Run diagnostics script
echo "Starting GP model diagnostics..."
Rscript 06_comprehensive_diagnostics.R

echo "Diagnostics complete!"
date
