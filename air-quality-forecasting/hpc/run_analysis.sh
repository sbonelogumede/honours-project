#!/bin/bash
#SBATCH --job-name=run_analysis
#SBATCH --output=logs/run_analysis_%j.out
#SBATCH --error=logs/run_analysis_%j.err
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
echo "Starting running analysis..."
Rscript run_analysis.R

echo "Analysis complete!"
date
