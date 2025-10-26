#!/bin/bash
#SBATCH --job-name=extract_components
#SBATCH --output=logs/07_extract_components_%j.out
#SBATCH --error=logs/07_extract_components_%j.err
#SBATCH --time=24:00:00
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=5
#SBATCH --mem=40G
#SBATCH --partition=ada

# Load R module
module load R

# Create logs directory if it doesn't exist
mkdir -p logs

# Run component extraction script
echo "Starting GP component extraction..."
Rscript 07_extract_components.R

echo "Component extraction complete!"
date
