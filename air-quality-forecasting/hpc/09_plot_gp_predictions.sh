#!/bin/bash
#SBATCH --job-name=plot_gp_predictions
#SBATCH --output=logs/09_plot_gp_predictions_%j.out
#SBATCH --error=logs/09_plot_gp_predictions_%j.err
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
echo "Starting plotting gp predictions..."
echo "Using $SLURM_CPUS_PER_TASK cores"
Rscript 09_plot_gp_predictions.R

echo "Plotting GP predictions complete!"
date
