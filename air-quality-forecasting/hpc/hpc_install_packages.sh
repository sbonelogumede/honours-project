#!/bin/bash
#SBATCH --job-name=install_R_pkgs
#SBATCH --output=logs/install_packages.out
#SBATCH --error=logs/install_packages.err
#SBATCH --time=01:00:00
#SBATCH --mem=4G
#SBATCH --cpus-per-task=1

# HPC Package Installation Script
# ================================

echo "=========================================="
echo "R Package Installation - Starting"
echo "Date: $(date)"
echo "=========================================="

# Load R module (adjust version as needed for your HPC)
module purge
module load R/4.3.0  # Adjust to your available R version

# Display R information
echo ""
echo "R Version Information:"
R --version
echo ""

# Set R library path
export R_LIBS_USER=~/R/x86_64-pc-linux-gnu-library/$(R --version | grep "R version" | sed 's/R version \([0-9]\+\.[0-9]\+\).*/\1/')

echo "R_LIBS_USER set to: $R_LIBS_USER"
echo ""

# Run the installation script
echo "Installing R packages..."
Rscript install_packages.R

# Check exit status
if [ $? -eq 0 ]; then
    echo ""
    echo "=========================================="
    echo "Package installation completed successfully!"
    echo "Date: $(date)"
    echo "=========================================="
else
    echo ""
    echo "=========================================="
    echo "ERROR: Package installation failed!"
    echo "Check logs/install_packages.err for details"
    echo "Date: $(date)"
    echo "=========================================="
    exit 1
fi
