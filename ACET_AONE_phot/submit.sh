#!/bin/sh
# ****************************************************************************
# Wrapper script for submitting jobs on ACRC HPC
# docs: https://www.acrc.bris.ac.uk/protected/hpc-docs/index.html
# ****************************************************************************
#SBATCH --job-name=run_stochem
#SBATCH --output=run_stochem.out
#SBATCH --error=run_stochem.err
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=4
#SBATCH --time=144:00:00
#SBATCH --mem=10gb
#SBATCH --account=chem007981
 
 
module load languages/Intel-OneAPI/2024.0.2

for_file="Base_test.for"

base_name=$(basename "$for_file" .for)

ifort "$for_file" -o "$base_name.o"
printf '97\n1\n1\n365\n' | ./"$base_name.o"
 
# year - start_month - start_day - run length(days)

# Delete all stoch3d files
rm -f stoch3d*

# Copy and rename DUMP.bin 
cp DUMP.BIN "DUMP-$base_name"
# Rename original DUMP.bin to RESTART
mv DUMP.BIN RESTART.BIN

ifort "$for_file" -o "$base_name.o"
printf '98\n1\n1\n365\n' | ./"$base_name.o"