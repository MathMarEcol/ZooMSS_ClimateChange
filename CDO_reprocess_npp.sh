#!/bin/bash

# script to regrid npp

# Start with TOS
indir="/Users/jason/Nextcloud/MME1Data/ZooMSS_Climate_Change/raw/npp/"
outdir="/Users/jason/Nextcloud/MME1Data/ZooMSS_Climate_Change/regrid/npp/"

# Declare an array of string with type
declare -a ModelArray=("CESM2" "UKESM1-0-LL" "GFDL-ESM4" "MPI-ESM1" "IPSL-CM6A-LR")
declare -a ExpArray=("historical") # "ssp126" "ssp370" "ssp585")

# Iterate the string array using for loop
for m in ${ModelArray[@]}; do
   echo $m

	for e in ${ExpArray[@]}; do
		echo $e
		curr_files=($(ls ${indir}*$m*$e*))
		num_files=${#curr_files[@]}
   		echo $curr_files

		for ((i=0; i<=num_files-1; i++)); do
			curr_file=${curr_files[i]}
			cdo -sinfov $curr_file # Print the current details of the file

			out_name1="tempfile_npp.nc"
			cdo -sellevidx,1 $curr_file $out_name1 # Extract layer 1 (surface)
			
			out_name2="tempfile_npp2.nc"
			cdo -L -remapbil,global_1 -selyear,1950/2100 -selname,pp $out_name1 $out_name2 # Remap to 1 degree global on the half-degree
			# 
			annual_name=${curr_file/_gn_/_onedeg_} # Replace gn or gr with onedeg in filename
			annual_name=${annual_name/_gr_/_onedeg_}
			annual_name=${annual_name/pp_Omon_/ppos_Oyr_} # Replace Omon with Oyr to indicate annual average and surface values only
			annual_name=${annual_name/raw/regrid} # change directory
			cdo -yearmean $out_name2 $annual_name

			#Clean up
			rm $out_name1 $out_name2
		done
		
		merged_name=${annual_name::${#annual_name}-16} # Remove the dates
		merged_name=${merged_name/_onedeg_/_onedeg_merged.nc} # Add merged tag
		merged_name=${merged_name/regrid/merged} # change directory
		cdo -O -mergetime $outdir*$m*$e* $merged_name

	done
done
