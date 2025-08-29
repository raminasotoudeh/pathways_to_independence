# PSID TA Cleaning Script
# This script processes raw PSID TA data files and saves them as RDS files

setwd("./data")

# Get list of all files in the directory
all_files = list.files()

# Filter for text data files 
text_files = all_files[grepl("txt", all_files)]
text_files = sort(text_files)

# Filter for SPSS syntax files containing variable definitions
sps_files = all_files[grepl("sps", all_files)]
sps_files = sort(sps_files)

# Process each pair of data and syntax files
for (x in 1:length(text_files)){
  
  # Read SPSS syntax file to extract variable definitions
  sps_vals = readLines(sps_files[x])
  
  # Skip header lines 
  sps_vals = sps_vals[11:length(sps_vals)]
  
  # Find the end of variable definitions
  split_location = which(sps_vals == "   .")[1]-1
  sps_vals = sps_vals[1:split_location]
  
  # Parse variable definitions
  sps_vals = strsplit(sps_vals, " ")
  sps_vals = lapply(sps_vals, function(x) trimws(subset(x, !x %in% c("", "(A)"))))
  
  # Convert to matrix format
  sps_vals = matrix(unlist(sps_vals), ncol = 4, byrow = T)
  # Remove the format column (column 3) as we only need positions
  sps_vals = sps_vals[,-3]

  # Read the raw fixed-width text data file
  txt_vals = readLines(text_files[x])
  
  # Initialize list to store processed rows
  rows_fixed = list()
  
  # Process each row of data
  for ( j in 1:length(txt_vals) ){

    # Split each row into individual characters
    row_split = strsplit(txt_vals[j], "")[[1]]
    
    # Create empty matrix to hold extracted values for this row
    row_vals = matrix("", ncol = nrow(sps_vals), nrow = 1)
    
    # Extract each variable using its defined position range
    for ( i in 1:nrow(sps_vals) ){
      row_vals[i] = trimws(paste0(row_split[sps_vals[i,2]:sps_vals[i,3]], collapse = ""))
    }
    
    # Convert to data frame and assign variable names from .sps file
    row_vals = as.data.frame(row_vals)
    colnames(row_vals) = sps_vals[,1]
    
    # Add processed row to the list
    rows_fixed[[length(rows_fixed) + 1]] = row_vals
  }
  
  # Combine all rows into a single data frame
  rows_fixed = do.call("rbind", rows_fixed)

  # Output
  new_name = text_files[x]
  new_name = gsub(".txt", "", new_name)
  saveRDS(rows_fixed, paste0("./cleaned_ta_data/", new_name, ".RDS"))
}