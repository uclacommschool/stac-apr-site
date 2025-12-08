################################################################################
##
## [ PROJ ] < APR Website >
## [ FILE ] < 00 - Create QMDs.R >
## [ AUTH ] < Jeffrey Yo>
## [ INIT ] < 12/7/25>
##
################################################################################

#Goal: generate `.qmd` files for each version and update `_quarto.yml`

################################################################################

## ---------------------------
## libraries
## ---------------------------
library(tidyverse)
library(tictoc)
## ---------------------------
## directory paths
## ---------------------------

#see current directory
getwd()

#create file directories


## ---------------------------
## helper functions & strings
## ---------------------------

#run source file
rnd<-function(x,n){
  posneg=sign(x)
  z = abs(x)*10^n
  z = z + 0.5 + sqrt(.Machine$double.eps)
  z = trunc(z)
  z = z/10^n
  z*posneg
}

create_qmds<-function(template_path,level_folder, name_file){
  
  # Load template
  template <- readLines(template_path)
  
  # List to store navigation entries
  page_entries <- c()
  
  # Generate a separate .qmd for each version
 
    dir_path <-file.path(".",level_folder)
    if (!dir.exists(dir_path)) {
      dir.create(dir_path, recursive = TRUE)
    }
    
    #name <- paste0("report_", ver)
    filename <- file.path(dir_path, paste0(name_file, ".qmd"))
    
    # Replace placeholders
    new_content <- gsub("rtac_name", paste0('"', name_file, '"'), template)
    
    # Write to file
    writeLines(new_content, filename)
    
    # Add to _quarto.yml entries
    page_entries <- c(page_entries, paste0("  - file: ", filename))
}

create_qmds_group<-function(template_path, name_file,
                            group_name, full_name, 
                            data_folder){
  
  # Load template
  template <- readLines(template_path)
  
  # List of versions
  versions <- str_c("ver", c(0:6))
  
  # List to store navigation entries
  page_entries <- c()
  
  # Generate a separate .qmd for each version
  for (ver in versions) {
    
    dir_path <-file.path(".", "descriptives",
                         paste0("Version ", str_sub(ver, -1)))
    if (!dir.exists(dir_path)) {
      dir.create(dir_path, recursive = TRUE)
    }
    
    #name <- paste0("report_", ver)
    filename <- file.path(dir_path, paste0(name_file,"-",group_name,".qmd"))
    
    # Replace placeholders
    new_content <- gsub("ver_num", paste0(ver), template)
    new_content <- gsub("full_group_name", paste0(full_name), new_content)
    new_content <- gsub("group_name", paste0(group_name), new_content)
    new_content <- gsub("data_folder", paste0(data_folder), new_content)
    
    # Write to file
    writeLines(new_content, filename)
    
    # Add to _quarto.yml entries
    page_entries <- c(page_entries, paste0("  - file: ", filename))
  }
  
}

## ---------------------------
## load & inspect data
## ---------------------------

#add nested lists
source(file.path(".","code","00 - Create Nested List.R"))

## -----------------------------------------------------------------------------
## Part 1 - Create QMDs - RTACs
## -----------------------------------------------------------------------------

for (reg_names in region_names){
  create_qmds(file.path(".","qmd","templates","_regional_template.qmd"),
              "RTACs",reg_names) 
}

## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------