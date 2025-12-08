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


#plots overall
create_qmds(file.path(".","qmd","templates","_plots_overall_template.qmd"),
            "02_placement-plots")

## -----------------------------------------------------------------------------
## Part 2 - Create QMDs - Demographic Group Placements
## -----------------------------------------------------------------------------

#Demographics
#American Indian/Alaskan Native
create_qmds_group(file.path(".","qmd","templates","_plots_group_template.qmd"),
            "03_placement-plots", "ai_an", "American Indian/Alaskan Native",
            "sample_grade_race_list")

#Asian
create_qmds_group(file.path(".","qmd","templates","_plots_group_template.qmd"),
                  "04_placement-plots", "asian", "Asian",
                  "sample_grade_race_list")

#Black
create_qmds_group(file.path(".","qmd","templates","_plots_group_template.qmd"),
                  "05_placement-plots", "black", "Black",
                  "sample_grade_race_list")

#Hispanic
create_qmds_group(file.path(".","qmd","templates","_plots_group_template.qmd"),
                  "06_placement-plots", "hispanic", "Hispanic",
                  "sample_grade_race_list")

#Native Hawaiian/Pacific Islander
create_qmds_group(file.path(".","qmd","templates","_plots_group_template.qmd"),
                  "07_placement-plots", "nh_pi", "Native Hawaiian/Pacific Islander",
                  "sample_grade_race_list")

#Two or More Races
create_qmds_group(file.path(".","qmd","templates","_plots_group_template.qmd"),
                  "08_placement-plots", "multi_race", "Two or More Races",
                  "sample_grade_race_list")

#White
create_qmds_group(file.path(".","qmd","templates","_plots_group_template.qmd"),
                  "09_placement-plots", "white", "White",
                  "sample_grade_race_list")

#Economic Disadvantaged
create_qmds_group(file.path(".","qmd","templates","_plots_group_template.qmd"),
                  "10_placement-plots", "ec_dis", "Economically Disadvantaged",
                  "sample_grade_race_list")

#Disability
create_qmds_group(file.path(".","qmd","templates","_plots_group_template.qmd"),
                  "11_placement-plots", "sp_ed", "Disability",
                  "sample_grade_race_list")

#English Language Learners
create_qmds_group(file.path(".","qmd","templates","_plots_group_template.qmd"),
                  "12_placement-plots", "ell", "English Language Learners",
                  "sample_grade_race_list")

#Female
create_qmds_group(file.path(".","qmd","templates","_plots_group_template.qmd"),
                  "13_placement-plots", "female", "Female",
                  "sample_grade_race_list")

## -----------------------------------------------------------------------------
## Part 3 - Create QMDs - State Category Placements
## -----------------------------------------------------------------------------

#Advance
create_qmds_group(file.path(".","qmd","templates","_plots_group_template.qmd"),
                  "14_placement-plots", "adv", "LEAP - Advanced",
                  "sample_grade_state_cat_list")

#Mastery
create_qmds_group(file.path(".","qmd","templates","_plots_group_template.qmd"),
                  "15_placement-plots", "mas", "LEAP - Mastery",
                  "sample_grade_state_cat_list")

#Basic
create_qmds_group(file.path(".","qmd","templates","_plots_group_template.qmd"),
                  "16_placement-plots", "bas", "LEAP - Basic",
                  "sample_grade_state_cat_list")

#Approaching Basic
create_qmds_group(file.path(".","qmd","templates","_plots_group_template.qmd"),
                  "17_placement-plots", "app", "LEAP - Approaching Basic",
                  "sample_grade_state_cat_list")

#Unsatisfactory
create_qmds_group(file.path(".","qmd","templates","_plots_group_template.qmd"),
                  "18_placement-plots", "uns", "LEAP - Unsatisfaction",
                  "sample_grade_state_cat_list")

## -----------------------------------------------------------------------------
## Part 4 - Create QMDs - i-Ready Growth Placements
## -----------------------------------------------------------------------------

#none
create_qmds_group(file.path(".","qmd","templates","_plots_group_template.qmd"),
                  "19_placement-plots", "none", "i-Ready - No PI",
                  "sample_grade_growth_cat_list")

#use
create_qmds_group(file.path(".","qmd","templates","_plots_group_template.qmd"),
                  "20_placement-plots", "use", "i-Ready - Use PI",
                  "sample_grade_growth_cat_list")

#typical
create_qmds_group(file.path(".","qmd","templates","_plots_group_template.qmd"),
                  "21_placement-plots", "typical", "i-Ready - Typical Growth",
                  "sample_grade_growth_cat_list")

#stretch
create_qmds_group(file.path(".","qmd","templates","_plots_group_template.qmd"),
                  "22_placement-plots", "stretch", "i-Ready - Stretch Growth",
                  "sample_grade_growth_cat_list")

## -----------------------------------------------------------------------------
## Part 5 - Create QMDs - i-Ready Category Placements
## -----------------------------------------------------------------------------

#Mid or Above
create_qmds_group(file.path(".","qmd","templates","_plots_group_template.qmd"),
                  "23_placement-plots", "mid", "i-Ready - Mid or Above",
                  "sample_grade_diag_cat_list")

#Early
create_qmds_group(file.path(".","qmd","templates","_plots_group_template.qmd"),
                  "24_placement-plots", "early", "i-Ready - Early",
                  "sample_grade_diag_cat_list")

#Below 1 Year
create_qmds_group(file.path(".","qmd","templates","_plots_group_template.qmd"),
                  "25_placement-plots", "b1", "i-Ready - Below 1 Year",
                  "sample_grade_diag_cat_list")

#Below 2 Years
create_qmds_group(file.path(".","qmd","templates","_plots_group_template.qmd"),
                  "26_placement-plots", "b2", "i-Ready - Below 2 Years",
                  "sample_grade_diag_cat_list")

#Below 3 Years
create_qmds_group(file.path(".","qmd","templates","_plots_group_template.qmd"),
                  "27_placement-plots", "b3", "i-Ready - Below 3 or More Years",
                  "sample_grade_diag_cat_list")
toc()

#NOTE: For the sake of rendering sanity, I decided to only keep the overall results 
#and placements for Versions 0-5. The subgroup results are only presented for Version 6.  

## -----------------------------------------------------------------------------
## Part 6.1 - Create QMDs - Regressions & Matched Descriptives (Overall)
## -----------------------------------------------------------------------------

create_qmds_group_reg<-function(template_path, name_file,
                                group_name, full_name, 
                                dir_folder, dir_sample_name){
  
  # Load template
  template <- readLines(template_path)
  
  # List of versions
  versions <- str_c("ver", c(0:6))
  
  # List to store navigation entries
  page_entries <- c()
  
  # Generate a separate .qmd for each version
  for (ver in versions) {
    
    dir_path <-file.path(".", "regressions", dir_folder)
    if (!dir.exists(dir_path)) {
      dir.create(dir_path, recursive = TRUE)
    }
    
    #name <- paste0("report_", ver)
    filename <- file.path(dir_path, paste0(name_file,"-",group_name,".qmd"))
    
    # Replace placeholders
    new_content <- gsub("ver_num", paste0(ver), template)
    new_content <- gsub("full_group_name", paste0(full_name), new_content)
    new_content <- gsub("group_name", paste0(group_name), new_content)
    new_content <- gsub("dir_sample", paste0(dir_sample_name), new_content)
    
    # Write to file
    writeLines(new_content, filename)
    
    # Add to _quarto.yml entries
    page_entries <- c(page_entries, paste0("  - file: ", filename))
  }
  
}

s3_dir_list <-c("'Full-Data-PI'","'Full-Data-PI-Fidelity'",
             "'Non-Random-PI'","'Non-Random-PI-Fidelity'")

qmd_dir_list<-c("Full Data - PI", "Full Data - PI Fidelity",
                "Non-Random - PI", "Non-Random - PI Fidelity")

tic()
map2(qmd_dir_list, s3_dir_list,
     function(qmd_folder, s3_folder){
       
       #pre-matched descriptives
       create_qmds_group_reg(file.path(".","qmd","templates",
                                       "_match_pre_descriptives_template.qmd"),
                             "00-pre_match_desc", "all", "Overall",
                             qmd_folder,s3_folder)
       
       #matched descriptives
       create_qmds_group_reg(file.path(".","qmd","templates",
                                       "_match_descriptives_template.qmd"),
                             "01-match_desc", "all", "Overall",
                             qmd_folder,s3_folder)
       
       #scatter plots
       create_qmds_group_reg(file.path(".","qmd","templates",
                                       "_scatter_plots_template.qmd"),
                             "02-scatter-plots", "all", "Overall",
                             qmd_folder,s3_folder)
       
       #overall matched
       create_qmds_group_reg(file.path(".","qmd","templates",
                                       "_regressions_grade_template.qmd"),
                             "03-reg", "overall", "Overall",
                             qmd_folder,s3_folder)
       
       
       #overall unmatched
       create_qmds_group_reg(file.path(".","qmd","templates",
                                       "_regressions_grade_unmatch_template.qmd"),
                             "04-reg", "overall_unmatch", "Overall",
                             qmd_folder,s3_folder)
       
       #black
       create_qmds_group_reg(file.path(".","qmd","templates",
                                       "_regressions_demo_template.qmd"),
                             "05-reg", "black", "Black",
                             qmd_folder,s3_folder)
       
       #hispanic
       create_qmds_group_reg(file.path(".","qmd","templates",
                                       "_regressions_demo_template.qmd"),
                             "06-reg", "hispanic", "Hispanic",
                             qmd_folder,s3_folder)
       
       #multirace
       create_qmds_group_reg(file.path(".","qmd","templates",
                                       "_regressions_demo_template.qmd"),
                             "07-reg", "multirace", "Multirace",
                             qmd_folder,s3_folder)
       
       #female
       create_qmds_group_reg(file.path(".","qmd","templates",
                                       "_regressions_demo_template.qmd"),
                             "08-reg", "female", "Female",
                             qmd_folder,s3_folder)
       
       #ec_dis_22_c - Economic Disadvantaged
       create_qmds_group_reg(file.path(".","qmd","templates",
                                       "_regressions_demo_template.qmd"),
                             "09-reg", "ec_dis_22_c", "Economically Disadvantaged",
                             qmd_folder,s3_folder)
       
       #sp_ed_22_c - Disability
       create_qmds_group_reg(file.path(".","qmd","templates",
                                       "_regressions_demo_template.qmd"),
                             "10-reg", "sp_ed_22_c", "Disability",
                             qmd_folder,s3_folder)
       
       #toc - Students of Color
       create_qmds_group_reg(file.path(".","qmd","templates",
                                       "_regressions_demo_template.qmd"),
                             "11-reg", "toc", "Students of Color",
                             qmd_folder,s3_folder)
       
       #toc - White
       create_qmds_group_reg(file.path(".","qmd","templates",
                                       "_regressions_demo_template.qmd"),
                             "12-reg", "white", "White",
                             qmd_folder,s3_folder)
       
     }
     )
toc()
#491.256 sec elapsed

## -----------------------------------------------------------------------------
## Part 6.2 - Create QMDs-Regressions & Matched Descriptives (Striving Learners)
## -----------------------------------------------------------------------------

qmd_dir_list_sl<-str_c("SL: ",c("Full Data - PI", "Full Data - PI Fidelity",
                "Non-Random - PI", "Non-Random - PI Fidelity"))

tic()
map2(qmd_dir_list_sl, s3_dir_list,
     function(qmd_folder, s3_folder){
       
       # #matched descriptives
       # create_qmds_group_reg(file.path(".","qmd","templates",
       #                                 "_match_descriptives_template.qmd"),
       #                       "00-match_desc", "", "Overall",
       #                       qmd_folder,s3_folder)
       
       #overall matched
       create_qmds_group_reg(file.path(".","qmd","templates",
                                       "_regressions_grade_template_sl.qmd"),
                             "01-reg", "overall", "Overall",
                             qmd_folder,s3_folder)
       
       
       #overall unmatched
       create_qmds_group_reg(file.path(".","qmd","templates",
                                       "_regressions_grade_unmatch_template_sl.qmd"),
                             "02-reg", "overall_unmatch", "Overall",
                             qmd_folder,s3_folder)
       
       #black
       create_qmds_group_reg(file.path(".","qmd","templates",
                                       "_regressions_demo_template_sl.qmd"),
                             "03-reg", "black", "Black",
                             qmd_folder,s3_folder)
       
       #hispanic
       create_qmds_group_reg(file.path(".","qmd","templates",
                                       "_regressions_demo_template_sl.qmd"),
                             "04-reg", "hispanic", "Hispanic",
                             qmd_folder,s3_folder)
       
       #multirace
       create_qmds_group_reg(file.path(".","qmd","templates",
                                       "_regressions_demo_template_sl.qmd"),
                             "05-reg", "multirace", "Multirace",
                             qmd_folder,s3_folder)
       
       #female
       create_qmds_group_reg(file.path(".","qmd","templates",
                                       "_regressions_demo_template_sl.qmd"),
                             "06-reg", "female", "Female",
                             qmd_folder,s3_folder)
       
       #ec_dis_22_c - Economic Disadvantaged
       create_qmds_group_reg(file.path(".","qmd","templates",
                                       "_regressions_demo_template_sl.qmd"),
                             "07-reg", "ec_dis_22_c", "Economically Disadvantaged",
                             qmd_folder,s3_folder)
       
       #sp_ed_22_c - Disability
       create_qmds_group_reg(file.path(".","qmd","templates",
                                       "_regressions_demo_template_sl.qmd"),
                             "08-reg", "sp_ed_22_c", "Disability",
                             qmd_folder,s3_folder)
       
       #toc - Students of Color
       create_qmds_group_reg(file.path(".","qmd","templates",
                                       "_regressions_demo_template_sl.qmd"),
                             "09-reg", "toc", "Students of Color",
                             qmd_folder,s3_folder)
       
       #toc - White
       create_qmds_group_reg(file.path(".","qmd","templates",
                                       "_regressions_demo_template_sl.qmd"),
                             "10-reg", "white", "White",
                             qmd_folder,s3_folder)
       
     }
)
toc()

## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------