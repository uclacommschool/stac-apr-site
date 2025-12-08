################################################################################
##
## [ PROJ ] < APR Website >
## [ FILE ] < 00 - Create Create Nested List.R >
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
library(data.table)
library(janitor)
library(readxl)
## ---------------------------
## directory paths
## ---------------------------

#see current directory
getwd()

#create file directories
data_dir<-"C:/Users/jyo/Box/UCLA Center for Community Schooling - STAC/APR/CDE Database/Raw Datasets"

export_dir<-"C:/Users/jyo/Box/UCLA Center for Community Schooling - STAC/Impact Study/datasets"

apr_dir<-"C:/Users/jyo/Box/UCLA Center for Community Schooling - STAC/APR"

## ---------------------------
## helper functions & strings
## ---------------------------


## ---------------------------
## load & inspect data
## ---------------------------

apr_data<-fread(file.path(apr_dir, "APR Analysis", "2025",
                          "APR_2425_updated.csv"))

imp_data<-read_excel(file.path(data_dir,
                          "CCSPP_Implementation Site Data-9-12-25.xlsx"),
                     sheet = "School Sites")


## -----------------------------------------------------------------------------
## Part 1 - Clean Data & Create Nested Lists
## -----------------------------------------------------------------------------

apr_data<-clean_names(apr_data)
imp_data<-clean_names(imp_data)


df_split <- apr_data %>%
  mutate(

    split_match = str_match(qid143_4, "(.*)\\s-\\s([\\d\\s]+)$"),
    
    # Extract the captured groups from the match matrix (Column 1 is the full match)
    school_name = split_match[, 2],
    cds_code = split_match[, 3],
    
    # Clean up the intermediate column
    split_match = NULL
  ) 

df_split <- 
  df_split %>%
  mutate(
    # Split on one-or-more spaces into exactly 3 parts
    parts = str_split_fixed(cds_code, "\\s+", 3),
    county_code   = parts[, 1],
    district_code = parts[, 2],
    school_code   = parts[, 3]
  ) %>%
  select(-parts)

df_split<-df_split %>% select(school_name, cds_code, county_code,
                              district_code, school_code, everything())

# test3<-df_split %>% select()

count_df<-imp_data %>% select(fa_county_code, fa_county) %>% unique()
lea_df<-imp_data %>% select(ss_district_code, s_ss_district) %>% unique()

#merge numbers together
test<-left_join(df_split, imp_data, by = c("cds_code" = "ss_cds_code"))

test<-test %>% select(school_name, cds_code, county_code,
                      district_code, school_code, ss_ccspp_region,
                      ss_county, s_ss_district, everything()) 

#update Monroe Elementary and KIPP Sol Academy
test<-test %>% mutate(
  ss_county = case_when(
    school_name == "Monroe Elementary" ~ "Fresno",
    school_name == "KIPP Sol Academy" ~ "Los Angeles",
    TRUE ~ ss_county),
  ss_ccspp_region = case_when(
    school_name == "Monroe Elementary" ~ "Central Valley",
    school_name == "KIPP Sol Academy" ~ "Greater Los Angeles",
    TRUE ~ ss_ccspp_region),
  s_ss_district = case_when(
    school_name == "Monroe Elementary" ~ "Caruthers Unified",
    school_name == "KIPP Sol Academy" ~ "Los Angeles Unified",
    TRUE ~ s_ss_district)
)

## -----------------------------------------------------------------------------
## Part 2 - Create Nested List
## -----------------------------------------------------------------------------

#region list
region_list<- split(test, test$ss_ccspp_region)

#county list
county_list <- lapply(region_list, function(x) {
  split(x, x$ss_county)
})

#district_list
district_list <- lapply(county_list, function(y) {
  lapply(y, function(z) {
    split(z, z$s_ss_district)
  })
})

site_list <- lapply(district_list, function(a) {
    lapply(a, function(b) {
      lapply(b, function(c) {
        split(c, c$school_name)
      })})})

## -----------------------------------------------------------------------------
## Part 3 - Create Region, County, and District Names
## -----------------------------------------------------------------------------

region_names<-unique(apr_data$qid143_1)

## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------