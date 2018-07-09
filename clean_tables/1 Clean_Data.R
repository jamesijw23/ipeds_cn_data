library(dplyr)     ## Manipulate Data Frames
library(stringr)   ## String Search and Manipulate (replace)
library(stringi)   ## String Search and Manipulate (extract years)
library(gtools)    ## Order Numbers in strings


##-----------------------------------------------------------
##
## A. admission table
##
##-----------------------------------------------------------

##---------------------------------------
##
## 1) Read-In Data
##
##---------------------------------------
setwd("~/UTK Projects/UTK Compare Peers/DataBase/Attempt4_Tables_based_on_Main_Concept/raw_data")
admission_info = read.csv("raw_admission.csv",check.names = F)
names_adm = colnames(admission_info)



##---------------------------------------
##
## 2) Clean
##
##---------------------------------------
## Remove non-important aspect of strings
## Note: For str_replace, use 1 pattern at a time
## 1.1 All the patterns to remove
non_imp_string_info_adm1 = c(" Composite ")
non_imp_string_info_adm2 = c(" percentile score ")
non_imp_string_info_adm3 = c("\\(ADM")
non_imp_string_info_adm4 = c("\\(IC")
non_imp_string_info_adm5 = c("\\)")

tmp_colname_adm1 = str_replace(string = names_adm,
                               pattern = non_imp_string_info_adm1,
                               replacement = "_" )
tmp_colname_adm2 = str_replace(string = tmp_colname_adm1, 
                               pattern = non_imp_string_info_adm2,
                               replacement = "_" )
tmp_colname_adm3 = str_replace(string = tmp_colname_adm2,
                               pattern = non_imp_string_info_adm3,
                               replacement = "")
tmp_colname_adm4 = str_replace(string = tmp_colname_adm3,
                               pattern = non_imp_string_info_adm4,
                               replacement = "")
tmp_colname_adm5 = str_replace(string = tmp_colname_adm4,
                               pattern = non_imp_string_info_adm5,
                               replacement = "")

## Change data frame colnames
colnames(admission_info) = tmp_colname_adm5
##---------------------------------------
##
## 3) Manipulate
##
##---------------------------------------

## Remove UnitID and Institution Name
imp_columns_adm = colnames(admission_info)[-c(1,2)]
non_imp_string_info_adm6 = c("ACT_25th_")
non_imp_string_info_adm7 = c("ACT_75th_")

## Obtain only years from colnames
years_adm6 = str_replace(string = imp_columns_adm,
                         pattern = non_imp_string_info_adm6,
                         replacement = "")
years_adm7 = str_replace(string = years_adm6,
                         pattern = non_imp_string_info_adm7,
                         replacement = "")
years_adm = as.matrix(unique(years_adm7))

##-----------------
## Name: find_midrange
## Input: Year as character
## Output: midrange for all schools
## Purpose: Find MidRange Function from Q25 and A75
##-----------------
find_midrange = function(tmp_year){
  mid_range = admission_info %>%
    select(contains(tmp_year)) %>%
    rowMeans(na.rm = T)
  return(mid_range)
}
## Apply function to years to create df
df_midrange_adm = data.frame(apply(years_adm,1,find_midrange))
## Create Columns Names
colnames(df_midrange_adm) = paste("ACT_MidRange_",years_adm,sep="")


## Add MidRange to Originally Data Set
admission_info_new = data.frame(admission_info,
                                df_midrange_adm,
                                check.names = F)

##---------------------------------------
##
## 4) Put into clean_table Folder
##
##---------------------------------------
setwd("~/UTK Projects/UTK Compare Peers/DataBase/Attempt4_Tables_based_on_Main_Concept/clean_tables/")
write.csv(admission_info_new,"clean_admission.csv",row.names = F)


























##-----------------------------------------------------------
##
## B. Cost of Attendence table
##
##-----------------------------------------------------------

##---------------------------------------
##
## 1) Read-In Data
##
##---------------------------------------
setwd("~/UTK Projects/UTK Compare Peers/DataBase/Attempt4_Tables_based_on_Main_Concept/raw_data")
coa_info = read.csv("raw_costs_attendance.csv",check.names = F)
names_coa = colnames(coa_info)
imp_cn =names_coa[c(1,2)]


##---------------------------------------
##
## 2) Clean
##
##---------------------------------------
## Remove non-important aspect of strings
## Note: For str_replace, use 1 pattern at a time
## 2.1 All the patterns to remove
old_string_info_coa1 = "Published in-state tuition and fees "
replace_string_info_coa1 = "instate_tf_cost"

old_string_info_coa2 = "Published out-of-state tuition and fees "
replace_string_info_coa2 = "oostate_tf_cost"

old_string_info_coa3 = "On campus  room and board "
replace_string_info_coa3 = "oc_room_and_board_cost"

old_string_info_coa4 = "Books and supplies "
replace_string_info_coa4 = "books_supplies_cost"

old_string_info_coa5 = "On campus  other expenses "
replace_string_info_coa5 = "oc_other_cost"

## Obtain years from all column names
years_coa_tmp = stri_extract_last_regex(names_coa, "\\d{4}")

years_coa_tmp[c(1,2)] = ""


names_coa1 = str_replace(string = names_coa,
                         pattern = old_string_info_coa1,
                         replacement = replace_string_info_coa1)
names_coa2 = str_replace(string = names_coa1,
                         pattern = old_string_info_coa2,
                         replacement = replace_string_info_coa2)
names_coa3 = str_replace(string = names_coa2,
                         pattern = old_string_info_coa3,
                         replacement = replace_string_info_coa3)
names_coa4 = str_replace(string = names_coa3,
                         pattern = old_string_info_coa4,
                         replacement = replace_string_info_coa4)
names_coa5 = str_replace(string = names_coa4,
                         pattern = old_string_info_coa5,
                         replacement = replace_string_info_coa5)


## Replace after a cost (AMAZING)
names_coa6 = str_replace(string = names_coa5,
                         pattern = "[^cost]*$",
                         replacement = "")

## Combine names with years
names_coa7 = paste(names_coa6,"_",years_coa_tmp,sep="")


## Change data frame colnames
colnames(coa_info) = names_coa7
colnames(coa_info)[c(1,2)] = imp_cn
##---------------------------------------
##
## 3) Manipulate
##
##---------------------------------------

## Get Years
years_coa = unique(years_coa_tmp[-c(1,2)])


##-----------------
## Name: find_coa
## Input: Year as character
## Output:  oos and ins coa for all schools
## Purpose: Find Out of State and In state Cost of Attendance 
##-----------------
find_coa = function(tmp_year){
  instate_coa = coa_info %>%
    select(contains(tmp_year)) %>%
    select(-contains("oostate_tf_cost")) %>%
    rowSums(na.rm = T)
  
  oostate_coa = coa_info %>%
    select(contains(tmp_year)) %>%
    select(-contains("instate_tf_cost")) %>%
    rowSums(na.rm = T)
  tmp_coa = data.frame(instate_coa,oostate_coa)
  colnames(tmp_coa) = paste(colnames(tmp_coa),"_",tmp_year,sep="")
  return(tmp_coa)
}





## Apply function to years to create df
df_coa_adm = data.frame(apply(as.matrix(years_coa),1,find_coa))



## Add MidRange to Originally Data Set
coa_info_new = data.frame(coa_info,
                          df_coa_adm,
                          check.names = F)

##---------------------------------------
##
## 4) Put into clean_table Folder
##
##---------------------------------------
setwd("~/UTK Projects/UTK Compare Peers/DataBase/Attempt4_Tables_based_on_Main_Concept/clean_tables/")
write.csv(coa_info_new,"clean_costs_attendance.csv",row.names = F)



















##-----------------------------------------------------------
##
## C. Undergraduate First Time Freshman and Transfer by race
##
##-----------------------------------------------------------

##---------------------------------------
##
## 1) Read-In Data
##
##---------------------------------------
setwd("~/UTK Projects/UTK Compare Peers/DataBase/Attempt4_Tables_based_on_Main_Concept/raw_data")
race_info = read.csv("raw_ug_ftf_tran_race.csv",check.names = F)
names_race = colnames(race_info)



##---------------------------------------
##
## 2) Clean
##
##---------------------------------------
## Obtain Years from names
nyears_race = stri_extract_last_regex(names_race, "\\d{4}")

## Determine: UG, First Time Freshman, Transfer
ug_name = "Undergraduate total"
ft_name = "First-time" 
tr_name = "Transfer-ins"
##-----------------
## Name: find_class
## Input: Column
## Output:  Classification of variable (what kind of students)
## Purpose: Find type of classification of column
##-----------------
find_class = function(x){
  if(str_detect(string = x,pattern = ug_name) == T){
    tmp_class = "ug"
  } else if(str_detect(string = x,pattern = ft_name) == T) {
    tmp_class = "ftf"
  }else if(str_detect(string = x,pattern = tr_name) == T) {
    tmp_class = "trans"
  } else {
    tmp_class = ""
  }
  return(tmp_class)
}
class_race = apply(as.matrix(names_race),1,find_class)



## Determine race of each column
gt_name = "Grand total"
ai_name = "American Indian" 
as_name = "Asian"
bl_name = "Black"
hs_name = "Hispanic"
ha_name = "Hawaiian"
wh_name = "White"
mr_name = "more races"
un_name = "unknown"
nr_name = "Nonresident"
##-----------------
## Name: find_race
## Input: Column
## Output:  Race of variable (what kind of students)
## Purpose: Find type of race of column
##-----------------
find_race = function(x){
  if(str_detect(string = x,pattern = gt_name) == T){
    tmp_race = "grand_total"
  } else if(str_detect(string = x,pattern = ai_name) == T) {
    tmp_race = "ai_an"
  }else if(str_detect(string = x,pattern = as_name) == T) {
    tmp_race = "asian"
  } else if(str_detect(string = x,pattern = bl_name) == T) {
    tmp_race = "black"
  }else if(str_detect(string = x,pattern = hs_name) == T) {
    tmp_race = "hispanic"
  }else if(str_detect(string = x,pattern = ha_name) == T) {
    tmp_race = "nh_pi"
  }else if(str_detect(string = x,pattern = wh_name) == T) {
    tmp_race = "white"
  }else if(str_detect(string = x,pattern = mr_name) == T) {
    tmp_race = "two_m_races"
  } else if(str_detect(string = x,pattern = un_name) == T) {
    tmp_race = "unknown"
  } else if(str_detect(string = x,pattern = nr_name) == T) {
    tmp_race = "non_resid"
  } else {
    tmp_race = ""
  }
  return(tmp_race)
}
actual_race = apply(as.matrix(names_race),1,find_race)

## Add al column information into 1 string
names_race1 = paste(class_race,"_",nyears_race,"_",actual_race,sep="")

## Change data frame colnames
colnames(race_info)[-c(1,2)]=names_race1[-c(1,2)]





##---------------------------------------
##
## 3) Manipulate
##
##---------------------------------------

## Unique Years
years_race = unique(nyears_race[-c(1,2)])


## Find URM and Minority for each year and type of student
##-----------------
## Name: find_min_urm
## Input: type of student (ie:ug,ftf,trans) and year
## Output:  Under represented minority percent (urm) nad Minority percent
## Purpose: Find type of race of column
##-----------------
find_min_urm = function(tmp_stud,tmp_year){
  ## Find Appropriate Coolumns in df
  cur_var = colnames(race_info)[str_detect(colnames(race_info),tmp_stud) &
                                  str_detect(colnames(race_info),tmp_year)]
  
  ## Find white column
  white_tmp_total = race_info %>%
    select(cur_var) %>%
    select(contains("white")) 
  
  ## Find blavk column
  black_tmp_total = race_info %>%
    select(cur_var) %>%
    select(contains("black")) 
  
  ## Find hispanic column
  hispanic_tmp_total = race_info %>%
    select(cur_var) %>%
    select(contains("hispanic")) 
  
  ## Find total column
  all_tmp_total = race_info %>%
    select(cur_var) %>%
    select(contains("total")) 
  
  ## Calculate urm %
  urm_percent = (hispanic_tmp_total + black_tmp_total) /all_tmp_total
  ## Calculate urm %
  minority_percent = 1-(white_tmp_total/all_tmp_total)  
  ## Combine and return df
  tmp_info = data.frame(urm_percent, minority_percent)
  
  colnames(tmp_info) = paste(c("urm_percent","minority_percent"),
                             "_",tmp_stud,"_",tmp_year,sep="") 
  return(tmp_info)
}

## Find the urm and minority percent for student type
ug_urm_min = as.data.frame(apply(as.matrix(years_race),1,find_min_urm,tmp_stud = "ug"))*100 
ftf_urm_min = as.data.frame(apply(as.matrix(years_race),1,find_min_urm,tmp_stud = "ftf"))*100 
trans_urm_min = as.data.frame(apply(as.matrix(years_race),1,find_min_urm,tmp_stud = "trans"))*100 

## Put all student types together
race_info_new = data.frame(race_info,ug_urm_min,ftf_urm_min,trans_urm_min)


##---------------------------------------
##
## 4) Put into clean_table Folder
##
##---------------------------------------
setwd("~/UTK Projects/UTK Compare Peers/DataBase/Attempt4_Tables_based_on_Main_Concept/clean_tables/")
write.csv(race_info_new,"clean_ug_ftf_tran_race.csv",row.names = F)









##-----------------------------------------------------------
##
## D. First Year Freshmen Residence
##
##-----------------------------------------------------------
##---------------------------------------
##
## 1) Read-In Data
##
##---------------------------------------
setwd("~/UTK Projects/UTK Compare Peers/DataBase/Attempt4_Tables_based_on_Main_Concept/raw_data")
resid_info = read.csv("raw_ftf_residence.csv",check.names = F)
names_resid = colnames(resid_info)

##---------------------------------------
##
## 2) Clean
##
##---------------------------------------
## Obtain Years
nyears_resid = stri_extract_last_regex(names_resid, "\\d{4}")

## Pattern to be removed
not_imp_string_resid1 = "First-time degree/certificate-seeking undergraduate students "
names_resid1 = str_replace(string = names_resid,
                           pattern = not_imp_string_resid1,
                           replacement = "")

## Pattern to replaced
not_imp_string_resid2 ="All first-time degree/certificate seeking undergraduates  total"
names_resid2 = str_replace(string = names_resid1,
                           pattern = not_imp_string_resid2,
                           replacement = "First_Time_Total")


## Remove First part of string
names_resid3 = sub(".*? (.+)", "\\1", names_resid2)

## Remove empty space in beginning of string
names_resid4 = trimws(names_resid3, which= "left")

## Remove parenthesis
names_resid5 = str_replace(string = names_resid4,
                           pattern="\\)",
                           replacement = "")


## Add Years back to column
names_resid6 = paste(names_resid5,"_",nyears_resid,sep="")
## Change Column Names
colnames(resid_info)[-c(1,2)] = names_resid6[-c(1,2)]


##---------------------------------------
##
## 3) Manipulate
##
##---------------------------------------


##----------------------
## Get schools states
setwd("~/UTK Projects/UTK Compare Peers/DataBase/Attempt4_Tables_based_on_Main_Concept/clean_tables/")
school_info = read.csv("school_info.csv")
resid_info1 = inner_join(x = resid_info,
                         y = school_info,
                         by ="UnitID") 
##----------------------
## Unique Years
years_resid = unique(nyears_resid[-c(1,2)])

##----------------------
##### Assume information for two below functions
##### Also the df: resid_info1 is known
## All important columns
all_imp_columns = c(datasets::state.name,"First_Time_Total","Foreign countries",
                    "District of Columbia")
## Input function as matrix
all_school_names = as.matrix(resid_info1$Institution.Name)
##----------------------




##-----------------
## Name: find_school_oos
## Input: school of interest and important variables (based on year)
## Output:  total ftf, total instate, total foreign, total oostate
## Purpose: Find out of state total
##-----------------
find_school_oos =function(tmp_school,cur_var){
  ## Find university information
  all_school_info_c = resid_info1 %>%
    select(cur_var) %>%
    filter(Institution.Name == tmp_school)
  
  ## Find state for university
  find_school_state = as.character(all_school_info_c$State)
  
  ## Choose approptiate columns for calculation
  all_sch_numbers = all_school_info_c %>%
    select(contains("First_Time_Total"),
           starts_with(find_school_state), 
           contains("Foreign countries"))
  
  colnames(all_sch_numbers) = NULL
  
  ## Calculate out of state
  oostate = as.numeric(all_sch_numbers[1]) - 
    (as.numeric(all_sch_numbers[2])
     + as.numeric(all_sch_numbers[3])) 
  
  school_nums = c(as.numeric(all_sch_numbers[1]),
    as.numeric(all_sch_numbers[2]),
    as.numeric(all_sch_numbers[3]),
    oostate)
  
  return(t(school_nums))
}


##-----------------
## Name: find_all_school_years
## Input: year
## Output:  total ftf, total/percet instate, total/percent foreign, total/percent oostate
## Purpose: Find totals and percents of ftt, instate, foreign, oostate
##-----------------
find_all_school_years = function(tmp_year){
  ## Get appropriate columns
  cur_var = c(colnames(resid_info1)[grepl(paste(all_imp_columns,collapse="|"), 
                                          colnames(resid_info1)) &
                                      str_detect(colnames(resid_info1),tmp_year)],
              "State","Institution.Name")
  
  ## Determine school ftf year information
  sch_ftf_tot = data.frame(t(apply(all_school_names,1,find_school_oos, cur_var = cur_var)))
  
  ## names Columns
  colnames(sch_ftf_tot) = c('total','instate','foreign','oostate')
  
  ## Find Percents
  sch_ftf_all = sch_ftf_tot %>%
    mutate(percent_instate = (instate/total)*100)%>%
    mutate(percent_foreign = (foreign/total)*100)%>%
    mutate(percent_oostate = (oostate/total)*100)
  
  ## Change Columns
  colnames(sch_ftf_all) = paste(colnames(sch_ftf_all),"_",tmp_year,sep="")
  return(sch_ftf_all)
}

resid_info_new = data.frame(resid_info1,
           as.data.frame(apply(as.matrix(years_resid), 1, find_all_school_years)))

##---------------------------------------
##
## 4) Put into clean_table Folder
##
##---------------------------------------
setwd("~/UTK Projects/UTK Compare Peers/DataBase/Attempt4_Tables_based_on_Main_Concept/clean_tables/")
write.csv(resid_info_new,"clean_ftf_residence.csv",row.names = F)





##-----------------------------------------------------------
##
## E. School Finances
##
##-----------------------------------------------------------

##---------------------------------------
##
## 1) Read-In Data
##
##---------------------------------------
setwd("~/UTK Projects/UTK Compare Peers/DataBase/Attempt4_Tables_based_on_Main_Concept/raw_data")
schf_info = read.csv("raw_ovr_school_finances.csv",check.names = F)
names_schf = colnames(schf_info)
##---------------------------------------
##
## 2) Clean
##
##---------------------------------------
## Patterns to remove and manipulate
not_imp_string_schf1 = "State appropriations \\("
replace_string_schf1 = "state_app"
not_imp_string_schf2 = "Tuition and fees  after deducting discounts and allowances \\("
replace_string_schf2 = "tf_revenue"
not_imp_string_schf3 = "\\)"

## Changing based on strings
names_schf1 = str_replace(string = names_schf,
                           pattern = not_imp_string_schf1,
                           replacement = replace_string_schf1)
names_schf2 = str_replace(string = names_schf1,
                          pattern = not_imp_string_schf2,
                          replacement = replace_string_schf2)
names_schf3 = str_replace(string = names_schf2,
                          pattern = not_imp_string_schf3,
                          replacement = "")
## Change years (Brute Force)
names_schf_t = names_schf3

repl_x = c("1516_F1A","1415_F1A","1314_F1A",
           "1213_F1A","1112_F1A","1011_F1A") ## based on the selection of years
repl_y = paste("_201",seq(5,0),sep="") ## to not write out all years

for(i in 1:length(repl_x)){
names_schf_t = str_replace(string = names_schf_t,
                          pattern = repl_x[i],
                          replacement = repl_y[i])
}
colnames(schf_info) = names_schf_t
##---------------------------------------
##
## 3) Manipulate (NONE NEEDED)
##
##---------------------------------------


##---------------------------------------
##
## 4) Put into clean_table Folder
##
##---------------------------------------
setwd("~/UTK Projects/UTK Compare Peers/DataBase/Attempt4_Tables_based_on_Main_Concept/clean_tables/")
write.csv(schf_info,"clean_ovr_school_finances.csv",row.names = F)





##-----------------------------------------------------------
##
## F. Financial Aid Students
##
##-----------------------------------------------------------

##---------------------------------------
##
## 1) Read-In Data
##
##---------------------------------------
setwd("~/UTK Projects/UTK Compare Peers/DataBase/Attempt4_Tables_based_on_Main_Concept/raw_data")
fina_info = read.csv("raw_student_financial_aid.csv",check.names = F)
names_fina = colnames(fina_info)
##---------------------------------------
##
## 2) Clean
##
##---------------------------------------
## Patterns to remove and manipulate
not_imp_string_fina1 ="Percent of full-time first-time undergraduates awarded Pell grants \\("
replace_string_fina1 = "percent_ftf_pell_"
not_imp_string_fina2 ="Percent of undergraduate students awarded Pell grants \\("
replace_string_fina2 = "percent_ug_pell_"
not_imp_string_fina3 = "\\)"

## Changing based on strings
names_fina1 = str_replace(string = names_fina,
                          pattern = not_imp_string_fina1,
                          replacement = replace_string_fina1)
names_fina2 = str_replace(string = names_fina1,
                          pattern = not_imp_string_fina2,
                          replacement = replace_string_fina2)
names_fina3 = str_replace(string = names_fina2,
                          pattern = not_imp_string_fina3,
                          replacement = "")

## Change years (Brute Force)
names_fina_t = names_fina3

repl_x = c("SFA1516","SFA1415","SFA1314",
           "SFA1213","SFA1112","SFA1011") ## based on the selection of years
repl_y = paste("201",seq(5,0),sep="") ## to not write out all years

for(i in 1:length(repl_x)){
  names_fina_t = str_replace(string = names_fina_t,
                             pattern = repl_x[i],
                             replacement = repl_y[i])
}
colnames(fina_info) = names_fina_t
##---------------------------------------
##
## 3) Manipulate (NONE NEEDED)
##
##---------------------------------------


##---------------------------------------
##
## 4) Put into clean_table Folder
##
##---------------------------------------
setwd("~/UTK Projects/UTK Compare Peers/DataBase/Attempt4_Tables_based_on_Main_Concept/clean_tables/")
write.csv(fina_info,"clean_student_financial_aid.csv",row.names = F)









##-----------------------------------------------------------
##
## G. Student Outcome
##
##-----------------------------------------------------------

##---------------------------------------
##
## 1) Read-In Data
##
##---------------------------------------
setwd("~/UTK Projects/UTK Compare Peers/DataBase/Attempt4_Tables_based_on_Main_Concept/raw_data")
stuo_info = read.csv("raw_student_outcome.csv",check.names = F)
names_stuo = colnames(stuo_info)
##---------------------------------------
##
## 2) Clean
##
##---------------------------------------
## Patterns to remove and manipulate
not_imp_string_stuo1 ="6-year Graduation rate - bachelor's degree within 150% of normal time \\("
not_imp_string_stuo1a ="Graduation rate - bachelor's degree within 150\\% of normal time \\(6-years\\) \\(GR200"
replace_string_stuo1 = "gradrate_6yr_"
not_imp_string_stuo2 ="Full-time retention rate "
replace_string_stuo2 = "retenratt_1styr_fulltime"
not_imp_string_stuo3 = "\\)"


## Obtain Years
nyears_stuo = stri_extract_last_regex(names_stuo, "\\d{4}")
years_stuo = nyears_stuo[is.na(nyears_stuo)==F]


## Change years (Brute Force/Manual)
## Lazy

yn = sort(rep(years_stuo,2),decreasing = T)
sn =  rep(c("gradrate_6yr_" ,"retenratt_1styr_"),length(years_stuo))

names_stuo_t = paste(sn,yn,sep="")
colnames(stuo_info)[-c(1,2)] = names_stuo_t 
##---------------------------------------
##
## 3) Manipulate (NONE NEEDED)
##
##---------------------------------------


##---------------------------------------
##
## 4) Put into clean_table Folder
##
##---------------------------------------
setwd("~/UTK Projects/UTK Compare Peers/DataBase/Attempt4_Tables_based_on_Main_Concept/clean_tables/")
write.csv(stuo_info,"clean_student_outcome.csv",row.names = F)









