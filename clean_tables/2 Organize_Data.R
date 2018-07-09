library(dplyr)
setwd("~/UTK Projects/UTK Compare Peers/DataBase/Attempt4_Tables_based_on_Main_Concept/clean_tables/")

#### Year 2016
year = "2016"
year_av = "2015"


##----------------------------------------------------------------------------
##--------------------------
## Read-In Cost 
##--------------------------
## In-State Cost: Tuition | R&B | Other | COA Total (DONE, Have to repeat costs in excel) 
## Out-of-State Cost: Tuition | R&B | Other | COA Total (DONE, Have to repeat costs in excel) 
coa = read.csv("clean_costs_attendance.csv")
## Self Manipulation
coa_mod = coa %>%
  select(UnitID,contains(year)) %>%
  mutate(all_others_2016 = books_supplies_cost_2016 + oc_other_cost_2016 ) %>%
  select(-books_supplies_cost_2016, -oc_other_cost_2016) %>%
select("UnitID", "instate_tf_cost_2016","instate_coa_2016",
       "oostate_tf_cost_2016","oostate_coa_2016",include = character())
 

##----------------------------------------------------------------------------
##--------------------------
## Read-In Enrollment
##--------------------------
## UG Enrollment (DONE)
ug = read.csv("clean_ug_ftf_tran_race.csv")
ug_mod = ug %>%
  select(UnitID,contains(year)) %>%
  select(UnitID,contains("total")) %>%
  mutate(ftf_2016_percent = 100*(ftf_2016_grand_total / (ftf_2016_grand_total+trans_2016_grand_total))) %>%
  mutate(trans_2016_percent = 100*(trans_2016_grand_total / (ftf_2016_grand_total+trans_2016_grand_total)))

##----------------------------------------------------------------------------
##--------------------------
## Read-In Student Financial aid
##--------------------------
## Pell UG (DONE)
## Pell FTF (DONE)
fina = read.csv("clean_student_financial_aid.csv")
fina_mod = fina %>%
  select(UnitID, contains(year_av))
##----------------------------------------------------------------------------
##--------------------------
## Read-In Residence Information FTF
##--------------------------
## Geographical Diversity: %Res | %OOS | %Int (FTF)
resid = read.csv("clean_ftf_residence.csv")

resid_mod = resid %>%
  select(UnitID,contains(year)) %>%
  select(UnitID,contains("percent_instate"),
         contains("percent_oostate"),
         contains("percent_foreign"))
##----------------------------------------------------------------------------
## Admission: ACT-Midrange
##--------------------------
## Read-In Admission Information
##--------------------------
stuad = read.csv("clean_admission.csv")
stuad_mod = stuad %>%
  select(UnitID,contains(year)) %>%
  select(UnitID,contains("Mid"))





##----------------------------------------------------------------------------
## Diversity: %Minority | URM (DONE)

div_mod = ug %>%
  select(UnitID,contains(year)) %>%
  select(UnitID,contains("percent")) %>%
  select(-contains("trans")) 






##----------------------------------------------------------------------------
## Student Success: 1st year Ret | 6yr Grad Rate
##--------------------------
## Read-In Outcome Information
##--------------------------
stuo = read.csv("clean_student_outcome.csv")
stuo_mod = stuo %>%
  select(UnitID,contains(year)) %>%
  select("UnitID","retenratt_1styr_2016","gradrate_6yr_2016")







##----------------------------------------------------------------------------
## School Info
sch_info = read.csv("school_info.csv")
##----------------------------------------------------------------------------
##--------------------------
## Combine all tables
##--------------------------
## Gather imp schols
setwd("~/UTK Projects/UTK Compare Peers/DataBase/Attempt4_Tables_based_on_Main_Concept/Manipulations_2_ASP")
ids = read.csv("current_imp_schoolsid.csv")
## Put all information together
info_all = inner_join(coa_mod, ug_mod, by='UnitID') %>%
  inner_join(.,stuad_mod,by="UnitID") %>%
  inner_join(., div_mod, by='UnitID') %>%
  inner_join(., stuo_mod, by='UnitID') %>%
  inner_join(., resid_mod, by='UnitID') %>%
    inner_join(., fina_mod, by='UnitID') %>%
  inner_join(., sch_info, by='UnitID') %>%
  select(UnitID,Institution.Name,Name.Abv,everything()) %>%
  filter(UnitID %in% ids[,1])


write.csv(info_all,"combine_info.csv",row.names = F)
