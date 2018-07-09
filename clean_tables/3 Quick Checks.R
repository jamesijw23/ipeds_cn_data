library(dplyr)
library(ggplot2)

## Gather Data
resid_table = read.csv("clean_ftf_residence.csv")

## Check column names for proper variables names
#colnames(resid_table)
## Obtain only 2016 data and OOS, INT, Res
oosins = resid_table %>%
  select(UnitID,Institution.Name.1,percent_instate_2016,
         percent_oostate_2016,
         percent_foreign_2016) %>%
  filter(UnitID == 220862)


data_16 = resid_table %>%
  select(UnitID,contains("2016")) %>%
  select(UnitID,Foreign.countries_2016,First_Time_Total_2016,
         Virginia_2016) %>%
  filter(UnitID ==233921 )
