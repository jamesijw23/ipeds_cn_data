# ipeds_cn_data
Gathering data in a formidable fashon 



Information and columns need to be calculated Gathered from IPEDS
##-----------------------------------------------------
## 3.) Organization of Tables
##-----------------------------------------------------

Years since 2010
Tables:
1. school_info (look up table, I created this table)
   Rows: Schools
   Columns: Abbreviations, State, City(1*), PEER, ASP, In-State, SEC, Public, Private
   Time: NA

2. admission
   Rows: Schools
   Columns: act_com_25, act_com_75, act_com_cM (5*)
   Time: 1617 to 0910 (IPEDs change name from IC to ADM)
 
3. ug_ftf_tran_race (2*) (6*) 
   Rows: Schools
   Columns: total, total_ai_an, total_asian, total_black, total_hip, total_hispanic, total_white, total_unknown, total_tmr, total_nonres
   Time: 16 to 10
Note: total should = UG Head Count
Note: total should = ftf Head Count

4. ovr_school_finances
   Rows: Schools
   Columns: state_approp_rev, tuition_fees_rev 
   Time: 1617 to 1011

5. costs_attendance
   Rows: Schools
   Columns: instate_tf, oostate_tf,room_board, others_costs, books_supplies, total_cost (4*)
   Time: 1718 to 1011

6. ftf_residence (3*)
   Rows: Schools
   Columns: oostate_total, instate_total, foreign_total, transfer_total 
   Time: 16, 14, 12 (15, 13, 11 is available but not included)

7. student_financial_aid
   Rows: Schools
   Columns: pell_per_ug, pell_per_ftf 
   Time: 1516 - 1011

8. student_outcome
   Rows: Schools
   Columns: 6_year_retention, first_year_retention
   Time: 16 to 10 (verify 11 and 12, names look different)

