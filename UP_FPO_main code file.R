
###############

  # Paper title: Association of membership in a farmer producer organization with crop diversity, household income, diet diversity, and women’s empowerment in Uttar Pradesh, India
  # Created date: 29 May 2023
  # Last update:  19 January 2025
  # Author: Nishmeet Singh 
  
###############  
  
    # Clear R Environment
    rm(list=ls())
    
    # Check if the packages that we need are installed
    want = c("tidyverse", "haven", "writexl","readxl","labelled","gtsummary","janitor","magrittr","ggrepel","margins","emmeans","marginaleffects","ggeffects",
             "Hmisc", "ROCR", "gridExtra", "pander", "reshape2", "lazyeval", "moments", "entropy","funModeling","twopartm","flextable","zoo","bstfun")
    have = want %in% rownames(installed.packages())
    
    # Install the packages that we miss
    if ( any(!have) ) { install.packages( want[!have] ) }
    
    # Load the packages
    junk <- lapply(want, library, character.only = T)
    
    # Remove the temporary objects we created
    rm(have, want, junk)
    
    
    
#  Raw Data Import ####
    
    #Set Work Directory
    setwd('@update with path')
    
    # Import
    df.raw<-read_xlsx("01 Data/FPO_Sample data set-06th June.xlsx")
  
    
    
# Data Processing ####
    
    # Rename variables using imported dictionary file
    new_names<-read_xlsx("05 Analysis//Data_quicksummary19062023.xlsx", sheet="Sheet1")
    str(new_names)
  
      # Create a vector of new variable names
      new_nm <- new_names$newvar
      
      # Rename variables in the original data frame
      names(df.raw) <- new_nm
    
    # Keep only eligible respondents 
    df.clean<- df.raw %>% filter(a_willing==1) %>% mutate(across(c(a_hhid,a_resp_id),as.factor)) #1609 observations
    
    # Count households with 1 or 2 respondents
    #df.clean %>% group_by(a_hhid) %>% summarise(n=n()) %>% ungroup() %>% count(n) %>% ungroup()
    #df.one_hh<-df.clean %>% group_by(a_hhid) %>% mutate(n=n()) %>% filter(n==1) 
    #790 household with 2 respondents and 29 households with only 1 respondent

    # Recoding 'NO' responses from 2 to 0
    df.clean<-df.clean %>% mutate(across(c(a_sex,fpo_ap_q35,fpo_a_ap_q37_4,fpo_ap_q38,fpo_ap_q39_o1,fpo_ap_q39_o4,fpo_ap_q40,fpo_a_ap_q41_4,fpo_ap_q42,fpo_ap_q43_o2,
                                           he_q2_cult,he_q4_liv,he_q6_othag,he_q8_nonag,he_q10_wage,he_q12_sal,dt_rice:dt_burger), .fns= ~ifelse(.==2,0,ifelse(.==0,0,1))))
    # Change to factor variables
   df.clean %<>% 
      mutate(a_fpo_hh=as.factor(a_fpo_hh))
   
   df.clean$a_fpo_hh <-fct_recode(df.clean$a_fpo_hh,"FPO household"="1","Non-FPO household"="2")
    
     
  ## Demographics ####
    
    # Change to factor variables
    df.demo<- df.clean %>% 
      select(a_hhid,a_resp_id,a_fpo_hh,a_sex,a_marr:a_caste,a_age,he_q1_hhmem) %>%
      mutate(across(c(a_hhid,a_resp_id,a_fpo_hh,a_sex,a_marr:a_caste),as.factor))

    # Recoding and creating factors for variables
    df.demo<- df.demo %>% mutate(
                          a_sex=fct_recode(a_sex,"Female"="0","Male"="1"),
                          a_caste=as.factor(ifelse(a_caste==9,NA,a_caste)),
                          a_marr=fct_recode(a_marr,"Currently married"="1","Widowed"="2","Never Married"="4"),
                          a_resp_rel=fct_recode(a_resp_rel,
                                                 "Spouse"="1","Parents"="2","Children"="3",
                                                 "mother/father in-law"="4","son/daughter in-law"="5",
                                                 "Other"="6"),
                          a_educ=fct_collapse(a_educ,`No formal school (including illiterate)`=c("1","2"),
                                               "Primary, complete (grade 1-4)"="3",
                                               "High school, complete (grade 5-9)"="4",
                                               `Secondary school, complete`= "5",
                                               `Graduate and above (including diploma)`=c("6","7")),
                          a_caste=fct_collapse
                          (a_caste,"Scheduled Caste/Tribe (SC/ST)" = c("1","2"),
                             # "Scheduled Tribe (ST)" = "2",
                             "Other Backward Caste (OBC)" = c("3","5"),
                             "General/Other" = c("4","6")
                             # "Muslim (OBC)" = "5",
                             # "Muslim (General)" = "6")
                           ))
    
    # Count of unique households 
    df.demo <- df.demo %<>% group_by(a_hhid,a_fpo_hh) %>% mutate(hh= ifelse(row_number() == 1,1,NA)) %>% ungroup()
    df.demo %<>% mutate(fpohh= ifelse(a_fpo_hh=="Non-FPO household" & hh==1,2,hh))
    
    table(df.demo$fpohh, df.demo$a_fpo_hh)
    #417 FPO households and 402 control households
    
  ## Agriculture ####  
    df.agri<- df.clean %>% select(a_hhid,a_fpo_hh,a_resp_id,starts_with("ag_"))
    
  ### land cultivated ####
    
    # area conversion 
    df.agri %<>% 
      mutate(ag_land_cent= ag_t_ap_q1_2*0.01, #1cent=0.01acre
             ag_land_bga= ag_t_ap_q1_3*0.4, #1bg=0.4 acre
             ag_land_biswa= ag_t_ap_q1_4*0.02, #1biswa= 0.02 acre (1bg=20bis)
             ag_land_kh_acr=ag_t_ap_q1_1+ag_land_cent+ag_land_bga+ag_land_biswa,
             ag_land_kh_hc=ag_land_kh_acr*0.4046,
             
             ag_land_cent_rb= ag_t_ap_q4_2*0.01, #1cent=0.01acre
             ag_land_bga_rb= ag_t_ap_q4_3*0.4, #1bg=0.4 acre
             ag_land_biswa_rb= ag_t_ap_q4_4*0.02, #1biswa= 0.02 acre (1bg=20bis)
             ag_land_rb_acr=ag_t_ap_q4_1+ag_land_cent_rb+ag_land_bga_rb+ag_land_biswa_rb,
             ag_land_rb_hc=ag_land_rb_acr*0.4046)
    
    # replace for people who said same land as Kharif
    df.agri %<>% mutate(ag_land_rb_acr=ifelse(ag_ap_q3==1,ag_land_kh_acr,ag_land_rb_acr),
                        ag_land_rb_hc=ifelse(ag_ap_q3==1,ag_land_kh_hc,ag_land_rb_hc))
    
    # total land (kharif and rabi)
    df.agri %<>% rowwise %>% mutate(ag_total_land_acr=ag_land_kh_acr+ag_land_rb_acr,
                                    ag_total_land_hc=ag_land_rb_hc+ag_land_kh_hc)
    
    df.agri %<>% mutate(land_cat= as_factor(ifelse(ag_land_kh_hc>0 & ag_land_kh_hc<=2,"1.small",
                                         ifelse(ag_land_kh_hc>2 & ag_land_kh_hc<=4,"2.medium",
                                         ifelse(ag_land_kh_hc>4,"3.large",
                                         "0.landless")))))
  
    df.agri$land_cat<- fct_relevel(df.agri$land_cat,"0.landless","1.small","2.medium","3.large")
    
  ### land owned ####
    df.landown<- df.clean %>% select(a_hhid,a_resp_id,we_part_b_q1,we_t_part_b_q1a_1:we_t_part_b_q1a_4)
    df.landown %<>% mutate(land_own=ifelse(we_part_b_q1<5,1,0)) 

    df.landown %<>% 
      mutate(ag_land_cent= we_t_part_b_q1a_2*0.01, #1cent=0.01acre
             ag_land_bga= we_t_part_b_q1a_3*0.4, #1bg=0.4 acre
             ag_land_biswa= we_t_part_b_q1a_4*0.02, #1biswa= 0.02 acre (1bg=20bis)
             ag_landown_acr=we_t_part_b_q1a_1+ag_land_cent+ag_land_bga+ag_land_biswa,
             ag_landown_hc=ag_landown_acr*0.4046)
    df.landown %<>% 
      select(a_hhid,a_resp_id,ag_landown_acr,ag_landown_hc)
    
  ### crop diversity ####
    
    # total number of crops 
    df.agri %<>% rowwise() %>% mutate(ag_total_crops=sum(ag_ap_q2,ag_ap_q5,na.rm=TRUE)) %>% 
      ungroup()
    
    df.agri %<>% mutate(ag_total_crops=ifelse(is.na(ag_ap_q2) &is.na(ag_ap_q5),NA,ag_total_crops))
    
    # recoding other crops
    must<-c("lahi","Lahi","lahi sarso","lahi(sarsho)","sarao","sarsho","sarsi","sarso","sarso in")
    ggram<-c("mirch,moong daal","mung")
    veg<-c("aloo","karbi","mirch,moong daal","mirch","mircha","morcha")
    maize<-c("makka")
    chilly<-c("mirch,moong daal","mirch","mircha","morcha")
    
    df.agri %<>% mutate(mustard=ifelse(ag_s_ap_q6_28 %in% must,1,0),
                      ag_a_ap_q6_14=ifelse(ag_s_ap_q6_28 %in% ggram,1,ag_a_ap_q6_14),
                      ag_a_ap_q6_22=ifelse(ag_s_ap_q6_28 %in% veg,1,ag_a_ap_q6_22),
                      ag_a_ap_q6_1=ifelse(ag_s_ap_q6_28 %in% maize,1,ag_a_ap_q6_1),
                      ag_a_ap_q8_1=ifelse(ag_s_ap_q6_28 %in% chilly,1,ag_a_ap_q8_1))
    
    # crop labels
    crop_label=c("a_fpo_hh","Maize", "Paddy", "Wheat","Jowar", "Foxtail millet",
                 "Finger millet",	"Little millet",	"Kodo millet", 	"Proso millet", "Barnyard millet",
                 "Pearl millet",	"Redgram", 	"Blackgram", 	"Green Gram",	"Chickpea", 	"Cowpea", 	"Groundnut",	"Sunflower",	"Castor",	"Sesame",	"Fruit",	"Vegetable",	"Flower crop", 	"Cotton",
                 "Sugarcane", 	"Coriander",	"Mushroom",	"Mixed Vegetables","Mustard")

  ### crop-wise ####
    
    #### cultivated land ####
    df.cropland<- df.agri %>% select(a_hhid,a_fpo_hh,"ag_maize_acres-t_ap_q12_1":"ag_jowar_biswa-t_ap_q12_4",
                             "ag_barnyard millet_acres-t_ap_q12_1":"ag_chickpea_biswa-t_ap_q12_4",
                             "ag_finger millet_acres-t_ap_q12_1":"ag_finger millet_biswa-t_ap_q12_4",
                             "ag_sesame_acres-t_ap_q12_1":"ag_sesame_biswa-t_ap_q12_4",
                             "ag_vegetable (specify)_acres-t_ap_q12_1":"ag_vegetable (specify)_biswa-t_ap_q12_4",
                             "ag_sugarcane_acres-t_ap_q12_1":"ag_sugarcane_biswa-t_ap_q12_4",
                             "ag_coriander_acres-t_ap_q12_1","ag_coriander_biswa-t_ap_q12_4",
                             "ag_mixed vegetables_acres-t_ap_q12_1":"ag_mixed vegetables_biswa-t_ap_q12_4",
                             "ag_red chili_acres-t_ap_q12_1":"ag_red chili_biswa-t_ap_q12_4",
                             "ag_tomato_acres-t_ap_q12_1":"ag_tomato_biswa-t_ap_q12_4",
                             "ag_lemon_acres-t_ap_q12_1":"ag_lemon_biswa-t_ap_q12_4",
                             "ag_bottle gourd_acres-t_ap_q12_1":"ag_bottle gourd_biswa-t_ap_q12_4",
                             "ag_pumpkin_acres-t_ap_q12_1":"ag_pumpkin_biswa-t_ap_q12_4",
                             "ag_green peas_acres-t_ap_q12_1":"ag_green peas_biswa-t_ap_q12_4")
    
    # pivot data to long format
    data1 <- df.cropland %>% 
      pivot_longer(!c(a_fpo_hh,a_hhid),
                   names_to = "crop_unit",
                   values_to = "area")
    
    data1 %<>% 
      mutate(total_area_acr= case_when(
        grepl("_acres",crop_unit)  ~ area,
        grepl("_cents",crop_unit)  ~ area*0.01,
        grepl("_bigha",crop_unit)  ~ area*0.4,
        grepl("_biswa",crop_unit)  ~ area*0.02,
        TRUE~ area))
    
    data1 %<>% mutate(crop_name=str_extract(crop_unit, "(?<=_)[^_]+"))
    
    # sum up total crop area
    data2 <-data1 %>% 
      group_by(a_hhid,crop_name,a_fpo_hh) %>% 
      dplyr::summarize(total_area=ifelse(all(is.na(total_area_acr)), NA, sum(total_area_acr, na.rm = TRUE)),.groups = "keep")
    data2 %<>% mutate(total_area=ifelse(total_area==0,NA,total_area)) 
    
    # wide format
    df.cropland<-data2 %>% pivot_wider(names_from = crop_name, values_from = total_area) %>% 
      ungroup()
    
    #### crop yield - paddy and wheat ####
    crop_name<-unique(data2$crop_name)
    
    df.cropyield <- df.agri %>% select(a_fpo_hh,a_hhid,((ends_with("_q15") | ends_with("_q16") |ends_with("_q17") | ends_with("_q18")) & contains(crop_name)))
    
    df.cropyield <- df.cropyield %>% 
      pivot_longer(cols=!c(a_fpo_hh,a_hhid),
                   names_to = c("crop", ".value"),
                   names_pattern = "^(ag_[a-z]+_ap)_(q\\d+)",
                   values_drop_na = TRUE)
  
    df.cropyield %<>% mutate(crop_name=str_extract(crop, "(?<=_)[^_]+")) %>% 
      filter(crop_name=="wheat"|crop_name=="paddy")

    # calculate yield (kg/ha), converting non-standardized responses
    
    # join with land area for per acre
    df.cropyield %<>% inner_join(data2, by=c("a_hhid","a_fpo_hh","crop_name")) %>% 
      filter(!is.na(total_area)) %>% # Dropped one observation where area was missing so yield cannot be calculated
      mutate(total_area_hc=total_area*0.4046)
    
    # add yield units
    df.cropyield %<>%
      mutate(yield_units=as.factor(q16))
    df.cropyield$yield_units<-fct_recode(df.cropyield$yield_units,"per acre"="1","per week"="2","total"="3")
    
    df.cropyield %<>%
      mutate(yield_kg_ha=case_when( 
        q16==1 & q17==2  ~ (q15*100)*0.4046, # reported in quintals per acre
        q16==2 & q17==1 ~ (q15)/total_area_hc, # reported in kg per week - assume should be total kg 
        q16==2 & q17==2 ~ (q15*100)/total_area_hc, # reported in quintals per week - assume should be total quintals
        q16==3 & q17==1 ~ (q15)/total_area_hc, # reported as total kg
        q16==3 & q17==2 ~ (q15*100)/total_area_hc,# reported as total quintals
        q16==1 & is.na(q17) ~ (q15*100)*0.4046, # reported as per acre but missing units, so imputing values as quins/ha, which is most reported response
        q16==2 & is.na(q17) ~ (q15*100)/total_area_hc # reported in per week but missing units, so imputing values assuming quins/ha
               )
      )
    
    # remove outliers (more or less than 3 SD from the mean by crop)
    df.cropyield %<>%
      group_by(crop_name) %>%
      mutate(
        mean_value = mean(yield_kg_ha, na.rm = TRUE),
        sd_value = sd(yield_kg_ha, na.rm = TRUE)
      ) %>%
      filter(abs(yield_kg_ha- mean_value) <= 3 * sd_value) %>%
      select(-mean_value, -sd_value) %>% # remove the temp columns
      ungroup()
    
    #### crop sale - paddy and wheat ####
    
    # Selling Status
    df.cropsold<- df.agri %>% select(a_fpo_hh,a_hhid,((ends_with("_q20"))))
    
    df.cropsold %<>% 
      pivot_longer(cols=!c(a_fpo_hh,a_hhid),
                   names_to = c("crop", ".value"),
                   names_pattern = "^(ag_[a-z]+_ap)_(q\\d+)",
                   values_drop_na = TRUE)
    
    df.cropsold %<>% mutate(crop_name=str_extract(crop,"(?<=_)[^_]+"),
                            sale_label=as.factor(q20)) %>% 
      filter(crop_name=="paddy"| crop_name=="wheat")
    
    df.cropsold$sale_label<-fct_recode(df.cropsold$sale_label,"Sold Fully"="1","Sold Partially"="2","Not Sold"="3") 
    
    
    # Sale amount (quintals)
    # Only for those that reported selling fully or partially
    
    df.cropsale<- df.agri %>% select(a_fpo_hh,a_hhid,((ends_with("_q20") | ends_with("_q22") | ends_with("_q23") | ends_with("_q25") | ends_with("_q27") | contains("_q28_")) & contains(crop_name)))

    df.cropsale <- df.cropsale %>% 
      pivot_longer(cols=!c(a_fpo_hh,a_hhid),
                   names_to = c("crop", ".value"),
                   names_pattern = "^(ag_[a-z]+_ap)_(q\\d+)",
                   values_drop_na = TRUE)

    df.cropsale %<>% mutate(crop_name=str_extract(crop, "(?<=_)[^_]+")) %>% 
      filter(crop_name=="wheat"|crop_name=="paddy")
    
    # Calculate sale quantity in quintals
    # Filter any observations that did not report selling any produce or had missing data on quantity
    
    df.cropsale %<>% filter(!(q20==3 | is.na(q22))) %>% 
      mutate(sale_quintal = case_when(
        q23==2  ~ q22, # reported in quintals
        q23==3 & q25==3  ~ (q22*80)/100 #  reported in bags of 80 kg each
      ))
 
    # Remove outliers (more than 3 standard deviations above or below the mean)
    
    # Sale quantity 
    df.cropsale %<>%
      group_by(crop_name) %>%
      mutate(
        mean_value = mean(sale_quintal, na.rm = TRUE),
        sd_value = sd(sale_quintal,na.rm = TRUE)
      ) %>%
      filter(abs(sale_quintal- mean_value) <= 3 * sd_value) %>%
      select(-mean_value, -sd_value) %>% # remove the temp columns
      ungroup()
    
    
    # Sale price (Rs.)
    df.cropsale %<>%
      mutate(sale_price_inr=q27/sale_quintal
      )
   
    df.cropsale %<>%
      group_by(crop_name) %>%
      mutate(
        mean_value = mean(sale_price_inr, na.rm = TRUE),
        sd_value = sd(sale_price_inr,na.rm = TRUE)
      ) %>%
      filter(abs(sale_price_inr- mean_value) <= 3 * sd_value) %>%
      select(-mean_value, -sd_value) %>% # remove the temp columns
      ungroup()
    
    
    # Crop selling point
    df.cropsellingpoint<- df.agri %>% select(a_fpo_hh,a_hhid,((contains("_q28")) & contains(crop_name)))
    
    df.cropsellingpoint <- df.cropsellingpoint %>% 
      pivot_longer(cols=!c(a_fpo_hh,a_hhid),
                   names_to = c("crop", ".value"),
                   names_pattern = "^(ag_[a-z]+_+a_ap_q28)_(\\d+)",
                   values_drop_na = TRUE)
    
    df.cropsellingpoint %<>% mutate(crop_name=str_extract(crop, "(?<=_)[^_]+"),
                                    sold_village_market=as.factor(`1`),
                                    sold_govt_mandi=as.factor(`2`),
                                    sold_trader=as.factor(`3`),
                                    sold_FPO=as.factor(`4`),
                                    sold_frishi_mela=as.factor(`5`),
                                    sold_direct_consumer=as.factor(`6`),
                                    sold_other=as.factor(`7`))
  
    df.cropsellingpoint$sold_village_market<-fct_recode(df.cropsellingpoint$sold_village_market,"Yes"="1","No"="0") 
    df.cropsellingpoint$sold_govt_mandi<-fct_recode(df.cropsellingpoint$sold_govt_mandi,"Yes"="1","No"="0") 
    df.cropsellingpoint$sold_trader<-fct_recode(df.cropsellingpoint$sold_trader,"Yes"="1","No"="0") 
    df.cropsellingpoint$sold_FPO<-fct_recode(df.cropsellingpoint$sold_FPO,"Yes"="1","No"="0") 
    df.cropsellingpoint$sold_frishi_mela<-fct_recode(df.cropsellingpoint$sold_frishi_mela,"Yes"="1","No"="0") 
    df.cropsellingpoint$sold_direct_consumer<-fct_recode(df.cropsellingpoint$sold_direct_consumer,"Yes"="1","No"="0") 
    df.cropsellingpoint$sold_other<-fct_recode(df.cropsellingpoint$sold_other,"Yes"="1","No"="0") 
    
    # Restricting to rice and paddy and those who sold
    
    df.cropsellingpoint %<>% 
    filter(crop_name=="wheat"|crop_name=="paddy") %>% 
    filter(a_hhid %in% df.cropsale$a_hhid) #restricting to only those who sold
    
    
    ## Income #### 
    df.income<-df.clean %>% select(a_hhid,a_resp_id,he_q2_cult:he_q14_exp)
    
    # number of income sources and total income- monthly
    df.income %<>%
      mutate(across(
        ends_with("_amt"),
        ~round((.x/12),1),
        .names = "{.col}_mon"
      )
      )
    
    df.income %<>% rowwise() %>%
      mutate(count_income_sources= sum(across(c(he_q2_cult,he_q4_liv,
                                          he_q6_othag,he_q8_nonag,he_q10_wage,he_q12_sal))),
             total_income_mon=sum(across(ends_with("_mon")),na.rm = TRUE),
             he_all_oth_inc_mon=sum(he_q7_othag_amt_mon,he_q9_nonag_amt_mon,he_q13_sal_amt_mon,na.rm = TRUE),
             alloth_inc_mon=sum(he_q7_othag_amt_mon,he_q9_nonag_amt_mon,
                                he_q11_wage_amt_mon,he_q13_sal_amt_mon))
    
    # Change Os into NAs for household level income
    df_status(df.income$count_income_sources)
    
    #Replace NAs in income with Os for households who did not have ag income
    df.income %<>% mutate(total_income_mon=ifelse(count_income_sources==0,0,total_income_mon),
      he_q3_cult_amt_mon=ifelse(he_q2_cult==0,0,he_q3_cult_amt_mon),
                      he_q5_liv_amt_mon=ifelse(he_q4_liv==0,0,he_q5_liv_amt_mon),
                      he_q7_othag_amt_mon=ifelse(he_q6_othag==0,0,he_q7_othag_amt_mon),
                      he_q9_nonag_amt_mon=ifelse(he_q8_nonag==0,0,he_q9_nonag_amt_mon),
                      he_q11_wage_amt_mon=ifelse(he_q10_wage==0,0,he_q11_wage_amt_mon),
      he_all_oth_inc_mon=ifelse(count_income_sources==0,0,he_all_oth_inc_mon)
    )
    

    ## Diet diversity ####
    df.diet<-df.clean %>% select(a_hhid,a_resp_id,dt_rice:dt_burger) %>% 
      mutate(
        fgds = ((rowSums(pick("dt_rice","dt_roti","dt_millet","dt_starch") == 1, na.rm=TRUE) > 0) + 
                  (rowSums(pick("dt_dal") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("dt_peanut") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("dt_cheese","dt_curd","dt_milk") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("dt_sausage","dt_meat", "dt_pork", "dt_chic","dt_fish") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("dt_eggs") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("dt_greenleaf") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("dt_carrot","dt_pap_ft") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("dt_tom","dt_gourd","dt_cuc") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("dt_org_ft","dt_ban_ft","dt_grap_ft") == 1, na.rm=TRUE) > 0)),
        
      )
    
    
    df.diet %<>% mutate(mddw = ifelse(fgds >= 5,1, 
                                        ifelse(fgds < 5, 0, NA)))
    
    ## Merge demographic, agriculture, income and diet data ####
    df.analysis<-left_join(df.demo,df.landown,by=c("a_hhid","a_resp_id"))
    df.analysis<-left_join(df.analysis,df.income,by=c("a_hhid","a_resp_id"))
    df.analysis<-left_join(df.analysis,df.diet,by=c("a_hhid","a_resp_id")) 
    df.analysis<-left_join(df.analysis,df.cropland,by=c("a_hhid")) 
    temp.ag<-df.agri %>% select(a_hhid,ag_total_crops,ag_land_kh_hc,land_cat) %>% filter(!is.na(ag_total_crops))
    temp.ag <- distinct(temp.ag)
    df.analysis<-left_join(df.analysis,temp.ag,by=c("a_hhid")) 
    df.analysis<- df.analysis %>% mutate(a_fpo_hh=a_fpo_hh.x)
    
    
     ## A-WEAI ####
     
     we<-df.clean %>% select(a_hhid,a_resp_id,starts_with("we_"))
     colnames(we)
     
     ### Production and Income Generation ####
     
     # MODULE G2 : ROLE IN HH DECISION-MAKING AROUND PRODUCTION AND INCOME GENERATION
     
     # g2.01 - Did you yourself participate in [ACTIVITY] in the past 12 months....
     we %>% select(we_part_a_q1_a,we_part_a_q4_b,we_part_a_q8_c,we_part_a_q12_d,we_part_a_q16_e) %>% 
       tbl_summary(statistic = all_categorical() ~ c("{n}, {p}%"))
     
     # Create a vector of alphabet sequence matching activities
     #q1= food farming q4=cash farming q8=livestock, q12= non-farming, q16=wage or salary, 
     #q20=major hh exp,q22=minor hh exp;
     
     we <- we %>%
       mutate(across(
         c(`we_part_a_q1_a`,`we_part_a_q4_b`,`we_part_a_q8_c`,`we_part_a_q12_d`,`we_part_a_q16_e`), # Note that activities F (fishing), G and H are not included here
         ~ifelse(.x == 1, 1, 0), 
         #.names = "partact_{str_extract(col,'q\\\\d+_(\\\\w+)')}"
         .names = "partact_{str_extract(col,'(?<=_)[a-z]$')}"
       ))
     
     names(we)
     
     # Summing the number of activities in which respondent participated in themselves
     we[, 'partact'] <- rowSums(select(we, partact_a:partact_e), na.rm = TRUE) # Number of activities in which individual participates
     
     # Summing the number of agricultural actitvities in which respondent participated in themselves
     we$partactagr <- rowSums(we[, c("partact_a", "partact_b", "partact_c")], na.rm = TRUE) # 
     
     #Number of agricultural activities in which individual participates
     table(we$partact)  
     table(we$partactagr) 
     
     # Sub-questions g2.02, g2.03 and g2.05
     
     # Adequate if respondent has at least some decision making power (meaning response codes 1-3 for g2.03)
     
     #If sole decision maker g2.02 across activities
     we %>% select("we_a_part_a_q2_1_a","we_a_part_a_q5_1_b","we_a_part_a_q9_1_c",
                   "we_a_part_a_q13_1_d","we_a_part_a_q17_1_e") %>% 
       tbl_summary(statistic = all_categorical() ~ c("{n}, {p}%"))
     
     #Some input in decision making
     we %>% select(we_part_a_q3_a,we_part_a_q6_b,we_part_a_q10_c,
                   "we_part_a_q14_d","we_part_a_q18_e") %>% 
       tbl_summary(statistic = all_categorical() ~ c("{n}, {p}%"))
     
     #Number of decision makers per activity
     
     we %<>% rowwise %>% mutate(num_dec_a=sum(across(starts_with("we_a_part_a_q2_"))),
                                num_dec_b=sum(across(starts_with("we_a_part_a_q5_"))),
                                num_dec_c=sum(across(starts_with("we_a_part_a_q9_"))),
                                num_dec_d=sum(across(starts_with("we_a_part_a_q13_"))),
                                num_dec_e=sum(across(starts_with("we_a_part_a_q17_"))))
     
     we <- we %>%
       mutate(
         # sole decision maker (selected self and no other member)
         across(c("we_a_part_a_q2_1_a","we_a_part_a_q5_1_b","we_a_part_a_q9_1_c",
                  "we_a_part_a_q13_1_d","we_a_part_a_q17_1_e"), 
                ~ case_when(.x == 1 & get(paste0("num_dec_",str_extract(cur_column(), "(?<=_)[a-z]$")))==1 ~ 1, 
                            .x != 1 ~ 0,
                            is.na(.x) ~ NA,
                            TRUE ~ 0),
                .names = "self_{str_extract(.col, '(?<=_)[a-z]$')}"
         ),
         # some say in decision
         across(c("we_part_a_q3_a","we_part_a_q6_b","we_part_a_q10_c","we_part_a_q14_d","we_part_a_q18_e"),
                ~ case_when(
                  .x==2 | .x==3 ~ 1,
                  .x == 1 ~ 0
                ),
                .names = "dec_{str_extract(.col, '(?<=_)[a-z]$')}"
         ),
         # Input decision
         across(
           starts_with("dec_"),
           ~ case_when(
             .x==1 ~ 1, # Had some say in decision making
             get(paste0("self_",str_extract(cur_column(), "(?<=_)[a-z]$"))) == 1 & get(paste0("partact_",str_extract(cur_column(), "(?<=_)[a-z]$")))==1 ~ 1, #Sole decision making and participated in the activity
             get(paste0("partact_",str_extract(cur_column(), "(?<=_)[a-z]$")))==1 & .x==0 ~ 0, #The denominator is respondents that participated in the activity but had no or few input
             TRUE ~ NA),
           .names = "inputdec_{str_extract(.col, '(?<=_)[a-z]$')}"
         )
       )
     names(we)
     table(we$inputdec_a)
     
     # g2.05 -  decisions regarding income generated from [ACTIVITY]
     
     we <- we %>%
       mutate(
         across(c("we_part_a_q7_b","we_part_a_q11_c","we_part_a_q15_d","we_part_a_q19_e"), #4 activities in the current survey
                ~ case_when(
                  get(paste0("partact_", str_extract(cur_column(), "(?<=_)[a-z]$"))) == 1  & (.x==2 | .x==3) ~ 1,
                  get(paste0("partact_", str_extract(cur_column(), "(?<=_)[a-z]$"))) == 1 & is.na(.x) ~ NA,
                  #(.x==1) & get(paste0("partact_", str_extract(cur_column(), "(?<=_)[a-z]$"))) == 1 ~ 0, #The denominator is respondents that participated in the activity but had no or few input (option 1)
                  TRUE ~ 0),
                .names = "incomedec_{str_extract(.col, '(?<=_)[a-z]$')}"
         ))
     table(we$incomedec_b)
     
     # Decisions regarding expenditure (In this survey we did not ask about feelings about decision making)
     #So this indicator is only relevant for actual decision making
     
     #Number of decision makers for expenditure activity
     
     we %<>% rowwise %>% mutate(num_expdec_f=sum(across(starts_with("we_a_part_a_q20_"))),
                                num_expdec_g=sum(across(starts_with("we_a_part_a_q22_"))))
     
     # Sole decision maker
     we <- we %>%
       mutate(
         # sole decision maker (selected self and no other member)
         across(c("we_a_part_a_q20_1_f","we_a_part_a_q22_1_g"), 
                ~ case_when(.x == 1 & get(paste0("num_expdec_",str_extract(cur_column(), "(?<=_)[a-z]$")))==1 ~ 1, 
                            .x != 1 ~ 0,
                            is.na(.x) ~ NA,
                            TRUE ~ 0),
                .names = "selfexp_{str_extract(.col, '(?<=_)[a-z]$')}"
         ),
         # some say in decision making
         across(c("we_part_a_q21_f","we_part_a_q23_g"),
                ~ case_when(
                  .x==2 | .x==3 ~ 1,
                  .x == 1 ~ 0
                ),
                .names = "decexp_{str_extract(.col, '(?<=_)[a-z]$')}"
         ),
         # Input in decisions regrading expenditure
         across(
           starts_with("decexp_"),
           ~ case_when(
             .x==1 ~ 1, # Had some say in decision making
             get(paste0("selfexp_",str_extract(cur_column(), "(?<=_)[a-z]$"))) == 1 ~ 1, #Sole decision making
             is.na(.x) ~ NA,
             #.x==0 ~ 0, #The denominator is respondents had no input
             TRUE ~ 0),
           .names = "inputdecexp_{str_extract(.col, '(?<=_)[a-z]$')}"
         )
       )
     
     ## Indicator: Production and Income Domain Aggregation
     
     #INPUT IN PRODUCTIVE DECISIONS: adequate if there is AT LEAST ONE activity in which individual has some input in decisions, or makes the decision, or feels he/she could make it if he/she wanted (we do not have the feel questions in this survey)
     
     # Sum of activities in which the respondent has some decision in
     we[, 'feelinginputdecagr_sum'] <- rowSums(select(we, c("inputdec_a","inputdec_b","inputdec_c")), na.rm = TRUE) # No. agr. activities individual has some input in decisions or feels can make decisions
     table(we$feelinginputdecagr_sum)
     
     # Binary if feelinginputdecagr_sum > 0
     we <- we %>% mutate(feelinginputdecagr = case_when(
       feelinginputdecagr_sum > 0 ~ 1,       # Has some input in decisions in AT LEAST ONE activity (binary)
       feelinginputdecagr_sum == 0 ~ 0,
       is.na(feelinginputdecagr_sum) ~ NA
     ))
     table(we$feelinginputdecagr)
     
     # Calculating No. activities individual has some input in income decisions
     we[, 'incomedec_sum'] <- rowSums(select(we, c("incomedec_b", "incomedec_c", "incomedec_d", "incomedec_e",
                                                   "inputdecexp_f","inputdecexp_g")), na.rm = TRUE)
     table(we$incomedec_sum)
     
     # Binary if incomedec_sum > 0
     we <- we %>% mutate(incdec_count = case_when(
       incomedec_sum > 0 ~ 1,       # Has some input in income decisions AND not only minor hh expend
       incomedec_sum ==1 & inputdecexp_g==1~ 0, #not only minor hh expend
       incomedec_sum <1 ~ 0,
       is.na(incomedec_sum) ~ NA
     ))
     table(we$incdec_count)
     
     # Drop all the skip variables since they are no longer needed
     we %<>% select(-starts_with("self"), -starts_with("partact"), -starts_with("inputdec"))
     names(we)
     
     ### Resources; productive Assets ####
     
     df.liv<-df.clean %>% select(a_hhid,a_resp_id,starts_with("we_"))
     df.liv1<- df.liv %>% select(a_hhid,a_resp_id,we_part_b_q4:we_a_part_b_q7_5)
     
     # #Count of livestock
     df.liv1%<>% rowwise() %>% mutate(we_total_livestock=sum(we_a_part_b_q5_1,we_a_part_b_q5_2,
                                                          we_a_part_b_q5_3,we_a_part_b_q5_4,
                                                          we_a_part_b_q7_1,we_a_part_b_q7_2,
                                                          we_a_part_b_q7_3,we_a_part_b_q7_4,
                                                          we_a_part_b_q7_5,na.rm = TRUE)) %>%
       ungroup()
    
     
     we %>% select("we_part_b_q1","we_part_b_q4","we_part_b_q6",we_part_b_q8:we_part_b_q16) %>% 
       tbl_summary(statistic = all_categorical() ~ c("{n}, {p}%"))
     
     #Yes, I own it solely = 1
     # Yes, I own it jointly = 2
     # Yes, I own some solely, some jointly = 3
     # Yes, someone else in the household owns it = 4
     # No, household does not own it = 5 [skip to Q4]
     # I do not know = 6 [skip to Q4]
     
     #Asset list
     #1=Ag land
     #4= Large livestock
     #6=Small livestock
     #8= Non-mechanised farm equipment
     #9=Mechanised farm eqiuipment
     #10= Non-farm business equip
     #11- Household or other structure
     #12- Large durables
     #13- Small durables
     #14- Mobile phone
     #15- Non-ag land
     #16- Transport
     
     we <- we %>%
       mutate(
         across(c("we_part_b_q1","we_part_b_q4","we_part_b_q6",we_part_b_q8:we_part_b_q16), # 12 items
                ~ case_when(.x <=4 ~ 1,
                            .x >4 ~ 0, #note this depends on whether "NO" is coded as 0 or 2
                            is.na(.x) ~ NA), 
                .names = "own_{str_extract(.col, '\\\\d+')}"), # generating own variable if they report hh owning it
       )
     
     
     # Sum types of assets hh owns (out of 12)
     we[, 'own_sum'] <- rowSums(select(we, starts_with("own_")), na.rm = TRUE) 
     table(we$own_sum)
     # Number of types of agricultural assets that household owns
     we[, 'ownag_sum'] <- rowSums(select(we, c("own_1", "own_4", "own_6", "own_8", "own_9")), na.rm = TRUE) # summing items that pertain to agricultural assets: In the survey we dont have fishing and poultry is combined with small livestock
     table(we$ownag_sum)
     
     #Joint or sole ownership
     we <- we %>%
       mutate(
         across(c("we_part_b_q1","we_part_b_q4","we_part_b_q6","we_part_b_q8","we_part_b_q9","we_part_b_q10"   ,"we_part_b_q11","we_part_b_q12","we_part_b_q13","we_part_b_q14","we_part_b_q15","we_part_b_q16"),
                ~ case_when(.x < 3 ~ 1, # if owned solely or jointly
                            
                            #(.x==3|.x==4) ~ 0, # if no then equal zero
                            .x >= 3 ~ 0
                ),
                .names = "selfjointown_{str_extract(.col, '\\\\d+')}")
       )
     # if the household does not own it, The denominator should be only households that own the asset
     table(we$selfjointown_1)
     
     ### Ownership ####
     
     #### OWNERSHIP: Adequate if:
     # - selfjoint owns AT LEAST two small assets (chicken=6,farming equipment non-mechanized=8, and small consumer durables=13)  
     # OR 
     # - one large asset (all the other). 
     
     #  This is the same to say: empowered if owns AT LEAST one asset and that asset is not a small asset.
     #  Inadequate if lives in a household that owns no assets
     
     # Summing the number of small assets that are owned
     we[, 'smallassets_sum'] <- rowSums(select(we, c("selfjointown_6", "selfjointown_8", "selfjointown_13")), na.rm = TRUE) 
     table(we$smallassets_sum)
     
     # Summing the number of large assets that are owned
     we[, 'largeassets_sum'] <- rowSums(select(we, c("selfjointown_1", "selfjointown_4", "selfjointown_9", "selfjointown_10", "selfjointown_11",
                                                     "selfjointown_12", "selfjointown_14", "selfjointown_15", "selfjointown_16")), na.rm = TRUE) 
     
     table(we$largeassets_sum)
     
     # Creating the binary indicator defined above
     
     we <- we %>%
       mutate(assetownership = 
                case_when(
                  smallassets_sum > 1 | largeassets_sum > 0 ~ 1, # if owns at least 2 small assets or owns 1 large asset
                  # if owns at least 1 large asset, # Zero hh owns no assets
                  TRUE ~ 0
                ))
     table(we$assetownership)
     
     ### Credit ####    
     
     # #  would anyone in hh be able to take loan or borrow from [SOURCE] ?
     
     # Constrained credit access based on question WEAI g3.03
     
     we %>% select("we_part_c_q1_a","we_part_c_q5_b","we_part_c_q9_c","we_part_c_q13_d") %>% 
       tbl_summary(statistic = all_categorical() ~ c("{n}, {p}%"))
     
     we <- we %>%
       mutate(
         across(c("we_part_c_q1_a","we_part_c_q5_b","we_part_c_q9_c","we_part_c_q13_d"), # note that we are now including lending sources a to d: 4 sources
                ~ case_when(.x == 2  ~ 1, # constrained if hh cannot borrow from that source
                            .x ==4 ~ NA, # dont know is missing 
                            TRUE ~ 0), # all else is zero meaning Yes or Maybe
                .names = "creditconstrained_{str_extract(.col, '(?<=_)[a-z]$')}")
       )
     table(we$creditconstrained_a)
     
     # Credit access based on question g3.04
     we %>% select("we_part_c_q2_a","we_part_c_q6_b","we_part_c_q10_c","we_part_c_q14_d") %>% 
       tbl_summary(statistic = all_categorical() ~ c("{n}, {p}%"))
     
     we <- we %>%
       mutate(
         across(c("we_part_c_q2_a","we_part_c_q6_b","we_part_c_q10_c","we_part_c_q14_d"), # note that we are now including lending sources a to d: 4 sources
                ~ case_when(.x == 1  ~ 1, # if any of the yes options for g3.04
                            .x == 2 ~ 0, # no 
                            is.na(.x) | .x == 3 ~ NA), # missing or don't know
                .names = "creditaccess_{str_extract(.col, '(?<=_)[a-z]$')}") # hh yes/maybe has access to credit source
       )  
     table(we$creditaccess_a)
     
     # AGGREGATING TO CREATE INDICATOR  
     
     # Number of credit sources that the hh uses
     we[, 'creditaccess'] <- rowSums(select(we, creditaccess_a:creditaccess_d), na.rm = TRUE) 
     table(we$creditaccess)    
     # No. of credit sources that the hh cannot borrow from
     we[, 'creditconstrained'] <- rowSums(select(we, creditconstrained_a:creditconstrained_d), na.rm = TRUE) 
     table(we$creditconstrained)
     
     #  who made the decision to borrow from [SOURCE] most of the time? (g3.05)
     # who makes decision re: what to do w/ money borrowed from [SOURCE] most of the time? (g3.06)
     
     #Number of decision makers regarding credit and usage
     
     we <- we %>%
       mutate(
         across(c("we_a_part_c_q3_1_a","we_a_part_c_q7_1_b","we_a_part_c_q11_1_c","we_a_part_c_q15_1_d"), # note that we are including 4 sources
                ~ case_when(.x == 1 & get(paste0("creditaccess_", str_extract(cur_column(), "(?<=_)[a-z]$"))) == 1 ~ 1, # if self made decision to borrow and had access to credit
                            (.x !=1) & get(paste0("creditaccess_", str_extract(cur_column(), "(?<=_)[a-z]$"))) == 1  ~ 0, # The denomin
                            TRUE ~ NA), # all else is 0 meaning that if self didn't participate either on own or jointly in decision
                .names = "creditselfjointborrow_{str_extract(.col, '(?<=_)[a-z]$')}"), 
         
         # self or joint decide how to use
         across(c("we_a_part_c_q4_1_a","we_a_part_c_q8_1_b","we_a_part_c_q12_1_c","we_a_part_c_q16_1_d"),
                ~ case_when(.x == 1 & get(paste0("creditaccess_", str_extract(cur_column(), "(?<=_)[a-z]$"))) == 1 ~ 1, # if self made decision about what to do with money
                            get(paste0("creditaccess_", str_extract(cur_column(), "(?<=_)[a-z]$"))) == 1 & (.x !=1) ~ 0, # if access but didn't say who borrowed
                            TRUE ~ 0),
                .names = "creditselfjointuse_{str_extract(.col, '(?<=_)[a-z]$')}")
       )
     
     table(we$creditselfjointborrow_a, useNA = "ifany")
     table(we$creditselfjointuse_a, useNA = "ifany")
     
     # Self or joint makes AT LEAST ONE decision regarding credit
     
     we <- we %>%
       mutate(
         creditselfjointanydec_a = if_else(creditselfjointborrow_a > 0 | creditselfjointuse_a > 0, 1, 
                                           if_else(is.na(creditselfjointborrow_a) & is.na(creditselfjointuse_a), NA, 0)), # Formal
         
         creditselfjointanydec_b = if_else(creditselfjointborrow_b > 0 | creditselfjointuse_b > 0, 1, 
                                           if_else(is.na(creditselfjointborrow_b) & is.na(creditselfjointuse_b), NA, 0)), # informal lender
         
         creditselfjointanydec_c = if_else(creditselfjointborrow_c > 0 | creditselfjointuse_c > 0, 1, 
                                           if_else(is.na(creditselfjointborrow_c) & is.na(creditselfjointuse_c), NA, 0)), # Friends
         
         creditselfjointanydec_d = if_else(creditselfjointborrow_d > 0 | creditselfjointuse_d > 0, 1, 
                                           if_else(is.na(creditselfjointborrow_d) & is.na(creditselfjointuse_d), NA, 0)), # SHGs
         
       )   
     
     #  AGGREGATING TO CREATE INDICATOR 
     
     #Adequate if self/selfjoint makes dec regarding AT LEAST ONE source of credit AND has at least one source of credit
     
     # Number of credit sources in which they self/jointly made decision to borrow
     we[, 'creditselfjointborrow_sum'] <- rowSums(select(we, starts_with("creditselfjointborrow_")), na.rm = TRUE) 
     
     # Number of credit sources in which they self/jointly made decision on how to use it
     we[, 'creditselfjointuse_sum'] <- rowSums(select(we, starts_with("creditselfjointuse_")), na.rm = TRUE) 
     
     
     ##### ------------ Adequate if Self or joint makes AT LEAST ONE decision regarding credit
     
     # Summing the sources of credit in which respondent makes at least 1 decision
     
     we[, 'credjanydec_sum'] <- rowSums(select(we, starts_with("creditselfjointanydec_")), na.rm = TRUE) 
     table(we$credjanydec_sum)
     # Creating the binary indicator where they self/jointly make AT LEAST ONE decision regarding AT LEAST ONE source of credit
     
     we <- we %>%
       mutate(credjanydec_any = ifelse(credjanydec_sum > 0, 1, 
                                       ifelse(is.na(credjanydec_sum), NA, 0))) # Self/Jointly makes AT LEAST ONE decision regarding AT LEAST ONE source of credit
     
     table(we$credjanydec_any)
     
     
     ### Group Membership ####     
     
     # SHG membership

     we %>% select(we_part_d_q1,we_part_d_q2,we_part_d_q3,we_part_d_q4) %>% 
       tbl_summary(by="we_part_d_q1",
         statistic = all_categorical() ~ c("{n}, {p}%"))
     
     we <- we %>%
       mutate(group_binary= 
                ifelse(we_part_d_q3==1 & we_part_d_q4==1,1, #There is a group in the community and the person is active member
                       #ifelse(is.na(we_part_d_q2),NA,0)))
                       ifelse(we_part_d_q3==2|we_part_d_q4==2|we_part_d_q4==3|is.na(we_part_d_q4),0,NA))) #There is no group in the community or the person is not an active member
     
     ### Time-use ####
     
     time<-df.clean %>% select(a_hhid,a_resp_id,d1:d48) %>% mutate(across(c(d1:d48),as.factor))
     str(time)
     time %>% select(d1:d48) %>% tbl_summary(type=c(d1:d48) ~ "categorical")
     
     #Calculate number of productive activities, minutes and hours spent (we have a 30 mins window)
     productive_work_activitycodes <- c("4", "5", "6", "7", "8", "9", "10", "13", "14")
     
     time %<>% rowwise() %>% 
       mutate(cont_workact= sum(c_across(d1:d48) %in% productive_work_activitycodes),
              cont_workload=(cont_workact*30), 
              cont_workload_hr=cont_workload/60)
     summary(time$cont_workact)
     summary(time$cont_workload)
     summary(time$cont_workload_hr)
     
     # Workload variable
     time %<>% mutate(poor_z105=ifelse(cont_workload_hr > 10.5, 1, 0),
                      npoor_z105=ifelse(cont_workload_hr <= 10.5, 1, 0))
     
     table(time$npoor_z105)
     
     # Join time with the weai module
     we<-left_join(we,time,by=c("a_hhid","a_resp_id"))
     names(we)
     
    
     ### Identifying empowered women ####
     
     df.weai <- we %>% select(a_hhid,a_resp_id,
                              feelinginputdecagr,
                              assetownership,
                              credjanydec_any,
                              group_binary,
                              incdec_count,
                              npoor_z105)
       
       #Define the weighted adequacy               
       df.weai %<>% mutate(w_feelinginputdecagr=1/5,
                           w_assetownership= 1/10, 
                           w_credjanydec_any=1/10, 
                           w_group_binary=1/5,
                           w_incdec_count=1/5, 
                           w_npoor_z105=1/5,
                           weight=1, #Note: =1 if unweighted; otherwise, assign variable containing individual sampling weights
     )
     
    
     #### ---------------------------------   DEFINE THE WEIGHTED ADEQUACY OF G0* MATRIX  
     
     # Create new variables with specified weights using mutate and across
     df.weai  %<>% mutate(wg0_feelinginputdecagr=feelinginputdecagr*w_feelinginputdecagr,
                     wg0_assetownership= assetownership*w_assetownership,
                     wg0_credjanydec_any=credjanydec_any*w_credjanydec_any,
                     wg0_group_binary=group_binary*w_group_binary,
                     wg0_incdec_count=incdec_count*w_incdec_count,
                     wg0_npoor_z105=npoor_z105*w_npoor_z105
     )
     
     #### ---------------------------------  Define the (weighted) adequacy count vector "ci" ------------------------------------------------------------------
     
     # Computing the frequency of missing values for the indicator
     df.weai %<>% rowwise %>% mutate(ci=sum(across(starts_with("wg0_")), na.rm=TRUE),
                                 n_missing=sum(is.na(c_across(starts_with("wg0_")))))
     
     #Check sample drop due to missing values # Will drop males due to non-participation
     df.weai %<>% filter(n_missing==0)
     
     # Identify women with a empowerment of more than cutoff

     # Assign a cutoff %, 3 out 5 domains
     
     df.weai %<>% 
       mutate(empowered_40=ifelse(ci>=0.4,1,0),
         empowered_60=ifelse(ci>=0.6,1,0))
     
     
     # for (x in cutoff_values) {
     #   new1<- paste0("ch_", x, "p")
     #   new2<- paste0("a_", x, "p")
     #   we1 %<>%
     #     mutate(!!new1:= ifelse(ci > (x / 100),1,0), #WE CREATE A VARIABLE THAT IDENTIFIES THE SEMPOWERED INDIVIDUALS (THOSE WHO HAVE AN ADEQUACY SCORE HIGHER THE 40%).
     #            !!new2:= ifelse(get(paste0("ch_",x,"p"))==1,ci,NA), # WE COMPUTE THE INDIVIDUAL INADEQUACY OF THOSE WHO ARE DISEMPOWERED. //
     #            
     #     )
     # }
     # 
     # for (x in cutoff_values) {
     #   new3<- paste0("DAI_", x, "p")
     #   new4<-paste0("EAI_",x,"p")
     #   we1 %<>%
     #     mutate(!!new3:=sum((ci*(get(paste0("ch_",x,"p")))*weight/total_w),na.rm=TRUE), #WE COMPUTE THE DISEMPOWERMENT INDEX (FOR EACH POSSIBLE CUTOFF X) //
     #            !!new4 :=1-(get(paste0("DAI_",x,"p")))
     #     )
     # }
     
  
     df.analysis<-left_join(df.analysis,df.weai,by=c("a_hhid","a_resp_id"))
     
     
     
# Regression Models ####
    
    # regression datasets
       df.reg<- df.analysis %>% 
         select(a_hhid,a_resp_id,a_fpo_hh,a_sex,a_caste,a_educ,he_q1_hhmem,
                ag_total_crops,ag_land_kh_hc,total_income_mon,
                fgds,mddw,empowered_60)

      # set non-FPO households as reference category
      df.reg$a_fpo_hh<- fct_recode(df.reg$a_fpo_hh,
                                  "FPO household"="1",
                                  "Non-FPO household"="2")
      
      df.reg$a_fpo_hh<- fct_relevel(df.reg$a_fpo_hh,"Non-FPO household","FPO household")
     
     # household level dataset for crop diversity and income
     
       # create 2 variables: 1 for education of males and 1 for education of females in a household
       df.reg.hh<- df.reg %>% 
         pivot_wider(names_from = a_sex, values_from = a_educ, names_prefix = "a_educ_") 
       
       # fill in values for education of females in a household
       df.reg.hh <- df.reg.hh %<>%  
         group_by(a_hhid) %>% 
         mutate(across(starts_with("a_educ_Female"), ~na.locf(.x, na.rm = FALSE, fromLast = TRUE))) %>%
         ungroup() 
       
       # single row per household
       df.reg.hh <- df.reg.hh %<>%  
         filter(!is.na(a_educ_Male) & !is.na(a_educ_Female)) %>% 
         select(a_hhid,a_fpo_hh,ag_total_crops,total_income_mon,he_q1_hhmem,ag_land_kh_hc,a_caste,a_educ_Male,a_educ_Female)
      
       # individual level dataset for diet diversity and A-WEAI
       df.reg.ind<- df.reg %>% 
         select(a_hhid,a_resp_id,a_fpo_hh,ag_land_kh_hc,a_sex,a_caste,a_educ,
                fgds,mddw,empowered_60)
       
    ## crop diversity ####
    crp1<- lm(ag_total_crops~a_fpo_hh+a_caste+a_educ_Male+a_educ_Female+ag_land_kh_hc, data=df.reg.hh) 
    
    cd<-crp1 %>% 
      tbl_regression(
        pvalue_fun = ~ style_pvalue(.x, digits = 2),
        tidy_fun = partial(tidy_robust,vcov="HC3")
      ) %>% 
      add_global_p() %>% 
      bold_p(t = 0.05) %>% 
      modify_column_unhide(column = std.error) %>% 
      as_flex_table() 
    
    cd
    
    ## income ####
    
    #write Excel for Stata 
    write_xlsx(df.reg.hh,"Income_Reg.xlsx")
    
    ## diet diversity ####
    
    # binary outcome
    mddw1<- glm(mddw~a_fpo_hh+a_caste+a_educ+ag_land_kh_hc+a_sex,data=df.reg.ind,
                family = binomial(link = "logit")) %>% 
      tbl_regression(
        exponentiate = TRUE,
        pvalue_fun = ~ style_pvalue(.x, digits = 2),
        tidy_fun = partial(tidy_robust,vcov="vcovCL",vcov_args=list(cluster = df.reg.ind$a_hhid))
      ) %>% 
      add_global_p() %>% 
      modify_column_unhide(column = std.error) %>% 
      bold_p(t = 0.05) %>% 
      as_flex_table()
      
    mddw1
    
    theme_gtsummary_compact()
    
    # continuous outcome
    mddw2<- lm(fgds~a_fpo_hh+a_caste+a_educ+ag_land_kh_hc+a_sex,data=df.reg.ind)
    
    tb.mddw2<- mddw2 %>% 
      tbl_regression(
        pvalue_fun = ~ style_pvalue(.x, digits = 2),
        tidy_fun = partial(tidy_robust,vcov="HC3"),
        intercept = TRUE,
      ) %>% 
      add_global_p() %>% 
      bold_p(t = 0.05) %>% 
      modify_column_unhide(column = std.error) %>% 
      as_flex_table() 
    
    tb.mddw2
    
    theme_gtsummary_compact()
    

    ## Women's empowerment (A-WEAI) ####    
    temp.weai<- df.reg.ind %>% filter(a_sex=="Female") 
    
    weai<- glm(empowered_60~a_fpo_hh+a_caste+a_educ+ag_land_kh_hc, data=temp.weai,
                family = binomial(link = "logit")) %>% 
      tbl_regression(
        exponentiate = TRUE,
        pvalue_fun = ~ style_pvalue(.x, digits = 2),
        tidy_fun = partial(tidy_robust,vcov="vcovCL")
      ) %>% 
      add_global_p() %>% 
      modify_column_unhide(column = std.error) %>% 
      bold_p(t = 0.05) %>% 
      add_n(location = "label") %>% 
      as_flex_table()
    
    weai
    
    theme_gtsummary_compact()
    

    
# Table and Figures ####  
    
    # Dataset for tables and figures
    temp.liv<-df.liv1 %>% select(a_hhid,a_resp_id,we_total_livestock)
    temp.liv %<>% mutate(tot_liv_lbl=as.factor(we_total_livestock))
    temp.liv$tot_liv_lbl <-fct_collapse(temp.liv$tot_liv_lbl,"2 or more" = c("2","3","4"),
                                        "1"="1",
                                        "None"="0"
                                        )
    
    df.tab<-left_join(df.analysis,temp.liv,by=c("a_hhid","a_resp_id"))
    df.tab$a_fpo_hh<- fct_recode(df.tab$a_fpo_hh,
                                 "FPO household"="1",
                                 "Non-FPO household"="2")
    
    ## Table 1 - demographics ####
    
    # individual level
    dem1 <- df.tab %>% 
      dplyr::select(a_fpo_hh,a_age,a_educ,a_caste) %>% 
      tbl_summary(by=a_fpo_hh,missing = "no",
                  digits = all_continuous() ~ 1,
                  type = list(all_continuous() ~ "continuous"
                  ),
                  statistic = list(all_continuous() ~ c("{mean} ({sd})"),
                                   all_categorical() ~ c("{p}%, ({n})"))
                  ) %>%
      modify_header(label ~ "****") %>%
      add_overall() %>% 
      add_n(statistic = "{p_miss}% ({N_miss})") %>%
      as_flex_table() 

    dem1
    
    # household level
    df.tab2<- df.tab %>% group_by(a_hhid) %>% slice(1) %>% 
    select(a_fpo_hh,
           he_q1_hhmem,land_cat,ag_landown_hc,ag_total_crops,tot_liv_lbl,
           total_income_mon,
           he_q2_cult,he_q3_cult_amt_mon,
           he_q4_liv,he_q5_liv_amt_mon,
           he_q10_wage,he_q11_wage_amt_mon,
           he_q8_nonag,
           he_all_oth_inc_mon) %>% 
      ungroup()
    
    dem2 <- df.tab2 %>% 
      dplyr::select(a_fpo_hh,he_q1_hhmem,
                    land_cat,ag_landown_hc,ag_total_crops,tot_liv_lbl,
                    total_income_mon,
                    he_q2_cult,he_q3_cult_amt_mon,
                    he_q4_liv,he_q5_liv_amt_mon,
                    he_q10_wage,he_q11_wage_amt_mon,
                    he_q8_nonag,
                    he_all_oth_inc_mon) %>% 
      tbl_summary(by=a_fpo_hh,missing = "no",
                  digits = all_continuous() ~ 1,
                  type = list(all_continuous() ~ "continuous",
                              c(he_all_oth_inc_mon) ~"continuous"
                  ),
                  statistic = list(all_continuous() ~ c("{mean} ({sd})"),
                                   all_categorical() ~ c("{p}%, ({n})"))
      ) %>%
      modify_header(label ~ "****") %>%
      add_overall() %>% 
      add_n(statistic = "{p_miss}% ({N_miss})") %>%
      as_flex_table() 
 
    dem2
    
    save_as_docx(dem1,dem2,
                 path = paste0("05 Analysis/raw/","Table1_",format(Sys.time(),"%d%m%Y"),".docx"))
    
    ## Figure 1 - crops ####
    crop<- df.agri %>% select(a_fpo_hh,ag_a_ap_q6_1:ag_a_ap_q6_29,mustard) %>% 
      rename(!!!setNames(names(.), crop_label))

    crop %<>% mutate(Vegetable=ifelse(`Mixed Vegetables`==1,1,Vegetable)) 
    
    crop1<-crop %>% select(a_fpo_hh,Wheat,Paddy,Vegetable,Sugarcane,Mustard,Chickpea,
                           "Redgram","Green Gram","Blackgram","Jowar","Pearl millet",Maize) %>% 
      group_by(a_fpo_hh) %>% 
      summarise(across(everything(), list(mean = mean),na.rm=TRUE))
    
    crop_label<-c("a_fpo_hh","Wheat","Paddy","Vegetable","Sugarcane","Mustard","Chickpea",
                  "Redgram","Green Gram","Blackgram","Jowar","Pearl millet","Maize")
    
    crop1 %<>% rename(!!!setNames(names(.), crop_label))
    
    crop_long <- crop1 %>%
      pivot_longer(cols = c(2:13),
                   values_to = "value",values_drop_na = TRUE) %>% 
      mutate(value=round(value*100,1))
    
    crop_fig<-crop_long %>%
      ggplot(aes(x=reorder(name, value),y=value,fill=fct_rev(a_fpo_hh)))+
      geom_col(width=0.5,
               position = position_dodge(width=0.6))+
      coord_flip(clip = 'off')+
      scale_y_continuous(breaks = c(0, 25, 50, 75,100),
                         labels = function(x) paste0(x, "%"),
                         limits = c(-0.1, 110)) +
      scale_fill_manual(values = c("#F5C300", "#20A14D"),
                        labels=c("Non-FPO household","FPO household"))+
      theme_bw()+
      guides(fill= guide_legend(reverse=T))+
      geom_text(aes(x=name,y=110,label=paste(value,"%",sep=""),group=fct_rev(a_fpo_hh)),size=6,color="black",fontface="bold",position=position_dodge(width=0.6))+
      labs(x = NULL,y="Percentage(%)")+
      theme(legend.position = "top",
            legend.title = element_blank(),
            axis.text.y = element_text(size=16,color="black"),
            axis.text.x = element_text(size=14,color="black"),
            legend.text = element_text(size=14, face="bold"))
    
    
    png(file=paste0("05 Analysis/raw/crop_fig_", format(Sys.time(), "%d%m%Y"), ".png"),
        units="in", width=15, height=10, res=320)
    crop_fig
    dev.off()
    
    ##Figure 2 - income sources ####
    hheco_sum<- df.tab2 %>% select(a_fpo_hh,he_q3_cult_amt_mon,he_q11_wage_amt_mon,
                                 he_q5_liv_amt_mon,he_all_oth_inc_mon) %>% 
      group_by(a_fpo_hh) %>% 
      summarise(across(everything(), list(mean = mean,sd=sd),na.rm=TRUE))
    
    var_name <- sub("_mean|_sd", "", names(hheco_sum)[-1])
    
    df_long <- hheco_sum %>%
      pivot_longer(cols = -a_fpo_hh,
                   names_to = "var",
                   values_to = "value") %>% 
      mutate(measure= str_extract(var, "(?<=_)[^_]+$")) %>% 
      mutate(var = gsub("_[^_]+$", "", var))
    
    df_wide <- df_long %>%
      pivot_wider(names_from = measure, values_from = value)
    
    inc_fig<-ggplot(df_wide, aes(x = reorder(var, mean), y = mean,fill=fct_rev(a_fpo_hh))) +
      geom_bar(stat = "identity",width=0.5,
               position = position_dodge(width=0.6)) +
      coord_flip(clip = 'off')+
      scale_y_continuous(breaks = c(0, 1000, 2000, 3000,4000),
                         limits = c(-0.1, 4000)) +
      scale_fill_manual(values = c("#F5C300", "#20A14D"))+
      ggtitle("") + ylab("Mean Income (INR)") + xlab("")+
      geom_text(aes(label = round(mean, 0)), size = 6, 
                position = position_dodge(0.6), hjust=-0.5,
                color="black",fontface="bold")+
      theme_bw()+
      guides(fill= guide_legend(reverse=T))+
      theme(legend.position = "top",
            legend.title = element_blank(),
            axis.text.y = element_text(size=16,color="black",face="bold"),
            axis.text.x = element_text(size=16,color="black"),
            legend.text = element_text(size=14, face="bold"),
            axis.title.x=element_text(
              size=18,color="black"))+
      scale_x_discrete(labels=c("All other income",
                                "Livestock income",
                                "Wage income","Cultivation income"
      ))
    
    png(file=paste0("05 Analysis/raw/inc_fig_", format(Sys.time(), "%d%m%Y"), ".png"),
        units="in", width=15, height=10, res=320)
    inc_fig
    dev.off()
    
    ## Table 2 - yield, sale quantity, price ####
    
    # yield
    tb.yl<-df.cropyield %>% select(crop_name,a_fpo_hh,yield_kg_ha) %>% 
      tbl_strata(strata = crop_name,
                 ~ .x %>%
                   tbl_summary(by=a_fpo_hh,
                               digits = all_continuous() ~ 1,
                               type = list(all_continuous() ~ "continuous2"),
                               statistic = list(all_continuous() ~ c("{mean} ({sd})"),
                                                all_categorical() ~ c("{n}, {p}%")),
                               missing_text = "(Missing)")  %>%
                   modify_header(label ~ "**Variable**") %>% 
                   add_n()) %>% 
      as_flex_table()
    
    tb.yl
    
    # selling status
    tb.sl.st<- df.cropsold %>% 
      select(-a_hhid,-crop,-q20) %>%
      tbl_strata(strata = crop_name,
                 ~ .x %>%
                   tbl_summary(by=a_fpo_hh,
                               missing="no",
                               digits = list(all_continuous() ~ 1, all_categorical() ~ 1),
                               type = list(all_continuous() ~ "continuous2"),
                               statistic = list(all_continuous() ~ c("{mean} ({sd})"),
                                                all_categorical() ~ c("{p}%, ({n}) "))
                               ) %>% 
                   add_n()
      ) %>% 
      as_flex_table()
    
    tb.sl.st
    
    # sale quantity and price
    tb.sl<-df.cropsale %>% select(crop_name,a_fpo_hh,sale_quintal,sale_price_inr) %>% 
      tbl_strata(strata = crop_name,
                 ~ .x %>%
                   tbl_summary(by=a_fpo_hh,
                               digits = all_continuous() ~ 1,
                               type = list(all_continuous() ~ "continuous2"),
                               statistic = list(all_continuous() ~ c("{mean} ({sd})"),
                                                all_categorical() ~ c("{p}% ({n})")),
                               label=list(
                                 sale_quant = "Quantity sold (quintals)",
                                 sale_price = "Sale Price (Rs/quintals)"
                               )
                   )  %>%
                   modify_header(label ~ "**Variable**") %>% 
                   add_n()
      ) %>% 
      as_flex_table()
    
    tb.sl
    
    # selling point
    tb.sl.pt<- df.cropsellingpoint %>% 
      select(a_fpo_hh,crop_name,sold_village_market:sold_other) %>%
      tbl_strata(strata = crop_name,
                 ~ .x %>%
                   tbl_summary(by=a_fpo_hh,
                               missing="no",
                               digits = list(all_continuous() ~ 1, all_categorical() ~ 0),
                               type = list(all_continuous() ~ "continuous2"),
                               statistic = list(all_continuous() ~ c("{mean} ({sd})"),
                                                all_categorical() ~ c("{p}% ({n})"))
                               ) %>% 
                   add_n()
      )
    
    tb.sl.pt
    
    save_as_docx("Yield"=tb.yl, "Sale status"=tb.sl.st, "Sale quantity and price"=tb.sl, "Selling point"=tb.sl.pt,
                 path = paste0("05 Analysis/raw/","Crop yield and sale summary table_",format(Sys.time(),"%d%m%Y"),".docx"))
    
    ## Figure 3 - diet diversity ####
    

    result <- predict_response(mddw2, c("a_sex","a_fpo_hh"))
    test_predictions(result)
    
    # fgds.fig<-ggpredict(mddw2, 
    #                     terms = c("a_sex","a_fpo_hh"))
    
    fgds.fig<-ggpredict(mddw2, 
                        terms = c("a_sex","a_fpo_hh")) %>% 
      plot(colors = c("#F5C300", "#20A14D"),line_size=1,dot_size=4) +
      geom_text_repel(aes(label = round(predicted,1)),
                      size=7, position = position_dodge(0.6),fontface="bold")+
      ggtitle("") + 
      ylab("FGDS score") + xlab("Respondent sex")+
      theme_bw()+
      labs(shape= "a_fpo_hh")+
      theme(legend.position = "top",
            legend.title = element_blank(),
            axis.text.y = element_text(size=18,color="black"),
            axis.text.x = element_text(size=18,color="black"),
            legend.text = element_text(size=18, face="bold"),
            axis.title.x=element_text(
              size=16,color="black"),
            axis.title.y=element_text(
              size=16,color="black"))+
      scale_y_continuous(breaks = c(0,2,4,6),
                         limits = c(0,6))
    
    fgds.fig
    
    png(file=paste0("fgds.fig_", format(Sys.time(), "%d%m%Y"), ".png"),
        units="in", width=14, height=8, res=200)
    fgds.fig
    dev.off()
    
    ## Table 3 - diet ####
    dqq_fpo<-df.tab %>% select(a_fpo_hh,fgds,mddw,
                               dt_gourd,dt_cuc,dt_greenleaf,
                               dt_pap_ft,dt_ban_ft,dt_grap_ft,
                               dt_cheese,dt_curd,dt_milk,dt_fish,
                               dt_peanut,
                               dt_cake,dt_sweet,dt_chips,dt_noodles,
                               dt_snacks,dt_juice,dt_softdrink) %>% 
      tbl_summary(by=a_fpo_hh,missing = "no",
                  digits = all_continuous() ~ 1,
                  type = list(all_continuous() ~ "continuous",c(fgds) ~ "continuous"
                  ),
                  statistic = list(all_continuous() ~ c("{mean} ({sd})"),
                                   all_categorical() ~ c("{p}% ({n})")),
                  label = c("mddw" ~ "Diverse diet (minimum dietary diversity score ≥5)",
                            "fgds" ~ "Minimum dietary diversity score",
                            "dt_gourd" ~ "Gourds",
                            "dt_cuc" ~ "Cucumber, capsicum, drumstick",
                            "dt_greenleaf" ~ "Green Leafs: mustard leaves,spinach,other greens",
                            "dt_pap_ft" ~ "Papaya, mango",
                            "dt_ban_ft" ~ "Banana, apple, watermelon",
                            "dt_grap_ft" ~ "Grapes, peaches, jackfruit",
                            "dt_cheese" ~ "Paneer",
                            "dt_curd" ~ "Curd",
                            "dt_milk" ~ "Milk",
                            "dt_fish" ~ "Fish",
                            "dt_peanut" ~ "Peanuts, cashews, almonds, pistachios, walnuts, pumpkin seeds, or sunflower seeds",
                            "dt_cake" ~ "Cake, biscuits, halwa, jalebi, ladoo",
                            "dt_sweet" ~ "Other mithai, kulfi, ice cream, shakes",
                            "dt_chips" ~ "Chips, namkeen",
                            "dt_noodles" ~ "Maggi noodles, wai wai",
                            "dt_snacks" ~ "Samosa, pakora, puri, vada",
                            "dt_juice" ~ "Fruit juice, frooti",
                            "dt_softdrink" ~ "Cold drinks"
                            )
                  ) %>%
      modify_header(all_stat_cols() ~ "**{level}**") %>%
      add_overall() %>% 
      bold_labels() %>%
      add_variable_grouping(
        "Nutrituous foods" = c("dt_gourd",
        "dt_cuc",
        "dt_greenleaf",
        "dt_pap_ft",
        "dt_ban_ft",
        "dt_grap_ft",
        "dt_cheese",
        "dt_curd",
        "dt_milk",
        "dt_fish",
        "dt_peanut"),
        "Unhealthy foods" = c(
        "dt_cake",
        "dt_sweet",
        "dt_chips",
        "dt_noodles",
        "dt_snacks",
        "dt_juice",
        "dt_softdrink")
      ) %>% 
      as_flex_table()
    
    dqq_fpo
    
    save_as_docx("Diet by FPO"=dqq_fpo,
                 path = paste0("05 Analysis/raw/","Diet table_",format(Sys.time(),"%d%m%Y"),".docx"))

    ## Table 4 - women's empowerment ####
    tb.weai<-df.tab %>% filter(a_sex=="Female") %>% 
      select(empowered_60,"feelinginputdecagr", "assetownership",
                      "credjanydec_any","group_binary",
                      "incdec_count","npoor_z105",a_fpo_hh) %>% 
      tbl_summary(by="a_fpo_hh",missing = "no",
                  type = list(c("feelinginputdecagr", "assetownership",
                                "credjanydec_any","group_binary",
                                "incdec_count","npoor_z105") ~ "categorical"),
                  statistic = all_categorical() ~ c("{p}% ({n})"),
                  label = c("empowered_60" ~ "Overall women’s empowerment",
                            "feelinginputdecagr" ~ "Input in productive decisions",
                            "assetownership" ~ "Ownership of assets",
                            "credjanydec_any" ~ "Access to and decisions about credit",
                            "incdec_count" ~ "Control over use of income",
                            "group_binary" ~ "Self-help group membership",
                            "npoor_z105" ~ "Work balance")
                  ) %>% 
      add_n(statistic = "{p_miss}% ({N_miss})") %>%
      as_flex_table()
    
    tb.weai
    
    save_as_docx("WEAI_Table"=tb.weai,
                 path = paste0("05 Analysis/raw/","WEAI_table_",format(Sys.time(),"%d%m%Y"),".docx"))
    
    ## FPO advice ####
    tb.fpo_adv<-df.clean %>% select(a_hhid,a_resp_id,a_fpo_hh,fpo_ap_q35,
                                 fpo_a_ap_q37_1,fpo_a_ap_q37_2,
                                 fpo_a_ap_q36_1,fpo_a_ap_q36_2,fpo_a_ap_q36_3,
                                 fpo_ap_q38,fpo_ap_q39_o1,fpo_ap_q39_o2,
                                 fpo_ap_q40,fpo_a_ap_q41_1,fpo_a_ap_q41_2,fpo_a_ap_q41_3,
                                 fpo_ap_q42
                                ) %>% 
      mutate(fpo_ap_q39_o2=ifelse(fpo_ap_q38==1 & a_fpo_hh==1 & fpo_ap_q39_o2==0,0,
                                  ifelse(fpo_ap_q38==1 & a_fpo_hh==2,0,fpo_ap_q39_o2))) %>% 
      tbl_summary(by=a_fpo_hh, missing = "no",
                  digits = all_categorical() ~ 0,
                  type = list(all_continuous() ~ "continuous"),
                  statistic = list(all_continuous() ~ c("{mean} ({sd})"),
                                   all_categorical() ~ c("{p}% ({n})")),
                  label = c("fpo_ap_q35" ~ "Received any regular farm-related advice",
                            "fpo_a_ap_q37_1" ~ "FPO ",
                            "fpo_a_ap_q37_2" ~ "Government or KVK (Krishi Vigyan Kendra)",
                            "fpo_a_ap_q36_1" ~ "Weather",
                            "fpo_a_ap_q36_2" ~ "Pest incidence and management",
                            "fpo_a_ap_q36_3" ~ "Market prices",
                            "fpo_ap_q38" ~ "Receive any training on crop production",
                            "fpo_ap_q39_o1" ~ "FPO",
                            "fpo_ap_q39_o2" ~ "Government/KVK",
                            "fpo_ap_q40" ~ "Purchase any inputs from FPO in past year",
                            "fpo_a_ap_q41_1" ~ "Fertilizer",
                            "fpo_a_ap_q41_2" ~ "Pesticide",
                            "fpo_a_ap_q41_3" ~ "Seeds",
                            "fpo_ap_q42" ~ "Used any processing facilities"
                            )
                  ) %>% 
      modify_header(stat_1 ~ "**FPO households, n=417**",
                    stat_2 ~ "**Non-FPO households, n=402**") %>% 
      add_variable_grouping(
        "Advice source" = c("fpo_a_ap_q37_1",
                            "fpo_a_ap_q37_2"),
        "Type of advice" = c("fpo_a_ap_q36_1",
                            "fpo_a_ap_q36_2",
                            "fpo_a_ap_q36_3"),
        "Training source" = c("fpo_ap_q39_o1",
                             "fpo_ap_q39_o2"),
        "Inputs purchased" = c("fpo_a_ap_q41_1",
                              "fpo_a_ap_q41_2",
                              "fpo_a_ap_q41_3")
                             ) %>% 
      as_flex_table()
    
    tb.fpo_adv
    
    
# Supplemental files ####
    
    ## S1: Summary of missing data
    
    names(df.tab)
    
    # individual level
    miss_dem1 <- df.tab %>% 
      dplyr::select(a_fpo_hh,a_age,a_caste) %>% 
      tbl_summary(by=a_fpo_hh,missing = "no",
                  digits = all_continuous() ~ 1,
                  type = list(all_continuous() ~ "continuous"
                  ),
                  statistic = list(all_continuous() ~ c("{mean} ({sd})"),
                                   all_categorical() ~ c("{p}%, ({n})"))
      ) %>%
      modify_header(label ~ "****") %>%
      add_overall() %>% 
      add_n(statistic = "{p_miss}% ({N_miss})") %>%
      modify_column_hide(c("stat_0","stat_1","stat_2"))
    
    miss_dem1
    
    # household level
    df.tab2<- df.tab %>% group_by(a_hhid) %>% slice(1) %>% 
      filter(!(a_sex=="Female")) %>% droplevels() %>%  # There are 10 households were these questions were skipped because they were asked only from males %>% 
      select(a_fpo_hh,a_hhid,empowered_60,a_sex,
             he_q1_hhmem,land_cat,ag_landown_hc,ag_total_crops,tot_liv_lbl,ag_land_kh_hc,
             total_income_mon,
             he_q2_cult,he_q3_cult_amt_mon,
             he_q4_liv,he_q5_liv_amt_mon,
             he_q10_wage,he_q11_wage_amt_mon,
             he_q8_nonag,
             he_all_oth_inc_mon,mddw) %>% 
      ungroup()
    
    # Education by Sex
    df.tab.hh<- df.tab %>% select(a_sex,a_hhid,a_educ) %>% 
      pivot_wider(names_from = a_sex, values_from = a_educ, names_prefix = "a_educ_") 
    
    # Join for educ by male and female
    df.tab2<-left_join(df.tab2,df.tab.hh,by=c("a_hhid"))
    
    miss_dem2 <- df.tab2 %>% 
      dplyr::select(a_fpo_hh,he_q1_hhmem,a_educ_Male,a_educ_Female,a_sex,
                    land_cat,ag_landown_hc,ag_total_crops,ag_land_kh_hc,
                    tot_liv_lbl,
                    total_income_mon,
                    he_q2_cult,he_q3_cult_amt_mon,
                    he_q4_liv,he_q5_liv_amt_mon,
                    he_q10_wage,he_q11_wage_amt_mon,
                    he_q8_nonag,
                    he_all_oth_inc_mon,mddw,empowered_60) %>% 
      tbl_summary(by=a_fpo_hh,
                  digits = all_continuous() ~ 1,
                  type = list(all_continuous() ~ "continuous",
                              c(he_all_oth_inc_mon) ~"continuous"
                  ),
                  statistic = list(all_continuous() ~ c("{mean} ({sd})"),
                                   all_categorical() ~ c("{p}%, ({n})"))
      ) %>%
      add_overall() %>% 
      #modify_column_hide(c("stat_0","stat_1","stat_2")) %>% 
      add_n(statistic = "{p_miss}% ({N_miss})")

    miss_dem2
    
    # Missing table
    miss_dem<-tbl_stack(list(miss_dem1, miss_dem2)) %>% 
      as_gt() %>% 
        gt::tab_footnote(
      footnote="Notes: Values are % (n). Women's empowerment was calculated only for women hence 809 missing values for males")
    
    miss_dem
    
    miss_dem %>% 
    gtsave(paste0("05 Analysis/raw/","S1_",format(Sys.time(),"%d%m%Y"),".docx"))

    ## S2: FPO and Crop Diversity
    cd
    save_as_docx(cd,
                 path = paste0("05 Analysis/raw/","CropDiv_table_",format(Sys.time(),"%d%m%Y"),".docx")) 
    
    # S3: FPO and Income regression table was created in Stata (refer to Stata do-file)
    
    ## S4: FPO and diverse diet
    mddw1
    save_as_docx(mddw1,
                 path = paste0("05 Analysis/raw/","MDDW_table_",format(Sys.time(),"%d%m%Y"),".docx")) 
    
    ## S5: FPO and diversity score
    tb.mddw2
    save_as_docx(tb.mddw2,
                 path = paste0("05 Analysis/raw/","MDDWfig_table_",format(Sys.time(),"%d%m%Y"),".docx")) 
    
    ## S6: FPO, Diet and Caste
    dqq_fpo_caste<-df.tab %>% select(a_fpo_hh,fgds,mddw,a_caste,
                               dt_gourd,dt_cuc,dt_greenleaf,
                               dt_pap_ft,dt_ban_ft,dt_grap_ft,
                               dt_cheese,dt_curd,dt_milk,dt_fish,
                               dt_peanut,
                               dt_cake,dt_sweet,dt_chips,dt_noodles,
                               dt_snacks,dt_juice,dt_softdrink) %>% 
      filter(!(is.na(a_caste))) %>% 
      tbl_strata(strata = a_caste,
                 ~ .x %>%
      tbl_summary(by=a_fpo_hh,missing = "no",
                  digits = all_continuous() ~ 1,
                  type = list(all_continuous() ~ "continuous",c(fgds) ~ "continuous"
                  ),
                  statistic = list(all_continuous() ~ c("{mean} ({sd})"),
                                   all_categorical() ~ c("{p}% ({n})")),
                  label = c("mddw" ~ "Diverse diet (minimum dietary diversity score ≥5)",
                            "fgds" ~ "Minimum dietary diversity score",
                            "dt_gourd" ~ "Gourds",
                            "dt_cuc" ~ "Cucumber, capsicum, drumstick",
                            "dt_greenleaf" ~ "Green Leafs: mustard leaves,spinach,other greens",
                            "dt_pap_ft" ~ "Papaya, mango",
                            "dt_ban_ft" ~ "Banana, apple, watermelon",
                            "dt_grap_ft" ~ "Grapes, peaches, jackfruit",
                            "dt_cheese" ~ "Paneer",
                            "dt_curd" ~ "Curd",
                            "dt_milk" ~ "Milk",
                            "dt_fish" ~ "Fish",
                            "dt_peanut" ~ "Peanuts, cashews, almonds, pistachios, walnuts, pumpkin seeds, or sunflower seeds",
                            "dt_cake" ~ "Cake, biscuits, halwa, jalebi, ladoo",
                            "dt_sweet" ~ "Other mithai, kulfi, ice cream, shakes",
                            "dt_chips" ~ "Chips, namkeen",
                            "dt_noodles" ~ "Maggi noodles, wai wai",
                            "dt_snacks" ~ "Samosa, pakora, puri, vada",
                            "dt_juice" ~ "Fruit juice, frooti",
                            "dt_softdrink" ~ "Cold drinks"
                  )
      ) %>%
      modify_header(all_stat_cols() ~ "{level}\n {n}") %>%
      bold_labels() %>%
      add_variable_grouping(
        "Nutrituous foods" = c("dt_gourd",
                               "dt_cuc",
                               "dt_greenleaf",
                               "dt_pap_ft",
                               "dt_ban_ft",
                               "dt_grap_ft",
                               "dt_cheese",
                               "dt_curd",
                               "dt_milk",
                               "dt_fish",
                               "dt_peanut"),
        "Unhealthy foods" = c(
          "dt_cake",
          "dt_sweet",
          "dt_chips",
          "dt_noodles",
          "dt_snacks",
          "dt_juice",
          "dt_softdrink")
      )) %>% 
      as_flex_table()
    
    dqq_fpo_caste
    
    save_as_docx(dqq_fpo_caste,
                 path = paste0("05 Analysis/raw/","MDDW_Caste_",format(Sys.time(),"%d%m%Y"),".docx")) 
    
    ## S7: FPO and WEAI
    save_as_docx(weai,
                 path = paste0("05 Analysis/raw/","WEAI_table_",format(Sys.time(),"%d%m%Y"),".docx")) 
    
    ## S8: FPO, Caste, WEAI
    tb.weai_caste<-df.tab %>% filter(a_sex=="Female") %>% 
      filter(!(is.na(a_caste))) %>% 
      select(empowered_60,"feelinginputdecagr", "assetownership",
             "credjanydec_any","group_binary",
             "incdec_count","npoor_z105",a_fpo_hh,a_caste) %>% 
      tbl_strata(strata = a_caste,
                 ~ .x %>%
      tbl_summary(by="a_fpo_hh",missing = "no",
                  type = list(c("feelinginputdecagr", "assetownership",
                                "credjanydec_any","group_binary",
                                "incdec_count","npoor_z105") ~ "categorical"),
                  statistic = all_categorical() ~ c("{p}% ({n})"),
                  label = c("empowered_60" ~ "Overall women’s empowerment",
                            "feelinginputdecagr" ~ "Input in productive decisions",
                            "assetownership" ~ "Ownership of assets",
                            "credjanydec_any" ~ "Access to and decisions about credit",
                            "incdec_count" ~ "Control over use of income",
                            "group_binary" ~ "Self-help group membership",
                            "npoor_z105" ~ "Work balance")
      ) 
      ) %>%
      as_flex_table()
    tb.weai_caste
    
    save_as_docx(tb.weai_caste,
                 path = paste0("05 Analysis/raw/","tb.weai_caste_",format(Sys.time(),"%d%m%Y"),".docx"))
    
    ## S9: FPO advice
    save_as_docx("FPO_advice"=tb.fpo_adv,
                 path = paste0("05 Analysis/raw/","FPO_advice_",format(Sys.time(),"%d%m%Y"),".docx"))



# End of code ####   
