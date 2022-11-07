### pre-amble
rm(list=ls())
library(tidyverse)
library(lubridate)
library(zoo)
library(pracma)
library(networkD3)
set.seed(12345)
setwd("~/Study_Data/1516-0489/Research/Holly/Data")

### Generic Drugs
med_SABA<-c("SALBUTAMOL")
med_LABA<-c("BAMBUTEROL", "FORMOTEROL", "SALMETEROL", "TERBUTALINE", "TIOTROPIUM")
med_LAMA<-c("IPRATROPIUM")
med_Theophylline<-c("THEOPHYLLINE", "AMINOPHYLLINE")
med_ICS<-c("BECLOMETASONE", "BUDESONIDE", "FLUTICASONE", 
           "MOMETASONE", "CICLESONIDE")
med_LTRA<-c("MONTELUKAST", "ZAFIRLUKAST", "CROMOLYN", "NEDOCROMIL")
### List of all medicine keywords
med_list<-substr(ls()[which(substr(ls(),1,3)=="med")],5,30)

### Brand named drugs
brand_SALBUTAMOL<-c("SALAMOL", "VENTOLIN", "AIROMIR", "SALBULIN",
                    "AIRSALB", "VENTMAX", "ASMASAL", 
                    "PULVINAL SALBUTAMOL","ALBUTEROL", "IPRAMOL", 
                    "COMBIVENT")
brand_BAMBUTEROL<-c("BAMBEC")
brand_FORMOTEROL<-c("ATIMOS", "FORADIL", "OXIS", "FOSTAIR",
                    "SYMBICORT", "DUORESP SPIROMAX", "FLUTIFORM")
brand_SALMETEROL<-c("NEOVENT", "SEREVENT", "SERETIDE", "AIRFLUSAL", "SIRDUPLA")
brand_TERBUTALINE<-c("BRICANYL")
brand_TIOTROPIUM<-c("SPIRIVA RESPIMAT")
brand_IPRATROPIUM<-c("ATROVENT", "INHALVENT", "IPRAVENT", "RESPONTIN", "IPRAMOL", 
                     "COMBIVENT")
brand_THEOPHYLLINE<-c("NUELIN", "SLOPHYLLIN", "SLO-PHYLLIN","UNIPHYLLIN")
brand_AMINOPHYLLINE<-c("PHYLLOCONTIN")
brand_BECLOMETASONE<-c("BECLOMETHASONE","CLENIL", "QVAR","AEROBEC","BECLAZONE","FILAIR",
                       "BECODISKS", "PULVINAL BECLOMETASONE", "ASMABEC","FOSTAIR")
brand_BUDESONIDE<-c("BUDELIN", "PULMICORT","SYMBICORT", "DUORESP SPIROMAX")
brand_MOMETASONE<-c("ASMANEX")
brand_FLUTICASONE<-c("FLIXOTIDE","FLUTIFORM", "SERETIDE", "AIRFLUSAL", 
                     "SIRDUPLA", "RELVAR ELLIPTA")
brand_CICLESONIDE<-c("ALVESCO")
brand_MONTELUKAST<-c("SINGULAIR")
brand_ZAFIRLUKAST<-c("ACCOLATE")
brand_CROMOLYN<-c("CROMOGLICATE", "INTAL")
brand_NEDOCROMIL<-c("TILADE")

### List of all medicine keywords
brand_list<-substr(ls()[which(substr(ls(),1,5)=="brand")],7,30)

##########################################################################
###  Record Cleaning 
##########################################################################

prescribing <- read.csv("~/Study_Data/1516-0489/Linked Data/PIS/20190919 amended_pis.csv", stringsAsFactors=FALSE)
names(prescribing)[names(prescribing)=="Index10"]<-"ID"
length(unique(prescribing$ID))

### data cleaning
prescribing$PrescDate<-as.Date(prescribing$PrescDate)
prescribing$DispDate<-as.Date(prescribing$DispDate)
sum(is.na(prescribing$PrescDate) | is.na(prescribing$DispDate)) # no missing dates
min(prescribing$PrescDate)
max(prescribing$PrescDate)
prescribing<-prescribing[which((format(prescribing$PrescDate,"%Y")!="2017" |
                                 format(prescribing$PrescDate,"%m")!="04") &
                                 as.numeric(format(prescribing$PrescDate,"%Y"))>=2009),]

### Load in the dataset which contains all dose information
dose <- read.csv("~/Study_Data/1516-0489/Linked Data/PIS/1516-0489_Final_dose_instructions_and_variables2.csv", stringsAsFactors=FALSE)
prescribing<-left_join(prescribing,dose)
rm(dose)

prescribing$dose<-toupper(prescribing$ePRNativeDoseInstructions)
prescribing$drugname<-ifelse(prescribing$PIApprovedName==prescribing$PIPrescribableItemName,
                                    prescribing$PIPrescribableItemName,
                                    paste(prescribing$PIPrescribableItemName,prescribing$PIApprovedName))
prescribing<-prescribing %>%
  select(-ePRNativeDoseInstructions,-PIApprovedName,-PIPrescribableItemName)

# Some have been deleted
prescribing<-prescribing[which(substr(prescribing$dose,1,7)!="DELETED"),]

##########################################################################
###   Drug Classification
##########################################################################

presc_unique<-prescribing %>% 
  select(drugname,dose,ePRNDName,PIDrugFormulation) %>% 
  add_count(drugname,dose,ePRNDName,PIDrugFormulation) %>%
  distinct() %>%
  arrange(-n)

# Identifying and assigning drug inclusion keywords
presc_unique$key<-""
for (med in med_list) {
  filter<-vapply(get(paste0("med_",med)),
                 function(x) str_detect(presc_unique$drugname, x), 
                 logical(nrow(presc_unique)))
  presc_unique[,paste0("flag_",med)]<-apply(filter,1,sum)
  for (medx in get(paste0("med_",med))) {
    filter<-vapply(get(paste0("brand_",medx)),
                   function(x) str_detect(presc_unique$drugname, x), 
                   logical(nrow(presc_unique)))
    presc_unique[,paste0("flag_",med)]<-(presc_unique[,paste0("flag_",med)]+apply(filter,1,sum))>0
    temp<-str_detect(presc_unique$drugname,medx) | as.logical(apply(filter,1,sum))
    presc_unique[temp,"key"]<-paste(presc_unique$key[temp],medx,sep="_")
  }
}
presc_unique$key<-ifelse(str_detect(presc_unique$drugname,"RELVAR ELLIPTA") |
                           str_detect(presc_unique$drugname,"VILANTEROL"),
                         paste(presc_unique$key,"VILANTEROL",sep="_"),
                         presc_unique$key)
presc_unique$key<-ifelse(str_detect(presc_unique$drugname,"MOXISLYTE"),
                         "_",
                         presc_unique$key)
presc_unique$key<-str_replace(substr(presc_unique$key,2,30)," ","_")

## How many flags does each prescription have?
presc_unique$flag_sum<-rowSums(presc_unique[,c("flag_LAMA","flag_ICS","flag_LABA", 
                                               "flag_LTRA", "flag_SABA", "flag_Theophylline")])

### Drop if they aren't in any of these categories
presc_unique<-presc_unique[which(presc_unique$flag_sum!=0),]
sum(presc_unique$n)

### code the drug class in cases with only one flag
presc_unique$drug_class<-NA
for (med in med_list) {
  presc_unique$drug_class<-ifelse(presc_unique[,paste0("flag_",med)]==T & presc_unique$flag_sum==1,
                                  med,
                                  presc_unique$drug_class) 
}

# Code the two-flag cases
presc_unique$drug_class<-ifelse(presc_unique$flag_ICS==T & 
                                  presc_unique$flag_LABA==T,
                                "ICS+LABA",presc_unique$drug_class)
presc_unique$drug_class<-ifelse(presc_unique$flag_LAMA==T & 
                                  presc_unique$flag_SABA==T,"LAMA",presc_unique$drug_class)

# drop sprays
sum(presc_unique %>% filter(PIDrugFormulation=="SPRAY") %>% select(n))
presc_unique<-presc_unique %>% filter(PIDrugFormulation!="SPRAY")
# drop drops
sum(presc_unique %>% filter(PIDrugFormulation=="DROPS") %>% select(n))
presc_unique<-presc_unique %>% filter(PIDrugFormulation!="DROPS")

## Getting rid of records matching formulation exclusion keywords
sum(presc_unique$n)
for (keyword in c("NASAL","NOSE","NOSTRIL","NASULE", "HAYFEVER",
                  "EYE","EAR","DROP","TONGUE", "FOAM","ENEMA", 
                  "RECTAL","SUPPOSITOR", "CREAM",
                  "OINTMENT", "ULCER","SKIN","PATCH","APPLY")) {
  presc_unique<-presc_unique %>%
    filter(str_detect(dose,keyword)==F & 
             str_detect(drugname,keyword)==F &
             str_detect(ePRNDName,keyword)==F)
}
sum(presc_unique$n)

# Removing records matching exclusion brand names 
sum(presc_unique$n)
for (brand in c("NASONEX","FLIXONASE","ANORO ELLIPTA","SUMATRIPTAN","AVAMYS",
                "RHINOCORT","NASOBEC","NASOFAN","RYNACROM","PIRINASE","SPIOLTO",
                "DYMISTA","POLLENASE","VIVIDRIN","DUAKLIR","SEEBRI", "ULTIBRO", 
                "PRED FORTE", "TRELEGY", "TRIMBOW", "BRALTUS", "RINATEC", 
                "ENTOCORT", "BENACORT", "AIRCORT", "BUDEFLAM", "BUDENOFALK",
                "CORTIMENT", "JORVEZA", "AZELASTINE", "CUTIVATE", "ELOCON", 
                "NALCROM", "CATACROM", "ASPIRE", "OPTICROM", "OPTREX", "BECONASE",
                "MURINE", "ACLIDINIUM", "GENUAIR", "OLADATEROL", "YANIMO")) {
  #print(presc_unique %>% filter(str_detect(presc_unique$drugname,brand)) %>% select(drugname))
  presc_unique<-presc_unique[which(str_detect(presc_unique$drugname,brand)==F),]
}
sum(presc_unique$n)

## Flagging brand names
presc_unique$brandname<-""
for (med in brand_list) {
  for (brand in get(paste0("brand_",med))) {
    presc_unique$brandname <- ifelse(str_detect(presc_unique$drugname, brand)==T,
                                     brand,
                                     presc_unique$brandname)
  }
}
presc_unique$brandname <-ifelse(presc_unique$brandname %in% c("","BECLOMETHASONE","CROMOGLICATE"),
                                "",
                                presc_unique$brandname)

### Changing the drug class of solutions
filters_solution<-vapply(c("SACHET", "RESPULE", "NEB", "VIAL", "AMPOULE"),
                         function(x) str_detect(paste0(presc_unique$dose,
                                                       presc_unique$drugname,
                                                       presc_unique$ePRNDName),x), 
                         logical(nrow(presc_unique)))


sum(presc_unique %>% filter(drug_class=="ICS") %>% select(n))
presc_unique<-presc_unique %>%
  mutate(solution=apply(filters_solution,1,sum),
         drug_class=ifelse(drug_class=="ICS" & (is.na(solution) | solution>0 | PIDrugFormulation=="SOL"),
                           "ICS_SOL",
                           drug_class)) %>%
  select(-solution)
sum(presc_unique %>% filter(drug_class=="ICS") %>% select(n))

prescribing_asthma<-inner_join(prescribing,presc_unique) %>%
  dplyr::select(ID,key,PrescDate,drugname,ePRNDName,PIItemStrength.UOM,prescribed_quantity,
                dispensed_quantity,drug_class,ndx,brandname,dose,PIDrugFormulation) %>%
  ungroup
rm(presc_unique)
length(unique(prescribing_asthma$ID))

##########################################################################
###  ICS daily medicine amount used calculation
##########################################################################

prescribing_asthma_ICS<-prescribing_asthma %>%
  filter(drug_class %in% c("ICS","ICS+LABA"))
rownames(prescribing_asthma_ICS)<-NULL

prescribing_asthma<-prescribing_asthma %>%
  filter(!drug_class %in% c("ICS","ICS+LABA"))

### Dose Frequency Keywords
dose_freq_one<-c("ONCE","O-D","O\\.D")
dose_freq_two<-c("TWICE","TWO TIMES","2 TIMES","TD","TID","BID","BD",
                 "B-D","B\\.D")
dose_freq_four<-c("QID","FOUR TIMES","4 TIMES")
dose_freq_daily<-c("DAILY","EVERY DAY","EACH DAY",
                   "MANE","NOCTE","MORN","NIGHT","EVE","BEDTIME",
                   "A\\.M","P\\.M","AM","PM")

# Dose frequency - as required/needed coded as minimum
filters_freq_one<-vapply(dose_freq_one,function(x) str_detect(prescribing_asthma_ICS$dose,x),
                         logical(nrow(prescribing_asthma_ICS)))
filters_freq_two<-vapply(dose_freq_two,function(x) str_detect(prescribing_asthma_ICS$dose,x),
                         logical(nrow(prescribing_asthma_ICS)))
filters_freq_four<-vapply(dose_freq_four,function(x) str_detect(prescribing_asthma_ICS$dose,x),
                          logical(nrow(prescribing_asthma_ICS)))
filters_freq_daily<-vapply(dose_freq_daily,function(x) str_detect(prescribing_asthma_ICS$dose,x),
                           logical(nrow(prescribing_asthma_ICS)))
prescribing_asthma_ICS$freq_dose<-ifelse(apply(filters_freq_one,1,sum)>=1,1,NA)
prescribing_asthma_ICS$freq_dose<-ifelse(is.na(prescribing_asthma_ICS$freq_dose) &
                                           (apply(filters_freq_two,1,sum)>=1 |
                                              ((str_detect(prescribing_asthma_ICS$dose, "MORN") |
                                                  str_detect(prescribing_asthma_ICS$dose, "AM") |
                                                  str_detect(prescribing_asthma_ICS$dose, "A.M") |
                                                  str_detect(prescribing_asthma_ICS$dose, "MANE")) &
                                                 (str_detect(prescribing_asthma_ICS$dose, "EVE") |
                                                    str_detect(prescribing_asthma_ICS$dose, "NIGHT") |
                                                    str_detect(prescribing_asthma_ICS$dose, "BEDTIME") |
                                                    str_detect(prescribing_asthma_ICS$dose, "PM") |
                                                    str_detect(prescribing_asthma_ICS$dose, "P.M") |
                                                    str_detect(prescribing_asthma_ICS$dose, "NOCTE")))),
                                         2,prescribing_asthma_ICS$freq_dose)
prescribing_asthma_ICS$freq_dose<-ifelse(apply(filters_freq_four,1,sum)>=1,4,prescribing_asthma_ICS$freq_dose)
prescribing_asthma_ICS$freq_dose<-ifelse(is.na(prescribing_asthma_ICS$freq_dose),
                                         ifelse(apply(filters_freq_daily,1,sum)>=1,
                                                1,
                                                prescribing_asthma_ICS$freq_dose),
                                         prescribing_asthma_ICS$freq_dose)

table(prescribing_asthma_ICS$freq_dose, useNA = "ifany")*100/nrow(prescribing_asthma_ICS)
sum(is.na(prescribing_asthma_ICS$freq_dose) & prescribing_asthma_ICS$key %in% c("CICLESONIDE","FLUTICASONE_VILANTEROL"))
sum(is.na(prescribing_asthma_ICS$freq_dose) & !prescribing_asthma_ICS$key %in% c("CICLESONIDE","FLUTICASONE_VILANTEROL"))
prescribing_asthma_ICS$freq_dose<-ifelse(is.na(prescribing_asthma_ICS$freq_dose),
                                         ifelse(prescribing_asthma_ICS$key %in% c("CICLESONIDE","FLUTICASONE_VILANTEROL"),
                                                1,2),
                                         prescribing_asthma_ICS$freq_dose)
table(prescribing_asthma_ICS$freq_dose, useNA = "ifany")*100/nrow(prescribing_asthma_ICS)

## Dose Quantity
prescribing_asthma_ICS$daily_dose<-NA
dose_quantities<-c("1","2","3","4","ONE","TWO","THREE","FOUR")
dose_quantities_num<-rep(c(1,2,3,4),2)
for (k in 1:8) {
  prescribing_asthma_ICS$daily_dose<-ifelse(str_detect(prescribing_asthma_ICS$dose,
                                                       paste0("TAKE ",dose_quantities[k]))==TRUE |
                                              str_detect(prescribing_asthma_ICS$dose,
                                                         paste0("INHALE ",dose_quantities[k]))==TRUE |
                                              str_detect(prescribing_asthma_ICS$dose,
                                                         paste0(dose_quantities[k]," AT "))==TRUE |
                                              str_detect(prescribing_asthma_ICS$dose,
                                                         paste0(dose_quantities[k]," TO BE TAKEN "))==TRUE |
                                              str_detect(prescribing_asthma_ICS$dose,
                                                         paste0(dose_quantities[k],"PUF"))==TRUE |
                                              str_detect(prescribing_asthma_ICS$dose,
                                                         paste0(dose_quantities[k]," PUF"))==TRUE |
                                              str_detect(prescribing_asthma_ICS$dose,
                                                         paste0(dose_quantities[k]," P "))==TRUE |
                                              str_detect(prescribing_asthma_ICS$dose,
                                                         paste0(dose_quantities[k],"P "))==TRUE |
                                              str_detect(prescribing_asthma_ICS$dose,
                                                         paste0(dose_quantities[k]," DAILY"))==TRUE,
                                            dose_quantities_num[k],
                                            prescribing_asthma_ICS$daily_dose)
  for (keyword in c("PUF","DOSE","CLICK","BLISTER", "TAB", "SACHET", "NEB", "RESP",
                    "VIAL","CAP","INHALATION","AMPOULE","DOSE","ACTUATION","TWIST")) {
    prescribing_asthma_ICS$daily_dose<-ifelse(str_detect(prescribing_asthma_ICS$dose,
                                                         paste0(dose_quantities[k]," ",keyword))==TRUE,
                                              dose_quantities_num[k],
                                              prescribing_asthma_ICS$daily_dose)
  }
}
table(prescribing_asthma_ICS$daily_dose, useNA = "ifany")*100/nrow(prescribing_asthma_ICS)
list<-c("BUDESONIDE","CICLESONIDE","FLUTICASONE_SALMETEROL","FLUTICASONE_VILANTEROL","MOMETASONE")
sum(is.na(prescribing_asthma_ICS$daily_dose) & prescribing_asthma_ICS$key %in% list)
sum(is.na(prescribing_asthma_ICS$daily_dose) & !prescribing_asthma_ICS$key %in% list)
prescribing_asthma_ICS$daily_dose<-ifelse(is.na(prescribing_asthma_ICS$daily_dose),
                                          ifelse(prescribing_asthma_ICS$key %in% list,
                                                 1,
                                                 2),
                                          prescribing_asthma_ICS$daily_dose)
table(prescribing_asthma_ICS$daily_dose, useNA = "ifany")*100/nrow(prescribing_asthma_ICS)

## How much do they take per day
prescribing_asthma_ICS$daily_dose_units<-prescribing_asthma_ICS$freq_dose*prescribing_asthma_ICS$daily_dose

##########################################################################
###  Medication Strength
##########################################################################

### Doses
doses_mcg<-c("10000","5000","4000","2000","1000","500","400",
             "320","250","200","184","160","125","100","92","80","65","50")
doses_mg<-c("0.5","20","10","5","4","2","1")

## Look for the values followed by keywords
prescribing_asthma_ICS$strength<-NA
for (k in doses_mcg) {
  prescribing_asthma_ICS$strength<-ifelse(is.na(prescribing_asthma_ICS$strength) &
                                            ((str_detect(prescribing_asthma_ICS$ePRNDName, paste0(k, "/"))==T &
                                                prescribing_asthma_ICS$drug_class=="ICS+LABA") |
                                               str_detect(prescribing_asthma_ICS$ePRNDName, paste0(k, "MCG"))==T |
                                               str_detect(prescribing_asthma_ICS$ePRNDName, paste(k, "MCG", sep=" "))==T |
                                               str_detect(prescribing_asthma_ICS$ePRNDName, paste(k, "MICROGRAM", sep=" "))==T |
                                               str_detect(prescribing_asthma_ICS$ePRNDName, paste0(k, "MICROGRAM"))==T),
                                          as.numeric(k),
                                          prescribing_asthma_ICS$strength)
}
for (k in doses_mg) {
  prescribing_asthma_ICS$strength<-ifelse(is.na(prescribing_asthma_ICS$strength) &
                                            (str_detect(prescribing_asthma_ICS$ePRNDName, paste0(k, "MG"))==T |
                                               str_detect(prescribing_asthma_ICS$ePRNDName, paste(k, "MG", sep=" "))==T |
                                               str_detect(prescribing_asthma_ICS$ePRNDName, paste0(k, "MILLIGRAM"))==T |
                                               str_detect(prescribing_asthma_ICS$ePRNDName, paste(k, "MILLIGRAM", sep=" "))==T),
                                          as.numeric(k)*1000,
                                          prescribing_asthma_ICS$strength)
}

# from manual comparison against our appendix dose strength table
nrow(prescribing_asthma_ICS)
prescribing_asthma_ICS<-prescribing_asthma_ICS %>%
  filter(!(key=="BECLOMETASONE" & !is.na(strength) & !strength %in% c(50,100,200,250,400))) %>%
  filter(!(key=="BECLOMETASONE_FORMOTEROL" & !is.na(strength) & !strength %in% c(100,200))) %>%
  filter(!(key=="BUDESONIDE" & !is.na(strength) & !strength %in% c(100,200,250,400))) %>%
  filter(!(key=="BUDESONIDE_FORMOTEROL" & !is.na(strength) & !strength %in% c(50,100,160,200,320,400))) %>%
  filter(!(key=="FLUTICASONE" & !is.na(strength) & !strength %in% c(50,100,125,250,500))) %>%
  filter(!(key=="FLUTICASONE_FORMOTEROL" & !is.na(strength) & !strength %in% c(50,125,250))) %>%
  filter(!(key=="FLUTICASONE_SALMETEROL" & !is.na(strength) & !strength %in% c(50,100,125,250,500))) %>%
  filter(!(key=="FLUTICASONE_VILANTEROL" & !is.na(strength) & !strength %in% c(92,184))) %>%
  filter(!(key=="MOMETASONE" & !is.na(strength) & !strength %in% c(200,400))) 
nrow(prescribing_asthma_ICS)
length(unique(prescribing_asthma_ICS$ID))
sum(is.na(prescribing_asthma_ICS$strength))*100/nrow(prescribing_asthma_ICS)

# mostly accuhalers and evohalers etc
for (k in doses_mcg) {
  prescribing_asthma_ICS$strength<-ifelse(is.na(prescribing_asthma_ICS$strength) &
                                            (str_detect(prescribing_asthma_ICS$ePRNDName, paste0(k," ACCUHALER")) |
                                               str_detect(prescribing_asthma_ICS$ePRNDName, paste0(k," CLICKHALER")) |
                                               str_detect(prescribing_asthma_ICS$ePRNDName, paste0(k," EVOHALER")) |
                                               str_detect(prescribing_asthma_ICS$ePRNDName, paste0(k," TURBOHALER")) |
                                               str_detect(prescribing_asthma_ICS$ePRNDName, paste0("QVAR ",k)) |
                                               str_detect(prescribing_asthma_ICS$ePRNDName, paste0("SERETIDE ",k)) |
                                               str_detect(prescribing_asthma_ICS$ePRNDName, paste0("INHAL ",k)) |
                                               str_detect(prescribing_asthma_ICS$ePRNDName, paste0("EVOHALER ",k)) |
                                               str_detect(prescribing_asthma_ICS$ePRNDName, paste0("SERETIDE MDI ",k)) |
                                               str_detect(prescribing_asthma_ICS$ePRNDName, paste0("SERETIDE",k)) |
                                               str_detect(prescribing_asthma_ICS$ePRNDName, paste0("ALVESCO ",k))),
                                          as.numeric(k),
                                          prescribing_asthma_ICS$strength)
}
sum(is.na(prescribing_asthma_ICS$strength))*100/nrow(prescribing_asthma_ICS)


mode_strength_key<-prescribing_asthma_ICS %>%
  filter(!is.na(strength)) %>%
  dplyr::count(key,strength) %>%
  arrange(key,n) %>%
  group_by(key) %>%
  dplyr::slice(n()) %>%
  dplyr::rename(mode_strength = strength) %>%
  select(-n)
prescribing_asthma_ICS<-left_join(prescribing_asthma_ICS,mode_strength_key) %>%
  mutate(strength = ifelse(is.na(strength),mode_strength,strength)) %>%
  select(-mode_strength)
rm(mode_strength_key)

rm(list=setdiff(ls(),c("prescribing_asthma_ICS","prescribing_asthma")))

##########################################################################
###  ICS Dose Category
##########################################################################

## How much do they take per day
prescribing_asthma_ICS$daily_dose_max<-prescribing_asthma_ICS$daily_dose_units*prescribing_asthma_ICS$strength

prescribing_asthma_ICS$dose_category<-ifelse((prescribing_asthma_ICS$key %in% 
                                                c("BUDESONIDE","BECLOMETASONE") & 
                                                prescribing_asthma_ICS$brandname=="") |
                                               prescribing_asthma_ICS$brandname %in%
                                               c("PULMICORT","SYMBICORT","CLENIL","BECODISKS",
                                                 "BECLAZONE","AEROBEC", "FILAIR"),
                                             ifelse(prescribing_asthma_ICS$daily_dose_max<=400,"low",
                                                    ifelse(prescribing_asthma_ICS$daily_dose_max<=800,"medium",
                                                           ifelse(prescribing_asthma_ICS$daily_dose_max<=3200,"high",
                                                                  "unknown"))),
                                             NA)

prescribing_asthma_ICS$dose_category<-ifelse((prescribing_asthma_ICS$brandname=="" & 
                                                prescribing_asthma_ICS$key %in% 
                                                c("BECLOMETASONE_FORMOTEROL")) |
                                               prescribing_asthma_ICS$brandname %in%
                                               c("QVAR","FOSTAIR","PULVINAL BECLOMETASONE"),
                                             ifelse(prescribing_asthma_ICS$daily_dose_max<=200,"low",
                                                    ifelse(prescribing_asthma_ICS$daily_dose_max<=400,"medium",
                                                           ifelse(prescribing_asthma_ICS$daily_dose_max<=1600,"high",
                                                                  "unknown"))),
                                             prescribing_asthma_ICS$dose_category)

prescribing_asthma_ICS$dose_category<-ifelse(prescribing_asthma_ICS$key=="CICLESONIDE",
                                             ifelse(prescribing_asthma_ICS$daily_dose_max<=160,"low",
                                                    ifelse(prescribing_asthma_ICS$daily_dose_max<=320,"medium",
                                                           "unknown")),
                                             prescribing_asthma_ICS$dose_category)

prescribing_asthma_ICS$dose_category<-ifelse((prescribing_asthma_ICS$brandname=="" & 
                                                prescribing_asthma_ICS$key %in% 
                                                c("FLUTICASONE","FLUTICASONE_SALMETEROL",
                                                  "FLUTICASONE_FORMOTEROL")) |
                                               prescribing_asthma_ICS$brandname %in%
                                               c("FLIXOTIDE","FLUTIFORM","SERETIDE"),
                                             ifelse(prescribing_asthma_ICS$daily_dose_max<=200,"low",
                                                    ifelse(prescribing_asthma_ICS$daily_dose_max<=500,"medium",
                                                           ifelse(prescribing_asthma_ICS$daily_dose_max<=2000,"high",
                                                                  "unknown"))),
                                             prescribing_asthma_ICS$dose_category)

prescribing_asthma_ICS$dose_category<-ifelse(prescribing_asthma_ICS$key=="FLUTICASONE_VILANTEROL",
                                             ifelse(prescribing_asthma_ICS$daily_dose_max<46 |
                                                      prescribing_asthma_ICS$daily_dose_max>368,
                                                    "unknown",
                                                    ifelse(prescribing_asthma_ICS$daily_dose_max<=92,
                                                           "medium",
                                                           "high")),
                                             prescribing_asthma_ICS$dose_category)

prescribing_asthma_ICS$dose_category<-ifelse(prescribing_asthma_ICS$brandname=="ASMABEC",
                                             ifelse(prescribing_asthma_ICS$daily_dose_max<=200,"low",
                                                    ifelse(prescribing_asthma_ICS$daily_dose_max<=400,"medium",
                                                           "unknown")),
                                             prescribing_asthma_ICS$dose_category)

prescribing_asthma_ICS$dose_category<-ifelse(prescribing_asthma_ICS$brandname=="BUDELIN",
                                             ifelse(prescribing_asthma_ICS$daily_dose_max<400 |
                                                      prescribing_asthma_ICS$daily_dose_max>3200,
                                                    "unknown",
                                                    ifelse(prescribing_asthma_ICS$daily_dose_max<=800,
                                                           "medium",
                                                           "high")),
                                             prescribing_asthma_ICS$dose_category)

prescribing_asthma_ICS$dose_category<-ifelse(prescribing_asthma_ICS$key=="MOMETASONE",
                                             ifelse(prescribing_asthma_ICS$daily_dose_max<=400,"low",
                                                    ifelse(prescribing_asthma_ICS$daily_dose_max<=800,"medium",
                                                           "unknown")),
                                             prescribing_asthma_ICS$dose_category)

prescribing_asthma_ICS$dose_category<-ifelse(prescribing_asthma_ICS$brandname=="SIRDUPLA",
                                             ifelse(prescribing_asthma_ICS$daily_dose_max<250 |
                                                      prescribing_asthma_ICS$daily_dose_max>2000,
                                                    "unknown",
                                                    ifelse(prescribing_asthma_ICS$daily_dose_max<=500,
                                                           "medium",
                                                           "high")),
                                             prescribing_asthma_ICS$dose_category)

prescribing_asthma_ICS$dose_category<-ifelse(prescribing_asthma_ICS$brandname=="AIRFLUSAL",
                                             ifelse(prescribing_asthma_ICS$daily_dose_max<500 |
                                                      prescribing_asthma_ICS$daily_dose_max>2000,
                                                    "unknown",
                                                    "high"),
                                             prescribing_asthma_ICS$dose_category)

prescribing_asthma_ICS$dose_category<-ifelse(prescribing_asthma_ICS$key=="BUDESONIDE_FORMOTEROL",
                                             ifelse(prescribing_asthma_ICS$daily_dose_max<=320,
                                                    "low",
                                                    ifelse(prescribing_asthma_ICS$daily_dose_max<=640,
                                                           "medium",
                                                           ifelse(prescribing_asthma_ICS$daily_dose_max<=2560,
                                                                  "high",
                                                                  "unknown"))),
                                             prescribing_asthma_ICS$dose_category)

sum(is.na(prescribing_asthma_ICS$dose_category))

table<-as.data.frame(prescribing_asthma_ICS %>% count(key,dose_category) %>%
                       group_by(key)  %>% 
                       mutate(prop = n*100/sum(n),
                              dose_category=str_to_title(dose_category),
                              key = str_replace(key,"_","+")) %>%
                       select(-n))
table$dose_category<-factor(table$dose_category,levels=c("Low","Medium","High","Unknown"))
ggplot(table) + 
  geom_bar(aes(x=key, y=prop,fill=dose_category),
           position="stack",stat="identity")  +
  coord_flip() + xlab("Drug") + ylab("Proportion") +
  labs(fill="Dose Category") + theme_bw()


### add the ICS/ICS+LABA into the not ICS records, rather than merging
### as some ICS/ICS+LABA records have been removed here
prescribing_asthma<-bind_rows(prescribing_asthma,prescribing_asthma_ICS)
rm(prescribing_asthma_ICS)
prescribing_asthma<-prescribing_asthma %>% 
  select(ID,PrescDate,ePRNDName,key,brandname,dose,dose_category,drug_class)

table<-as.data.frame(prescribing_asthma %>% count(drug_class,key,brandname))

##########################################################################
###  Regimens
##########################################################################

### grace period of previous prescriptions
grace<-120

### how long has it been since your last prescription of each stage?
prescribing_asthma<-prescribing_asthma %>%
  group_by(ID) %>%
  arrange(ID,PrescDate) %>%
  mutate(last_ICS=replace_na(as.numeric(PrescDate-na.locf(as.Date(ifelse(drug_class=="ICS",
                                                                  PrescDate,NA)),na.rm=F)),9999),
         last_combo=replace_na(as.numeric(PrescDate-na.locf(as.Date(ifelse(drug_class=="ICS+LABA",
                                                                           PrescDate,NA)),na.rm=F)),9999),
         last_ICS_low=replace_na(as.numeric(PrescDate-na.locf(as.Date(ifelse(drug_class %in% c("ICS","ICS+LABA") &
                                                                                dose_category=="low",
                                                                              PrescDate,NA)),na.rm=F)),9999),
         last_ICS_medium=replace_na(as.numeric(PrescDate-na.locf(as.Date(ifelse(drug_class %in% c("ICS","ICS+LABA") &
                                                                               dose_category=="medium",
                                                                              PrescDate,NA)),na.rm=F)),9999),
         last_ICS_high=replace_na(as.numeric(PrescDate-na.locf(as.Date(ifelse(drug_class %in% c("ICS","ICS+LABA") &
                                                                                dose_category=="high",
                                                                              PrescDate,NA)),na.rm=F)),9999),
         last_ICS_dose=ifelse(last_ICS_low<last_ICS_high & last_ICS_low<last_ICS_medium,"low",
                              ifelse(last_ICS_medium<last_ICS_high,"medium","high")),
         last_LTRA=replace_na(as.numeric(PrescDate-na.locf(as.Date(ifelse(drug_class=="LTRA",
                                                                         PrescDate,NA)),na.rm=F)),9999),
         last_LABA=replace_na(as.numeric(PrescDate-na.locf(as.Date(ifelse(drug_class=="LABA",
                                                                          PrescDate,NA)),na.rm=F)),9999),
         last_LAMA=replace_na(as.numeric(PrescDate-na.locf(as.Date(ifelse(drug_class=="LAMA",
                                                                         PrescDate,NA)),na.rm=F)),9999),
         last_Theo=replace_na(as.numeric(PrescDate-na.locf(as.Date(ifelse(drug_class=="Theophylline",
                                                                          PrescDate,NA)),na.rm=F)),9999),
         last_ICS_SOL=replace_na(as.numeric(PrescDate-na.locf(as.Date(ifelse(drug_class=="ICS_SOL",
                                                                             PrescDate,NA)),na.rm=F)),9999))



prescribing_asthma<-prescribing_asthma %>% 
  mutate(regimen = ifelse(last_ICS<=grace | last_combo<=grace,
                          paste0("ICS","_",last_ICS_dose),
                          "")) %>%
  mutate(regimen = ifelse(last_combo<=grace | last_LABA<=grace,
                          ifelse(last_ICS<=grace | last_combo<=grace,
                                 paste0(regimen,"_LABA"),
                                 "LABA"),
                          regimen)) %>%
  mutate(regimen = ifelse(last_ICS_SOL<=grace,
                           ifelse(regimen=="",
                                  "ICSSOL",
                                  paste0(regimen,"_ICSSOL")),
                           regimen)) %>%
  mutate(regimen  = ifelse(last_LTRA<=grace,
                            ifelse(regimen=="",
                                   "LTRA",
                                   paste0(regimen,"_LTRA")),
                            regimen)) %>%
  mutate(regimen  = ifelse(last_LAMA<=grace,
                           ifelse(regimen=="",
                                  "LAMA",
                                  paste0(regimen,"_LAMA")),
                           regimen)) %>%
  mutate(regimen  = ifelse(last_Theo<=grace,
                           ifelse(regimen=="",
                                  "Theo",
                                  paste0(regimen,"_Theo")),
                           regimen)) %>%
  mutate(regimen = ifelse(drug_class=="SABA" & regimen=="",
                          "SABA",regimen))
                                       

##########################################################################
###  BTS Steps
##########################################################################

### Number of add-on therapies in last grace period
prescribing_asthma<-prescribing_asthma %>%
  mutate(number_add_on=(as.numeric(last_LAMA<=grace)+ 
                          as.numeric(last_LTRA<=grace)+
                          as.numeric(last_ICS_SOL<=grace)+
                          as.numeric(last_Theo<=grace)))

### BTS Step Coding 
### STEP 0 
prescribing_asthma<-prescribing_asthma %>%  
  mutate(BTS_Step=ifelse(last_ICS>grace &
                           last_combo>grace,
                         0,NA)) 

### STEP 1 
prescribing_asthma<-prescribing_asthma %>% 
  mutate(BTS_Step=ifelse(last_ICS<=grace & 
                           last_ICS_dose=="low" &
                           last_LABA>grace &
                           last_combo>grace & 
                           number_add_on==0,  
                         1,BTS_Step))


### STEP 2 - low ICS + LABA (inc. as combo)
prescribing_asthma<-prescribing_asthma %>% 
  mutate(BTS_Step=ifelse(((last_ICS<=grace & last_LABA<=grace) | 
                            last_combo<=grace) &
                              last_ICS_dose=="low",
                         2,BTS_Step)) 


### STEP 3 
prescribing_asthma<-prescribing_asthma %>% 
  mutate(BTS_Step=ifelse((last_ICS<=grace & 
                           last_ICS_dose=="low" &
                           last_LABA>grace &
                           last_combo>grace &
                           number_add_on>0) |
                           ((last_combo<=grace | last_ICS<=grace) &
                              last_ICS_dose=="medium" & 
                              number_add_on==0), 3,BTS_Step))
    


### STEP 4
prescribing_asthma<-prescribing_asthma %>% 
  mutate(BTS_Step=ifelse((last_ICS<=grace | last_combo<=grace) &
                           (last_ICS_dose=="high" |
                           (last_ICS_dose=="medium" &
                              number_add_on>0)),
                         4,BTS_Step))

# # check: one BTS step per regimen
# prescribing_asthma %>% ungroup %>%
#   distinct(regimen,BTS_Step) %>%
#   count(regimen) %>% count(n)


##########################################################################
###  Final Cleaning
##########################################################################

prescribing_asthmax <- prescribing_asthma %>%
  distinct(ID, PrescDate, BTS_Step,regimen)  %>%
  arrange(ID, PrescDate, BTS_Step,nchar(regimen),regimen) %>%
  group_by(ID,PrescDate) %>%
  slice(n()) %>%
  ungroup 

prescribing_asthmax <- prescribing_asthmax %>%
  filter(PrescDate>=as.Date("2009-06-01"))
rm(prescribing_asthma,prescribing_asthma_ICS,grace)

save(prescribing_asthmax,file="~/Study_Data/1516-0489/Research/Holly/Data/BTS Step Paper.RData")

# What is the prevalence of each regimen?
length(unique(prescribing_asthmax$regimen))
View(prescribing_asthmax %>% ungroup %>% count(regimen,sort=T) %>% 
       mutate(percent = round(n*100/sum(n),1)) %>% filter(percent>1) %>%
       mutate(other_n = nrow(prescribing_asthmax)-sum(n), other_p = 100-sum(percent)))


#How many regimens at each step?
prescribing_asthmax %>% ungroup %>% distinct(regimen,BTS_Step) %>% count(BTS_Step)
head(prescribing_asthmax %>% ungroup %>% filter(BTS_Step==0) %>% count(regimen, sort=T) %>%
       mutate(perc = round(n*100/sum(n),2)))
table(prescribing_asthmax$BTS_Step)
round(table(prescribing_asthmax$BTS_Step)*100/nrow(prescribing_asthmax),1)



# Time until change of Step
prescribing_asthmax2 <- prescribing_asthmax %>% 
  mutate(drop = ID==lag(ID) & BTS_Step==lag(BTS_Step)) %>%
  filter(drop!=1) %>%
  select(-drop) %>%
  group_by(ID) %>%  
  mutate(time_until_change = as.numeric(lead(PrescDate)-PrescDate))
summary(prescribing_asthmax2$time_until_change)

# Time until change: step up or down 
prescribing_asthmax3<-prescribing_asthmax2 %>%
  mutate(change = ifelse(lead(BTS_Step)>BTS_Step,"Step Up","Step Down"))
tapply(prescribing_asthmax3$time_until_change,prescribing_asthmax3$change,summary)

prescribing_asthmax2 %>% 
  group_by(BTS_Step) %>%
  summarise(missing = sum(is.na(time_until_change)),
            mperc = missing*100/(missing+sum(!is.na(time_until_change))),
            median=median(time_until_change,na.rm = T),
            LQ = quantile(time_until_change,prob=0.25,na.rm = T),
            UQ = quantile(time_until_change,prob=0.75,na.rm = T))


ggplot(prescribing_asthmax3 %>% filter(!is.na(change)),
       aes(x=time_until_change,y=as.factor(BTS_Step), 
           fill=as.factor(BTS_Step))) +
  geom_density_ridges2(show.legend = F) +
  coord_cartesian(xlim=c(0,350)) +
  facet_grid(. ~ change) +
  theme_bw() +
  ylab("BTS Step") + xlab("Days Until Change") +
  theme(text = element_text(size=20))
  


# Time until change of Regimen
prescribing_asthmax4 <- prescribing_asthmax %>% 
  group_by(ID) %>%
  mutate(drop = ID==lag(ID) & regimen==lag(regimen)) %>%
  filter(drop!=1) %>%
  select(-drop) %>%
  group_by(ID) %>%  
  mutate(time_until_change = as.numeric(lead(PrescDate)-PrescDate),
         change = ifelse(lead(BTS_Step)==BTS_Step,
                         "Same Step",
                         ifelse(lead(BTS_Step)>BTS_Step,
                                "Step Up",
                                "Step Down")))  %>%
  filter(!is.na(time_until_change))  
table(prescribing_asthmax4$change)*100/nrow(prescribing_asthmax4)
round(prop.table(table(prescribing_asthmax4$BTS_Step,prescribing_asthmax4$change),1),2)

# Number of Distinct steps per year
prescribing_asthmax %>%
  mutate(year = year(PrescDate)) %>%
  distinct(ID,year,BTS_Step) %>% 
  count(ID,year, name="distinct") %>%
  count(distinct, name ="count") %>%
  mutate(prop = round((count*100/sum(count)),1),
         total = sum(count))

# Number of changes per year
prescribing_asthmax %>% 
  mutate(year = year(PrescDate)) %>%
  group_by(ID,year) %>% 
  mutate(drop = ID==lag(ID) & BTS_Step==lag(BTS_Step)) %>%
  filter(drop==F | is.na(drop)) %>%
  ungroup %>%
  count(ID,year,name="year_changes") %>%
  count(year_changes,name="count") %>%
  mutate(label = ifelse(year_changes<=5,as.character(year_changes),">5")) %>%
  group_by(label) %>%
  mutate(count = sum(count)) %>%
  filter(year_changes<=6) %>%
  ungroup %>% 
  mutate(prop = round((count*100/sum(count)),1)) %>%
  select(-year_changes)


t1<-prescribing_asthmax %>%
  mutate(year = year(PrescDate)) %>%
  distinct(ID,year,BTS_Step) %>% 
  count(ID,year, name="distinct") 
t2<-prescribing_asthmax %>% 
  mutate(year = year(PrescDate)) %>%
  group_by(ID,year) %>% 
  mutate(drop = ID==lag(ID) & BTS_Step==lag(BTS_Step)) %>%
  filter(drop==F | is.na(drop)) %>%
  ungroup %>%
  count(ID,year,name="year_changes") %>%
  mutate(label = ifelse(year_changes<=5,as.character(year_changes-1),"5+"))
table<-left_join(t1,t2)
table(table$distinct,table$label)



# changes
links<-prescribing_asthmax2 %>%
  group_by(ID) %>%
  mutate(end = lead(BTS_Step),
         target = paste0(end,"x")) %>% 
  rename(source = BTS_Step) %>%
  filter(!is.na(end) & source!=0 & end!=0) %>%
  ungroup %>% 
  count(source,target,name="value")

nodes<-data.frame(name=c(as.character(links$source),
                         as.character(links$target)) %>% unique()) %>%
  mutate(label = substr(name,1,1)) %>%
  arrange(label)

links$IDsource<-match(links$source,nodes$name)-1
links$IDtarget<-match(links$target,nodes$name)-1
  
sankeyNetwork(Links=links,
              Nodes = nodes,
              Source = "IDsource",
              Target = "IDtarget",
              Value = "value",
              NodeID = "label",
              iterations = 0,
              fontSize = 40)


# BTS steps over time
df<-prescribing_asthmax %>% 
  mutate(PrescDate = format(PrescDate,"%Y-%m")) %>%
  group_by(ID, PrescDate) %>%
  summarise(BTS_Step = max(BTS_Step)) %>%
  ungroup %>% 
  count(PrescDate,BTS_Step,name="count") %>%
  group_by(PrescDate) %>% mutate(perc = count*100/sum(count))

ggplot(df) +
  geom_line(aes(x=PrescDate,y=perc,group=as.factor(BTS_Step),
                color=as.factor(BTS_Step)),
            size=3) +
  theme_bw() + 
  theme(text = element_text(size=20),
        axis.text.x = element_text(angle = 90)) +
  ylab("Percentage of Individuals") +
  xlab("Month") +
  labs(color = "Maximum BTS \nStep during \nMonth") +
  scale_x_discrete(breaks = c("2009-07","2010-01","2010-07","2011-01","2011-07",
                              "2012-01","2012-07","2013-01","2013-07",
                              "2014-01","2014-07", "2015-01", "2015-07",
                              "2016-01", "2016-07","2017-01")) +
  scale_y_continuous(limits=c(0,35), breaks=seq(0,35,5))


