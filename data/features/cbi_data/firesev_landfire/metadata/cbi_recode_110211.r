
#...........................CBI Recode..........................................

# February 2011 
# Eva Karau

# This program calculates CBI values from FIRESEV burn severity field data.

# 1. Calculates CBI for individual elements in understory and overstory strata.
# 2. Calculates Total CBI (across all sections)
# 3. Calculates GeoCBI using both pre and post-fire FCOV according to De Santis and Chuvieco(2008).
# 4. Creates a .csv (fd.csv) including many of the original field data variables and the calculated CBI variables.
# 5. Creates a .csv (cbi_recode.csv) which is a subset of fd.full and includes only calculated CBI variables. 

# Note: I made a few minor changes in this script from Eva's original
#       1. Took soil & rock cover/color out of A (likely inconsistent field interpretation)
#       2. Added in a calculation of Average FH severity rating

# Before running this code:
# Export the FIRESEV_Burn_Severity table in the FireSevFieldData Access database to a .csv.
# Modify directory path and csv names in the first section of the program

#...............................................................................

#Set directory path and csv names  

#Set the path for the working directory 
Path <- "F:\\firesev\\fieldwork\\data\\recode_110211"
 
#Modify the name of the .csv (created from the Access table) 
InCsv <- "burnsev.csv"

#Pick a name for the full output .csv here:
FullCsv <- "cbi_calc_full.csv"

#Pick a name for the CBI only .csv (subset of FullCsv) 
CbiCsv <- "cbi_calc.csv"

#...............................................................................

#Read data into R from .csv that has been exported from Access

fd <- read.csv(paste(Path,InCsv,sep="\\"), header=TRUE, sep = ",", dec=".")

#...............................................................................

#Remove uneccesary columns from the data frame

 fd$RegID <- NULL
 fd$ProjID <- NULL
 fd$Date <- NULL
 fd$Examiner <- NULL
 fd$Comments <- NULL
 fd$LS_BurnSev <- NULL
 #fd$FH_Overall <- NULL
 #fd$FH_Overstory <- NULL
 #fd$FH_Fuel <- NULL
 #fd$FH_Soil <- NULL
 fd$EVT_pre <- NULL
 fd$EVT_post <- NULL
 fd$SS_pre <- NULL
 fd$SS_post <- NULL
 fd$ESP <- NULL


#------------------------------------------------------------------------------- 

#Calculate CBI for section A.Substrates (divide by 32.67, unless otherwise noted)

 fd$A_Litter_cbi <- fd$A_Litter/32.67
 fd$A_Duff_cbi <- fd$A_Duff/32.67
 fd$A_FineFuel_cbi <- fd$A_FineFuel/32.67
 fd$A_LargeFuel_cbi <- fd$A_LargeFuel/32.67
 #fd$A_SoilRockCovCol_cbi <- fd$A_SoilRockCovCol/32.67
#For soil color/cover fraction, disregard %green/brown and add %gray/black and %red to calculate CBI as shown below
 fd$A_SoilColFracCov_cbi <- (fd$A_GrayBlack + fd$A_Red)/32.67
#Calculate total CBI for section A
 cbi_A_list <- cbind(fd$A_Litter_cbi, fd$A_Duff_cbi, fd$A_FineFuel_cbi, fd$A_LargeFuel_cbi, fd$A_GreenBrown_cbi, fd$A_SoilColFracCov_cbi)
 fd$cbi_A <- apply(cbi_A_list, 1, mean, na.rm = TRUE)
 rm("cbi_A_list")
 
 #------------------------------------------------------------------------------- 

#Calculate CBI for section B.Herbs, Low Shrubs and Trees (divide by 32.67, unless otherwise noted)

 fd$B_Altered_cbi <- fd$B_Altered/32.67
#For Frequency/Percent Living high percentage values indicate lower severity, so need to subtract the %value from 100.  
 fd$B_PctLive_cbi <- (100-fd$B_PctLive)/32.67 
#For Spp Comp-Rel. Abundance - recode U,L,M,H to CBI values of 0,1,2,3
 fd$B_Comp_cbi <- ifelse(fd$B_Comp =="U",0,ifelse(fd$B_Comp=="L",1,ifelse(fd$B_Comp=="M",2,ifelse(fd$B_Comp=="H",3,(ifelse(fd$B_Comp=="","NA",fd$B_Comp)))))) 
#Calculate total CBI for section B including diameter of smallest branch
 cbi_B_t_list <- cbind(fd$B_Altered_cbi, fd$B_PctLive_cbi, as.numeric(fd$B_Comp_cbi), fd$B_Twig)
 fd$cbi_B_t <- apply(cbi_B_t_list, 1, mean, na.rm = TRUE) 
 rm("cbi_B_t_list")
 
#Calculate total CBI for section B NOT including diameter of smallest branch
 cbi_B_list <- cbind(fd$B_Altered_cbi, fd$B_PctLive_cbi, as.numeric(fd$B_Comp_cbi))
 fd$cbi_B <- apply(cbi_B_list, 1, mean, na.rm = TRUE) 
 rm("cbi_B_list")
  
#-------------------------------------------------------------------------------

#Calculate CBI for section B1.Herbs(divide by 32.67, unless otherwise noted)

 fd$B1_Altered_cbi <- fd$B1_Altered/32.67
#For Frequency/Percent Living high percentage values indicate lower severity, so need to subtract the %value from 100.  
 fd$B1_PctLive_cbi <- (100-fd$B1_PctLive)/32.67 
#For Spp Comp-Rel. Abundance - recode U,L,M,H to CBI values of 0,1,2,3
 fd$B1_Comp_cbi <- ifelse(fd$B1_Comp =="U",0,ifelse(fd$B1_Comp=="L",1,ifelse(fd$B1_Comp=="M",2,ifelse(fd$B1_Comp=="H",3,(ifelse(fd$B1_Comp=="","NA",fd$B1_Comp)))))) 
#Calculate total CBI for section B1 
 cbi_B1_list <- cbind(fd$B1_Altered_cbi, fd$B1_PctLive_cbi, as.numeric(fd$B1_Comp_cbi))
 fd$cbi_B1 <- apply(cbi_B1_list, 1, mean, na.rm = TRUE) 
 rm("cbi_B1_list")
 
#-------------------------------------------------------------------------------

#Calculate CBI for section B2.Low Shrubs(divide by 32.67, unless otherwise noted)

 fd$B2_Altered_cbi <- fd$B2_Altered/32.67
#For Frequency/Percent Living high percentage values indicate lower severity, so need to subtract the %value from 100.  
 fd$B2_PctLive_cbi <- (100-fd$B2_PctLive)/32.67 
#For Spp Comp-Rel. Abundance - recode U,L,M,H to CBI values of 0,1,2,3
 fd$B2_Comp_cbi <- ifelse(fd$B2_Comp =="U",0,ifelse(fd$B2_Comp=="L",1,ifelse(fd$B2_Comp=="M",2,ifelse(fd$B2_Comp=="H",3,(ifelse(fd$B2_Comp=="","NA",fd$B2_Comp)))))) 
#Calculate total CBI for section B2 including diameter of smallest branch
 cbi_B2_t_list <- cbind(fd$B2_Altered_cbi, fd$B2_PctLive_cbi, as.numeric(fd$B2_Comp_cbi), fd$B2_Twig)
 fd$cbi_B2_t <- apply(cbi_B2_t_list, 1, mean, na.rm = TRUE) 
 rm("cbi_B2_t_list")

#Calculate total CBI for section B2 NOT including diameter of smallest branch
 cbi_B2_list <- cbind(fd$B2_Altered_cbi, fd$B2_PctLive_cbi, as.numeric(fd$B2_Comp_cbi))
 fd$cbi_B2 <- apply(cbi_B2_list, 1, mean, na.rm = TRUE) 
 rm("cbi_B2_list")

#-------------------------------------------------------------------------------

#Calculate CBI for section C.Tall Shrubs and Trees(divide by 32.67, unless otherwise noted)

 fd$C_Altered_cbi <- fd$C_Altered/32.67
#For Frequency/Percent Living high percentage values indicate lower severity, so need to subtract the %value from 100.  
 fd$C_PctLive_cbi <- (100-fd$C_PctLive)/32.67 
#For Spp Comp-Rel. Abundance - recode U,L,M,H to CBI values of 0,1,2,3
 fd$C_Comp_cbi <- ifelse(fd$C_Comp =="U",0,ifelse(fd$C_Comp=="L",1,ifelse(fd$C_Comp=="M",2,ifelse(fd$C_Comp=="H",3,(ifelse(fd$C_Comp=="","NA",fd$C_Comp)))))) 
#Calculate total CBI for section C including diameter of smallest branch
 cbi_C_t_list <- cbind(fd$C_Altered_cbi, fd$C_PctLive_cbi, as.numeric(fd$C_Comp_cbi), fd$C_Twig)
 fd$cbi_C_t <- apply(cbi_C_t_list, 1, mean, na.rm = TRUE) 
 rm("cbi_C_t_list")

#Calculate total CBI for section C NOT including diameter of smallest branch
 cbi_C_list <- cbind(fd$C_Altered_cbi, fd$C_PctLive_cbi, as.numeric(fd$C_Comp_cbi))
 fd$cbi_C <- apply(cbi_C_list, 1, mean, na.rm = TRUE) 
 rm("cbi_C_list") 
 
#-------------------------------------------------------------------------------
 
#Calculate CBI for section D.Small Trees(divide by 32.67, unless otherwise noted)

 fd$D_Green_cbi <- (100 - fd$D_Green)/32.67 # Subtract %Green from 100 to achieve accurate CBI values
 fd$D_BlackBrown_cbi <- fd$D_BlackBrown/32.67
#For Frequency/Percent Living high percentage values indicate lower severity, so need to subtract the %value from 100.  
 fd$D_PctLive_cbi <- (100-fd$D_PctLive)/32.67 
#Calculate total CBI for section D including diameter of smallest branch
 cbi_D_t_list <- cbind(fd$D_Green_cbi, fd$D_BlackBrown_cbi, fd$D_PctLive_cbi, fd$D_Twig)
 fd$cbi_D_t <- apply(cbi_D_t_list, 1, mean, na.rm = TRUE) 
 rm("cbi_D_t_list")
  
#Calculate total CBI for section D NOT including diameter of smallest branch
 cbi_D_list <- cbind(fd$D_Green_cbi, fd$D_BlackBrown_cbi, fd$D_PctLive_cbi)
 fd$cbi_D <- apply(cbi_D_list, 1, mean, na.rm = TRUE) 
 rm("cbi_D_list")  
 
#-------------------------------------------------------------------------------
 
#Calculate CBI for section E.Intermediate Trees(divide by 32.67, unless otherwise noted)

 fd$E_Green_cbi <- (100 - fd$E_Green)/32.67 # Subtract %Green from 100 to achieve accurate CBI values
 fd$E_BlackBrown_cbi <- fd$E_BlackBrown/32.67
#For Frequency/Percent Living high percentage values indicate lower severity, so need to subtract the %value from 100.  
 fd$E_PctLive_cbi <- (100-fd$E_PctLive)/32.67 
#Calculate total CBI for section E 
 cbi_E_list <- cbind(fd$E_Green_cbi, fd$E_BlackBrown_cbi, fd$E_PctLive_cbi)
 fd$cbi_E <- apply(cbi_E_list, 1, mean, na.rm = TRUE) 
 rm("cbi_E_list")  
#-------------------------------------------------------------------------------
 
#Calculate CBI for section F.Tall Trees(divide by 32.67, unless otherwise noted)

 fd$F_Green_cbi <- (100 - fd$F_Green)/32.67 # Subtract %Green from 100 to achieve accurate CBI values
 fd$F_BlackBrown_cbi <- fd$F_BlackBrown/32.67
#For Frequency/Percent Living high percentage values indicate lower severity, so need to subtract the %value from 100.  
 fd$F_PctLive_cbi <- (100-fd$F_PctLive)/32.67 
#Calculate total CBI for section F 
 cbi_F_list <- cbind(fd$F_Green_cbi, fd$F_BlackBrown_cbi, fd$F_PctLive_cbi)
 fd$cbi_F <- apply(cbi_F_list, 1, mean, na.rm = TRUE) 
 rm("cbi_F_list")  

#-------------------------------------------------------------------------------

#Calculate Total CBI (across all sections) INCLUDING diameter of smallest branch
#Take into account the conditional inclusion of the B, B1 and B2 categories: 
#If there are non-NA values in the Tree strata (C, D, E, or F), then use B; if tree strata are all NA, use either B1, B2, or both B1 and B2.  

 fd$cbi_tot_t <- ifelse(is.na(fd$cbi_C_t) & is.na(fd$cbi_D_t) & is.na(fd$cbi_E) & is.na(fd$cbi_F) & is.na(fd$cbi_B1) & !is.na(fd$cbi_B2_t)
 ,apply(cbind(fd$cbi_A, fd$cbi_B2_t), 1, mean, na.rm = TRUE) 
 ,ifelse(is.na(fd$cbi_C_t) & is.na(fd$cbi_D_t) & is.na(fd$cbi_E) & is.na(fd$cbi_F) & is.na(fd$cbi_B2_t) & !is.na(fd$cbi_B1)
 ,apply(cbind(fd$cbi_A, fd$cbi_B1), 1, mean, na.rm = TRUE) 
 ,ifelse(is.na(fd$cbi_C_t) & is.na(fd$cbi_D_t) & is.na(fd$cbi_E) & is.na(fd$cbi_F) & !is.na(fd$cbi_B1) & !is.na(fd$cbi_B2_t)
 ,apply(cbind(fd$cbi_A, fd$cbi_B1, fd$cbi_B2_t), 1, mean, na.rm = TRUE)
 ,apply(cbind(fd$cbi_A, fd$cbi_B_t, fd$cbi_C_t, fd$cbi_D_t, fd$cbi_E, fd$cbi_F), 1, mean, na.rm = TRUE)
 )
 )
 ) 

#Calculate Total CBI (across all sections) NOT including diameter of smallest branch

 fd$cbi_tot <- ifelse(is.na(fd$cbi_C) & is.na(fd$cbi_D) & is.na(fd$cbi_E) & is.na(fd$cbi_F) & is.na(fd$cbi_B1) & !is.na(fd$cbi_B2)
 ,apply(cbind(fd$cbi_A, fd$cbi_B2), 1, mean, na.rm = TRUE) 
 ,ifelse(is.na(fd$cbi_C) & is.na(fd$cbi_D) & is.na(fd$cbi_E) & is.na(fd$cbi_F) & is.na(fd$cbi_B2) & !is.na(fd$cbi_B1)
 ,apply(cbind(fd$cbi_A, fd$cbi_B1), 1, mean, na.rm = TRUE) 
 ,ifelse(is.na(fd$cbi_C) & is.na(fd$cbi_D) & is.na(fd$cbi_E) & is.na(fd$cbi_F) & !is.na(fd$cbi_B1) & !is.na(fd$cbi_B2)
 ,apply(cbind(fd$cbi_A, fd$cbi_B1, fd$cbi_B2), 1, mean, na.rm = TRUE)
 ,apply(cbind(fd$cbi_A, fd$cbi_B, fd$cbi_C, fd$cbi_D, fd$cbi_E, fd$cbi_F), 1, mean, na.rm = TRUE)
 )
 )
 )

#-------------------------------------------------------------------------------

#Calculate GeoCBI using pre-fire FCOV values
#
#rename A_FCov_Post to be consistent with other FCOV columns
names(fd)[names(fd)=="A_FCovPost"] <- "A_FCov_Post"

#Change FCOV_Post weights to decimal and multiply by CBI values

fd$FC_wgt_A_post <- (fd$A_FCov_Post/100)*fd$cbi_A
fd$FC_wgt_B_post <- (fd$B_FCov_Post/100)*fd$cbi_B
fd$FC_wgt_B1_post <- (fd$B1_FCov_Post/100)*fd$cbi_B1
fd$FC_wgt_B2_post <- (fd$B2_FCov_Post/100)*fd$cbi_B2
fd$FC_wgt_C_post <- (fd$C_FCov_Post/100) *fd$cbi_C
fd$FC_wgt_D_post <- (fd$D_FCov_Post/100)*fd$cbi_D
fd$FC_wgt_E_post <- (fd$E_FCov_Post/100)*fd$cbi_E
fd$FC_wgt_F_post <- (fd$F_FCov_Post/100)*fd$cbi_F

# Calculate weighted CBI with FCOV_Post using apply to form the numerator of the GecCBI calculation

FcovWgtListPost = cbind(fd$FC_wgt_A_post, fd$FC_wgt_B_post, fd$FC_wgt_B1_post, fd$FC_wgt_B2_post, fd$FC_wgt_C_post, fd$FC_wgt_D_post, fd$FC_wgt_E_post, fd$FC_wgt_F_post)

fd$FCovWgtCBI_Post <- apply(FcovWgtListPost, 1, sum, na.rm = TRUE)

# Calculate the sum of all FCOV_Post values to form the denominator of the GecCBI calculation

FcovSumListPost <- cbind(fd$A_FCov_Post/100, fd$B_FCov_Post/100,fd$B1_FCov_Post/100, fd$B2_FCov_Post/100, fd$C_FCov_Post/100, fd$D_FCov_Post/100, fd$E_FCov_Post/100, fd$F_FCov_Post/100)  

fd$FCovSumCBI_Post <- apply(FcovSumListPost, 1, sum, na.rm = TRUE)

# Calculate GeoCBI using post-fire FCOV values
 
fd$GeoCBI_post <- fd$FCovWgtCBI_Post/fd$FCovSumCBI_Post

# Delete unecessary columns

fd$FC_wgt_A_post <- NULL 
fd$FC_wgt_B_post <- NULL
fd$FC_wgt_B1_post <- NULL
fd$FC_wgt_B2_post <- NULL
fd$FC_wgt_C_post <- NULL
fd$FC_wgt_D_post <- NULL
fd$FC_wgt_E_post <- NULL
fd$FC_wgt_F_post <- NULL

#...............................................................................

#Calculate GeoCBI using pre-fire FCOV values

#rename A_FCov_Pre to be consistent with other FCOV columns
names(fd)[names(fd)=="A_FCovPre"] <- "A_FCov_Pre"

#Change FCOV_Pre weights to decimal and multiply by CBI values

fd$FC_wgt_A_pre <- (fd$A_FCov_Pre/100)*fd$cbi_A
fd$FC_wgt_B_pre <- (fd$B_FCov_Pre/100)*fd$cbi_B
fd$FC_wgt_B1_pre <- (fd$B1_FCov_Pre/100)*fd$cbi_B1
fd$FC_wgt_B2_pre <- (fd$B2_FCov_Pre/100)*fd$cbi_B2
fd$FC_wgt_C_pre <- (fd$C_FCov_Pre/100)*fd$cbi_C
fd$FC_wgt_D_pre <- (fd$D_FCov_Pre/100)*fd$cbi_D
fd$FC_wgt_E_pre <- (fd$E_FCov_Pre/100)*fd$cbi_E
fd$FC_wgt_F_pre <- (fd$F_FCov_Pre/100)*fd$cbi_F

# Calculate weighted CBI with FCOV_Pre using apply to form the numerator of the GecCBI calculation

FcovWgtListPre = cbind(fd$FC_wgt_A_pre, fd$FC_wgt_B_pre, fd$FC_wgt_B1_pre, fd$FC_wgt_B2_pre, fd$FC_wgt_C_pre, fd$FC_wgt_D_pre, fd$FC_wgt_E_pre, fd$FC_wgt_F_pre)

fd$FCovWgtCBI_Pre <- apply(FcovWgtListPre, 1, sum, na.rm = TRUE)

# Calculate the sum of all FCOV_Pre values to form the denominator of the GecCBI calculation

FcovSumListPre <- cbind(fd$A_FCov_Pre/100, fd$B_FCov_Pre/100, fd$B1_FCov_Pre/100, fd$B2_FCov_Pre/100, fd$C_FCov_Pre/100, fd$D_FCov_Pre/100, fd$E_FCov_Pre/100, fd$F_FCov_Pre/100)  

fd$FCovSumCBI_Pre <- apply(FcovSumListPre, 1, sum, na.rm = TRUE)

# Calculate GeoCBI using pre-fire FCOV values
 
fd$GeoCBI_pre <- fd$FCovWgtCBI_Pre/fd$FCovSumCBI_Pre

# Delete unecessary columns

fd$FC_wgt_A_pre <- NULL 
fd$FC_wgt_B_pre <- NULL
fd$FC_wgt_B1_pre <- NULL
fd$FC_wgt_B2_pre <- NULL
fd$FC_wgt_C_pre <- NULL
fd$FC_wgt_D_pre <- NULL
fd$FC_wgt_E_pre <- NULL
fd$FC_wgt_F_pre <- NULL

#...............................................................................

#Do a calculation on the FH burn severity fields
fd$FH_Average <- apply(cbind(fd$FH_Overstory,fd$FH_Fuel,fd$FH_Soil),1,mean,na.rm=T)
fd$FH_WAvg  <- apply(cbind(fd$FH_Overstory,fd$FH_Overstory,fd$FH_Fuel,fd$FH_Fuel,fd$FH_Soil),1,mean,na.rm=T)

#-------------------------------------------------------------------------------

#Export data frames to .csv

#Export fd to .csv 
                 
write.csv(fd, file = paste(Path, FullCsv, sep="\\"))

#Export cbi_recode to .csv (this is a subset of the fd data frame, including only the calculated CBI fields)

cbi_recode <- data.frame(PlotID=fd$PlotID, A_Litter_cbi=fd$A_Litter_cbi, A_Duff_cbi=fd$A_Duff_cbi, A_FineFuel_cbi=fd$A_FineFuel_cbi,
A_LargeFuel_cbi=fd$A_LargeFuel_cbi, A_SoilColFracCov_cbi=fd$A_SoilColFracCov_cbi, 
cbi_A=fd$cbi_A, B_Altered_cbi=fd$B_Altered_cbi, B_PctLive_cbi=fd$B_PctLive_cbi, B_Comp_cbi=fd$B_Comp_cbi, cbi_B_t=fd$cbi_B_t, 
cbi_B=fd$cbi_B, B1_Altered_cbi=fd$B1_Altered_cbi, B1_PctLive_cbi=fd$B1_PctLive_cbi, B1_Comp_cbi=fd$B1_Comp_cbi, cbi_B1=fd$cbi_B1,
B2_Altered_cbi=fd$B2_Altered_cbi, B2_PctLive_cbi=fd$B2_PctLive_cbi, B2_Comp_cbi=fd$B2_Comp_cbi, cbi_B2_t=fd$cbi_B2_t, cbi_B2=fd$cbi_B2,
C_Altered_cbi=fd$C_Altered_cbi, C_PctLive_cbi=fd$C_PctLive_cbi, C_Comp_cbi=fd$C_Comp_cbi, cbi_C_t=fd$cbi_C_t,cbi_C=fd$cbi_C,
D_Green_cbi=fd$D_Green_cbi, D_BlackBrown_cbi=fd$D_BlackBrown_cbi, D_PctLive_cbi=fd$D_PctLive_cbi, cbi_D_t=fd$cbi_D_t, cbi_D=fd$cbi_D,
E_Green_cbi=fd$E_Green_cbi, E_BlackBrown_cbi=fd$E_BlackBrown_cbi, E_PctLive_cbi=fd$E_PctLive_cbi, cbi_E=fd$cbi_E, F_Green_cbi=fd$F_Green_cbi,
F_BlackBrown_cbi=fd$F_BlackBrown_cbi, F_PctLive_cbi=fd$F_PctLive_cbi, cbi_F=fd$cbi_F, cbi_tot_t=fd$cbi_tot_t, cbi_tot=fd$cbi_tot,
GeoCBI_post=fd$GeoCBI_post, GeoCBI_pre=fd$GeoCBI_pre, FH_Overall=fd$FH_Overall, FH_Average=fd$FH_Average, FH_WAvg=fd$FH_WAvg)

write.csv(cbi_recode, file = paste(Path, CbiCsv, sep="\\"))

 #-----------------------------END----------------------------------------------
