## from https://rpubs.com/chrisbrunsdon/14998

CreateVariables <- function(CensusData,na.rm=TRUE) {
  attach(CensusData)
  Age0_4    <- 100 * ( T1_1AGE0T + T1_1AGE1T + T1_1AGE2T + T1_1AGE3T + T1_1AGE4T ) /
    T1_1AGETT
  Age5_14   <- 100 * ( T1_1AGE5T + T1_1AGE6T + T1_1AGE7T + T1_1AGE8T + T1_1AGE9T +
                         T1_1AGE10T + T1_1AGE11T + T1_1AGE12T + T1_1AGE13T + T1_1AGE14T) / 
    T1_1AGETT
  Age25_44  <- 100 * ( T1_1AGE25_29T + T1_1AGE30_34T + T1_1AGE35_39T + T1_1AGE40_44T ) /
    T1_1AGETT
  Age45_64  <- 100 *( T1_1AGE45_49T + T1_1AGE50_54T + T1_1AGE55_59T + T1_1AGE60_64T ) / T1_1AGETT
  Age65over <- 100 *( T1_1AGE65_69T + T1_1AGE70_74T + T1_1AGE75_79T + T1_1AGE80_84T + T1_1AGEGE_85T ) / T1_1AGETT
  
  EU_National           <- 100 * (T2_1UKN + T2_1PLN + T2_1LTN + T2_1EUN) / T2_1TN
  ROW_National          <- 100 * (T2_1RWN) / T2_1TN
  Born_outside_Ireland  <- 100 * (T2_1TBP - T2_1IEBP) / T2_1TBP
  
  Separated            <- 100 * (T1_2SEPT + T1_2DIVT) / T1_2T
  SinglePerson         <- 100 * (T5_2_1PP - T4_5RP  ) / T5_2_TP
  Pensioner            <- 100 * T4_5RP / T4_5TP
  LoneParent           <- 100 * (T4_3FTLF + T4_3FTLM) / T4_5TF
  DINK                 <- 100 * T4_5PFF / T4_5TF
  NonDependentKids     <- 100 * T4_4AGE_GE20F / T4_4TF
  
  RentPublic         <- 100 * T6_3_RLAH / T6_3_TH
  RentPrivate        <- 100 * T6_3_RPLH / T6_3_TH
  #Terraced           <-
  #Detached           <-
  Flats              <- 100 * T6_1_FA_H / T6_1_TH
  NoCenHeat          <- 100 * T6_5_NCH / T6_5_T
  RoomsHH            <- (T6_4_1RH + 2*T6_4_2RH + 3*T6_4_3RH + 4*T6_4_4RH + 5*T6_4_5RH + 6*T6_4_6RH + 7*T6_4_7RH + 8*T6_4_GE8RH) / T6_4_TH
  PeopleRoom         <- T1_1AGETT / (T6_4_1RH + 2*T6_4_2RH + 3*T6_4_3RH + 4*T6_4_4RH + 5*T6_4_5RH + 6*T6_4_6RH + 7*T6_4_7RH + 8*T6_4_GE8RH)
  SepticTank         <- 100 * T6_7_IST / T6_7_T
  
  HEQual              <-  100 * ((T10_4_ODNDT + T10_4_HDPQT + T10_4_PDT + T10_4_DT) / T10_4_TT) # educ to degree or higher
  Employed            <-  100 * T8_1_WT / T8_1_TT
  TwoCars             <-  100 * (T15_1_2C + T15_1_3C + T15_1_GE4C) / (T15_1_NC + T15_1_1C + T15_1_2C + T15_1_3C + T15_1_GE4C)
  JTWPublic           <-  100 * (T11_1_BU + T11_1_TDL) / T11_1_T
  HomeWork            <-  100 * T9_2_PH / T9_2_PT
  LLTI                <-  100 * (T12_3BT + T12_3VBT) / T12_3TT
  UnpaidCare          <-  100 * (T12_2TM + T12_2TF )/ (T1_1AGETT)
  
  Students           <- 100 * T8_1_ST / T8_1_TT
  Unemployed         <- 100 * T8_1_ULGUPJT / T8_1_TT
  # PartTime           <- 100 *
  EconInactFam       <- 100 * T8_1_LAHFT / T8_1_TT
  Agric              <- 100 * (T14_1_AFFM + T14_1_AFFF) / (T14_1_TM + T14_1_TF)
  Construction       <- 100 * (T14_1_BCM  + T14_1_BCF ) / (T14_1_TM + T14_1_TF)
  Manufacturing      <- 100 * (T14_1_MIM  + T14_1_MIF ) / (T14_1_TM + T14_1_TF)
  Commerce           <- 100 * (T14_1_CTM  + T14_1_CTF ) / (T14_1_TM + T14_1_TF)
  Transport          <- 100 * (T14_1_TCM  + T14_1_TCF ) / (T14_1_TM + T14_1_TF)
  Public             <- 100 * (T14_1_PAM  + T14_1_PAF ) / (T14_1_TM + T14_1_TF)
  Professional       <- 100 * (T14_1_PSM  + T14_1_PSF ) / (T14_1_TM + T14_1_TF)
  
  ### MISC
  Broadband          <- 100 * T15_3_B / (T15_3_B + T15_3_OTH)            # Internet connected HH with Broadband
  Internet           <- 100 * (T15_3_B + T15_3_OTH) / T15_3_T            # Households with Internet
  
  detach(CensusData)
  ### Bringing it all together
  
  Place <- data.frame(CensusData[,1],stringsAsFactors=FALSE)
  colnames(Place)[1] <- 'GEOGID'
  Demographic <- data.frame(Age0_4, Age5_14, Age25_44, Age45_64, Age65over, EU_National, ROW_National, Born_outside_Ireland)
  HouseholdComposition <- data.frame(Separated, SinglePerson, Pensioner, LoneParent, DINK, NonDependentKids)
  Housing <- data.frame(RentPublic, RentPrivate, Flats, NoCenHeat, RoomsHH, PeopleRoom, SepticTank)
  SocioEconomic <- data.frame(HEQual, Employed, TwoCars, JTWPublic, HomeWork, LLTI, UnpaidCare)
  Employment <- data.frame(Students, Unemployed, EconInactFam,Agric,Construction,Manufacturing,Commerce,Transport,Public,Professional)
  Misc  <- data.frame(Internet, Broadband)
  
  DerivedData <- data.frame(Place,Demographic,HouseholdComposition,Housing,SocioEconomic,Employment,Misc)
  if (na.rm) DerivedData[which(is.na(DerivedData),arr.ind=T)] <- 0                            # there are a few NAs - not when I do it? - CB
  
  DerivedData
}