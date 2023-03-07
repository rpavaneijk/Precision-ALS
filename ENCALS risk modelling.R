## Parms:
# date = date at screening
# birth = date of birth
# onset = date of symptom onset
# dx = date of diagnosis
# total = ALSFRS-R total score at screening
# vc = %predicted slow/forced vital capacity at screening (GLI-2021 reference)
# onset = bulbar site of symptom onset (T/F)
# ftd = presence of ALS-FTD at screening (T/F)
# ee = definite el escorial at screening (T/F)

## Create data from input parameters:
D <- data.frame (AGE_ONSET = as.numeric (as.Date (onset) - as.Date (birth))/365.25,
                 DISDUR = (as.numeric (as.Date (date) - as.Date (onset))/365.25)*12,
                 DXDELAY = (as.numeric (as.Date (dx) - as.Date (onset))/365.25)*12,
                 TOTAL = as.numeric (total),
                 FVC = as.numeric (vc), 
                 ONSET = as.numeric (onset),
                 FTD = as.numeric (ftd),
                 EE = as.numeric (ee))

## Estimate the ALSFRS-R progression rate at screening
D$SLOPE <- (D$TOTAL - 48) / D$DISDUR

## Estimate the non-linear transformations
D$tAGE <- (D$AGE_ONSET/100)^-2
D$tDXDELAY <- ((D$DXDELAY/10)^-.5) + log (D$DXDELAY/10)
D$tSLOPE <- ((-D$SLOPE+0.1)^-.5)
D$tFVC <- ((D$FVC/100)^-1) + ((D$FVC/100)^-.5)

## Calculate the TRICALS Risk Profile
D$LP <- (-1.839*D$tSLOPE) + (-2.376*D$tDXDELAY) + (-0.264*D$tAGE) + (0.474*D$tFVC) +
  (0.271*D$ONSET) + (0.238*D$EE) + (0.415*D$FTD) 

## Eligibility (e.g. range -6.00 [lb] to -2.00[ub]):
if (D$LP >= lb & D$LP <= ub){"ELIGIBLE"}else{"NOT ELIGIBLE"}
