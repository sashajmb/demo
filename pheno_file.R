####### Phenotypic file 
# Use age at recruitment and reported sex
# Focus on CAU only
# Look at all chronic pain, exclude general pain
# Control: no pain at all ("none of the above" from 6159)

# Load bd dataframe
load("../../data/raw/bd671644_orig_sasha.Robj", verbose = TRUE)

### Phenotypes - codes and columns
# Age at recruitment, 21022
# Sex, 31
# Genetic ethnic grouping, 22006
# Back pain for 3+ months, 3571
# Neck/shoulder pain for 3+ months, 3404
# Hip pain for 3+ months, 3414
# Knee pain for 3+ months, 3773
# General pain for 3+ months, 2956 -> should be considered NA
# Headache for 3+ months, 3799
# Facial pains for 3+ months, 4067
# Stomach/abdominal pain for 3+ months, 3741
# Pain type(s) experienced in the last month, 6159 -> only use "none of the above", rest is NA

# OPTION 1: MANUALLY
colnames(bd) <- gsub("f.", "", colnames(bd), fixed = T)
colnames(bd) <- gsub("31", "sex", colnames(bd), fixed = T)
colnames(bd) <- gsub("21022", "age", colnames(bd), fixed = T)
colnames(bd) <- gsub("22006", "ethnicity", colnames(bd), fixed = T)
colnames(bd) <- gsub("3571", "c_back", colnames(bd), fixed = T)
colnames(bd) <- gsub("3404", "c_neck", colnames(bd), fixed = T)
colnames(bd) <- gsub("3414", "c_hip", colnames(bd), fixed = T)
colnames(bd) <- gsub("3773", "c_knee", colnames(bd), fixed = T)
colnames(bd) <- gsub("2956", "c_general", colnames(bd), fixed = T)
colnames(bd) <- gsub("3799", "c_headache", colnames(bd), fixed = T)
colnames(bd) <- gsub("4067", "c_facial", colnames(bd), fixed = T)
colnames(bd) <- gsub("3741", "c_abdominal", colnames(bd), fixed = T)
colnames(bd) <- gsub("6159", "pain", colnames(bd), fixed = T)
colnames(bd) <- gsub(".0.", ".instance1.", colnames(bd), fixed = T)
colnames(bd) <- gsub(".1.", ".instance2.", colnames(bd), fixed = T)
colnames(bd) <- gsub(".2.", ".instance3.", colnames(bd), fixed = T)
colnames(bd) <- gsub(".3.", ".instance4.", colnames(bd), fixed = T)
#change array to question?
colnames(bd) <- gsub(".0", ".array1", colnames(bd), fixed = T)
colnames(bd) <- gsub(".1", ".array2", colnames(bd), fixed = T)
colnames(bd) <- gsub(".2", ".array3", colnames(bd), fixed = T)
colnames(bd) <- gsub(".3", ".array4", colnames(bd), fixed = T)
colnames(bd) <- gsub(".4", ".array5", colnames(bd), fixed = T)
colnames(bd) <- gsub(".5", ".array6", colnames(bd), fixed = T)
colnames(bd) <- gsub(".6", ".array7", colnames(bd), fixed = T)

# OPTION 2: LOOPING - find a way to include the column name change in the loop!
better_colnames <- c(
  "f." = "",
  "31" = "sex",
  "21022" = "age",
  "22006" = "ethnicity",
  "3571" = "c_back",
  "3404" = "c_neck",
  "3414" = "c_hip",
  "3773" = "c_knee",
  "2956" = "c_general",
  "3799" = "c_headache",
  "4067" = "c_facial",
  "3741" = "c_abdominal",
  "6159" = "pain"
)
for (replacement in names(better_colnames)) {
  bd <- setNames(bd, gsub(replacement, better_colnames[replacement], names(bd), fixed = TRUE))
}
chronic_columns <- c("c_back", "c_neck", "c_hip", "c_knee", "c_general", "c_facial", "c_headache", "c_abdominal", "sex", "age", "ethnicity")
for (col in chronic_columns) {
  for (i in 0:3) {
    col_to_replace <- paste0(col, ".", i, ".0")
    new_col_name <- paste0(col, ".instance", i + 1, "")
    names(bd) <- gsub(col_to_replace, new_col_name, names(bd), fixed = TRUE)
  }
}
pain_columns <- ("pain")
for (col in pain_columns) {
  for (i,j in [0:3],[0:6]) {
    
    
# OPTION 3: CREATING A DF?
codes_id <- data.frame(age = 21022,
                       sex = 31)

#Do non-chronic pain types need to be reported as 0 or NA?
#Do chronic pains require the corresponding pain in the last month to be reported as well (as per OG script)? 
#Doing this here

### ID
fid <- bd[, "eid"] # added spaces arrouund arrows + changed to lowercase
# should i add drop = FALSE to ensure that the result is a dataframe?or is it fine as a vecotr
iid <- bd[, "eid"]

### Sex
sex <- bd[, "sex.instance1"] # changed name to lowercase
#w<-which(is.na(Sex) & !is.na(bd.rec[,5])) #not sure what this is for
#Sex[w]<-bd.rec[w,5]

### Age
age <- bd[, "age.instance1"]

### Genetic ethnicity
cau <- bd[, "ethnicity.instance1"]

#### PAIN

###Control
# make vectors for non chronic pain types- field 6159
# new version, include all non-chronic pain types (to be excluded from final phenotype file df) so that they can be used
# to define the chronic pain types (as the corresponding non-chronic pain must have been reported in the last month 
# if they have chronic pain) + need all pain types that include "pain all over" or "general chronic pain" to be made NA)
# (in other words, whoever reported pain allover the body for 3+ months or in 6159 must be NA for any other phenotype), 
# and need pain types to differentiate "None of the above" (nopain) used as control (where only "none of the above" was 
# reported in all instances) VS inconsistent data when pain+nopain was reported (to be made NA).

# replacing pain<-bd[,35:62] #6159 with
pain <- bd [, grep("pain", names(bd))]
# or pain -< bd[, grep("^pain", names(bd))]
# had this written before: pain<-sapply(pain,as.character)

pain[pain == -7] <- -9 # MF: to avoid clash with knee (7)

getpain <- function(trait) {
  res <- vector()
  for(i in 1:ncol(pain)) {                  #loop to iterate over each column of the pain data frame
    w <- grep(trait, pain[, i])                 #search for the presence of the trait in the ith column of the pain data frame and stores it in w variable
    if(length(w) != 0) res <- append(res, w)}    #checks if w contains any elements (so if trait was found in ith column). If w is not empty, the indices stored in w are appended to the res vector
  return(unique(res))}                      #returns the unique elements in the res vector (=indices of columns where the trait was found)

back <- getpain(4); tmp <- rep(0, nrow(bd)); tmp[back] <- 1; back <- tmp
neck <- getpain(3); tmp <- rep(0, nrow(bd)); tmp[neck] <- 1; neck <- tmp    
hip <- getpain(6); tmp <- rep(0, nrow(bd)); tmp[hip] <- 1; hip <- tmp
knee <- getpain(7); tmp <- rep(0, nrow(bd)); tmp[knee] <- 1; knee <- tmp
abdo <- getpain(5); tmp <- rep(0, nrow(bd)); tmp[abdo] <- 1; abdo <- tmp
headache <- getpain(1); tmp <- rep(0, nrow(bd)); tmp[headache] <- 1; headache <- tmp
facial <- getpain(2); tmp <- rep(0, nrow(bd)); tmp[facial] <- 1; face <- tmp # changed from face to facial
allover <- getpain(8); tmp <- rep(0, nrow(bd)); tmp[allover] <- 1; allover <- tmp
nopain <- getpain(-9); tmp <- rep(0, nrow(bd)); tmp[nopain] <- 1; nopain <- tmp  # MF: not required as it is done below
pain[pain == -3] <- NA #prefer not to say

## MF: this is much quicker
NAs <- which(apply(is.na(pain), 1, all))

# pain type + pain all over = NA
# MF: added NAs as we need to exclude them here, too
back[c(which(allover == 1), NAs)] <- NA
neck[c(which(allover == 1), NAs)] <- NA
hip[c(which(allover == 1), NAs)] <- NA
knee[c(which(allover == 1), NAs)] <- NA
abdo[c(which(allover == 1), NAs)] <- NA
headache[c(which(allover == 1), NAs)] <- NA
facial[c(which(allover == 1), NAs)] <- NA
#nopain[which(allover == 1)] <- NA # unnecessary as done below for all other pain types

#nopain <- rep(0, nrow(bd))
nopain[back == 1 | neck == 1 | hip == 1 | knee == 1 | abdo == 1 | headache == 1 | face == 1 | allover == 1] <- NA
# MF: same as above
nopain[NAs] <- NA #CONTROL #means unless all non-chronic pain types are 0, nopain should be NA
# so nopain ("none of the above") is control only if reported in all instances with no other pain type (or NA) reported 

#### CHRONIC
#back, 3571, cols:15-18
#neck/shoulder, 3404, cols:7-10
#hip, 3414, cols:11-14
#knee, 3773, cols:23-26
#abdominal/stomach, 3741, cols:19-22
#headache, 3799, cols: 27-30
#facial, 4067, cols: 31-34
#general, 2956, cols:3-6

#### Chronic

# make function to assign 
#getchron<-function(dset) {
  #tmp<-sapply(dset,as.character)
  #tmp<-ifelse(tmp==1,1,ifelse(tmp==0,0,NA))
#  tmp<-apply(dset,2,as.numeric)             #converts each value of each column to a numeric value and puts resulting matrix in tmp variable 
#  w <- which(apply(is.na(tmp), 1, all))     #extract indices of rows where all values are NA and store them in w variable
#  tmp<-rowSums(tmp,na.rm=T)                 # calculate sum of values in each row of tmp and ignore any NA values when doing so
#  tmp[w]<-NA                                #makes NA rows of tmp corresponding to indices of rows where values are all NA 
#  tmp[tmp>0]<-1                             #replaces any non-zero values in tmp with 1 
#  return(tmp)}

# MF: modifed to extract 1s only (cases)
getchron <- function(dset) {
  #tmp<-apply(dset,2,as.numeric)             #converts each value of each column to a numeric value and puts resulting matrix in tmp variable 
  w <- which(apply(is.na(dset), 1, all))
  dset[dset != 1] <- NA                   #extract indices of rows where all values are NA and store them in w variable
  tmp <- rowSums(dset, na.rm=T)                 # calculate sum of values in each row of tmp and ignore any NA values when doing so
  tmp[w] <- NA                                #makes NA rows of tmp corresponding to indices of rows where values are all NA 
  tmp[tmp == 0] <- NA
  tmp[tmp > 0] <- 1                             #replaces any non-zero values in tmp with 1 
  return(tmp)}
  

c_back <- getchron(bd[, grep("c_back.*", names(bd))])
c_neck <- getchron(bd[, grep("c_neck.*", names(bd))])
c_hip <- getchron(bd[, grep("c_hip.*", names(bd))])
c_knee <- getchron(bd[, grep("c_knee.*", names(bd))])
c_abdo <- getchron(bd[, grep("c_abdo.*", names(bd))])
c_headache <- getchron(bd[, grep("c_headache.*", names(bd))])
c_facial <- getchron(bd[, grep("c_facial.*", names(bd))])
c_general <- getchron(bd[, grep("c_general.*", names(bd))]) #Need to make rows containing this NA

# Make sure corresponding pain was experienced in the last month for all chronic pain types reported
# MF: we don't need them, they are confusing, so we better remove them
#cBack[which(back==0)]<-NA
#cNeck[which(neck==0)]<-NA
#cHip[which(hip==0)]<-NA
#cKnee[which(knee==0)]<-NA
#cAbdo[which(abdo==0)]<-NA
#cHeadache[which(headache==0)]<-NA
#cFace[which(face==0)]<-NA
#cGeneral[which(allover==0)]<-NA

# Chronic pain types + General pain = NA
# MF: we need to NA all uncertain phenotypes= General + Pain all over from 6159
c_back[c(which(c_general == 1), which(allover == 1), NAs)] <- NA
c_neck[c(which(c_general == 1), which(allover == 1), NAs)] <- NA
c_hip[c(which(c_general == 1), which(allover == 1), NAs)] <- NA
c_knee[c(which(c_general == 1), which(allover == 1), NAs)] <- NA
c_abdo[c(which(c_general == 1), which(allover == 1), NAs)] <- NA
c_headache[c(which(c_general== 1), which(allover == 1), NAs)] <- NA
c_facial[c(which(c_general == 1), which(allover == 1), NAs)] <- NA

# MF: Add controls, we don't need to have a separate control phenotype
c_back[which(nopain == 1)] <- 0
c_neck[which(nopain == 1)] <- 0
c_hip[which(nopain == 1)] <- 0
c_knee[which(nopain == 1)] <- 0
c_abdo[which(nopain == 1)] <- 0
c_headache[which(nopain == 1)] <- 0
c_facial[which(nopain == 1)] <- 0

# Need to handle other options (-3, -2, -1) that are displayed in the pheno file! Make them NA?
# Could do : anything<0 <- NA?


###### Pull things together
dset<-data.frame(fid, iid, sex, age, cau,
                 c_back,c_neck,c_hip,c_knee,c_abdo,c_headache,c_facial)

dsetC<-dset[dset$CAU%in%"1",]
write.csv(dsetC, "dsetC4.csv", row.names = FALSE)
#write.table(dsetC, file = "dsetC4.txt", sep = "\t", row.names = FALSE)

