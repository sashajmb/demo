####### Phenotypic file 2
#Use age at recruitment and reported sex
#Focus on CAU only
#Look at all chronic pain, exclude general pain
#Control: no pain at all ("none of the above" from 6159)

setwd("/data/SBBS-FreydinLab/msc/sasha/data/")
load("/data/SBBS-FreydinLab/msc/sasha/data/bd671644_orig_sasha.Robj", verbose = TRUE)

### Phenotypes - codes and columns
#Age at recruitment, 21002
#Sex, 31
#Genetic ethnic grouping, 22006
#Back pain for 3+ months, 3571
#Neck/shoulder pain for 3+ months, 3404
#Hip pain for 3+ months, 3414
#Knee pain for 3+ months, 3773
#General pain for 3+ months, 2956 -> should be considered NA
#Headache for 3+ months, 3799
#Facial pains for 3+ months, 4067
#Stomach/abdominal pain for 3+ months, 3741
#Pain type(s) experienced in the last month, 6159 -> only use "none of the above", rest is NA

#Do non-chronic pain types need to be reported as 0 or NA?
#Do chronic pains require the corresponding pain in the last month to be reported as well (as per OG script)? 
#Doing this here

### ID
FID<-bd[,1]
IID<-bd[,1]

### Sex
Sex<-bd[,2]

### Age
Age<-bd[,63]

### Genetic ethnicity
CAU<-bd[,64]

#### PAIN

###Control
# make vectors for non chronic pain types- field 6159
# new version, include all non-chronic pain types (to be excluded from final phenotype file df) so that they can be used
# to define the chronic pain types (as the corresponding non-chronic pain must have been reported in the last month 
# if they have chronic pain) + need all pain types that include "pain all over" or "general chronic pain" to be made NA)
# (in other words, whoever reported pain allover the body for 3+ months or in 6159 must be NA for any other phenotype), 
# and need pain types to differentiate "None of the above" (nopain) used as control (where only "none of the above" was 
# reported in all instances) VS inconsistent data when pain+nopain was reported (to be made NA).

pain<-bd[,35:62] #6159
#pain<-sapply(pain,as.character)
pain[pain==-7]<--9 # MF: to avoid clash with knee

getpain<-function(trait) {
  res<-vector()
  for(i in 1:ncol(pain)) {                  #loop to iterate over each column of the pain data frame
    w<-grep(trait,pain[,i])                 #search for the presence of the trait in the ith column of the pain data frame and stores it in w variable
    if(length(w)!=0) res<-append(res,w)}    #checks if w contains any elements (so if trait was found in ith column). If w is not empty, the indices stored in w are appended to the res vector
  return(unique(res))}                      #returns the unique elements in the res vector (=indices of columns where the trait was found)

back<-getpain(4);tmp<-rep(0,nrow(bd));tmp[back]<-1;back<-tmp
neck<-getpain(3);tmp<-rep(0,nrow(bd));tmp[neck]<-1;neck<-tmp    
hip<-getpain(6);tmp<-rep(0,nrow(bd));tmp[hip]<-1;hip<-tmp
knee<-getpain(7);tmp<-rep(0,nrow(bd));tmp[knee]<-1;knee<-tmp
abdo<-getpain(5);tmp<-rep(0,nrow(bd));tmp[abdo]<-1;abdo<-tmp
headache<-getpain(1);tmp<-rep(0,nrow(bd));tmp[headache]<-1;headache<-tmp
face<-getpain(2);tmp<-rep(0,nrow(bd));tmp[face]<-1;face<-tmp
allover<-getpain(8);tmp<-rep(0,nrow(bd));tmp[allover]<-1;allover<-tmp
nopain<-getpain(-9);tmp<-rep(0,nrow(bd));tmp[nopain]<-1;nopain<-tmp  # MF: not required as it is done below
pain[pain==-3]<-NA #prefer not to say

## MF: this is much quicker
NAs <- which(apply(is.na(pain), 1, all))

# pain type + pain all over = NA
# MF: added NAs as we need to exclude them here, too
back[c(which(allover==1),NAs)]<-NA
neck[c(which(allover==1),NAs)]<-NA
hip[c(which(allover==1),NAs)]<-NA
knee[c(which(allover==1),NAs)]<-NA
abdo[c(which(allover==1),NAs)]<-NA
headache[c(which(allover==1),NAs)]<-NA
face[c(which(allover==1),NAs)]<-NA
#nopain[which(allover==1)]<-NA # unnecessary as done below for all other pain types

#nopain <- rep(0, nrow(bd))
nopain[back==1 | neck==1 | hip==1 | knee==1 | abdo==1 | headache==1 | face==1 | allover==1] <- NA
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
#face, 4067, cols: 31-34
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
getchron<-function(dset) {
  #tmp<-apply(dset,2,as.numeric)             #converts each value of each column to a numeric value and puts resulting matrix in tmp variable 
  w <- which(apply(is.na(dset), 1, all))
  dset[dset!=1]<-NA                   #extract indices of rows where all values are NA and store them in w variable
  tmp<-rowSums(dset,na.rm=T)                 # calculate sum of values in each row of tmp and ignore any NA values when doing so
  tmp[w]<-NA                                #makes NA rows of tmp corresponding to indices of rows where values are all NA 
  tmp[tmp==0]<-NA
  tmp[tmp>0]<-1                             #replaces any non-zero values in tmp with 1 
  return(tmp)}
  

cBack<-getchron(bd[,15:18])
cNeck<-getchron(bd[,7:10])
cHip<-getchron(bd[,11:14])
cKnee<-getchron(bd[,23:26])
cAbdo<-getchron(bd[,19:22])
cHeadache<-getchron(bd[,27:30])
cFace<-getchron(bd[,31:34])
cGeneral<-getchron(bd[,3:6]) #Need to make rows containing this NA

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
cBack[c(which(cGeneral==1),which(allover==1),NAs)]<-NA
cNeck[c(which(cGeneral==1),which(allover==1),NAs)]<-NA
cHip[c(which(cGeneral==1),which(allover==1),NAs)]<-NA
cKnee[c(which(cGeneral==1),which(allover==1),NAs)]<-NA
cAbdo[c(which(cGeneral==1),which(allover==1),NAs)]<-NA
cHeadache[c(which(cGeneral==1),which(allover==1),NAs)]<-NA
cFace[c(which(cGeneral==1),which(allover==1),NAs)]<-NA

# MF: Add controls, we don't need to have a separate control phenotype
cBack[which(nopain==1)]<-0
cNeck[which(nopain==1)]<-0
cHip[which(nopain==1)]<-0
cKnee[which(nopain==1)]<-0
cAbdo[which(nopain==1)]<-0
cHeadache[which(nopain==1)]<-0
cFace[which(nopain==1)]<-0


# Need to handle other options (-3, -2, -1) that are displayed in the pheno file! Make them NA?
# Could do : anything<0 <- NA?


###### Pull things together
dset<-data.frame(FID,IID,Sex,Age,CAU,
                 cBack,cNeck,cHip,cKnee,cAbdo,cHeadache,cFace)

dsetC<-dset[dset$CAU%in%"1",]
write.csv(dsetC, "dsetC4.csv", row.names = FALSE)
#write.table(dsetC, file = "dsetC4.txt", sep = "\t", row.names = FALSE)

