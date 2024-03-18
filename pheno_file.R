####### Phenotypic file 

# Include age at recruitment and reported sex
# Focus on caucasians (cau) only
# Traits: chronic pain types, exclude general pain
# Control: no pain reported, chronic or non-chronic
# (so valid if only "none of the above" from non-chronic pain question (code 6159) was reported)

# Load bd dataframe
load("../../data/raw/bd671644_orig_sasha.Robj", verbose = TRUE)


### CLARIFYING BD COLUMN NAMES (replacing numerical codes with traits)

# Each bd file column names follows structure: "Field_ID.Instance#.Array#" 
# Field_ID: UK Biobank code for a specific phenotypic trait
# Instance: ith (order of thee) occasion where participants had measurements performed 
# (e.g. Instance 0 is first time participants came in, Instance 3 is the fourth time they were asked to come in)
# Array: ith (order of the) answer given by participant to a question in one instance 
# e.g. Array 0 is the first answer given, Array 4 is the fifth answer given to the same question
# If all participants attended a clinic 3 times and had their height measured twice during each visit, 
# then the data-field would have 3 instances, each of which being an array of 2 values.
# As data across instances and arrays will be concatenated in final phenotype df for each participant, only
# field ID section of bd column names is relevant and need to be clarified

### Traits - Field_ID equivalence from UK Biobank
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
# Pain type(s) experienced in the last month, 6159 (non-chronic pain types):
#     headache, 1 
#     facial pain, 2 
#     neck/shoulder pain, 3
#     back pain, 4 
#     stomach/abdominal pain, 5
#     hip pain, 6 
#     knee pain, 7 
#     pain all over the body, 8 
#     none of the above, -7 
#     prefer not to say, -3
# should i put the detail of the 6159 codes in the non-chronic pain section rather?

# OPTION 1: MANUALLY CHANGING BD COLNAMES
colnames(bd) <- gsub("f.", "", colnames(bd), fixed = T)
colnames(bd) <- gsub("31", "sex", colnames(bd), fixed = T)
colnames(bd) <- gsub("21022", "age", colnames(bd), fixed = T)
colnames(bd) <- gsub("22006", "caucasian", colnames(bd), fixed = T)
colnames(bd) <- gsub("3571", "back_pain", colnames(bd), fixed = T)
colnames(bd) <- gsub("3404", "neck_pain", colnames(bd), fixed = T)
colnames(bd) <- gsub("3414", "hip_pain", colnames(bd), fixed = T)
colnames(bd) <- gsub("3773", "knee_pain", colnames(bd), fixed = T)
colnames(bd) <- gsub("2956", "general_pain", colnames(bd), fixed = T)
colnames(bd) <- gsub("3799", "headache", colnames(bd), fixed = T)
colnames(bd) <- gsub("4067", "facial_pain", colnames(bd), fixed = T)
colnames(bd) <- gsub("3741", "abdominal_pain", colnames(bd), fixed = T)
colnames(bd) <- gsub("6159", "non-chronic", colnames(bd), fixed = T)

# OPTION 2: LOOPING TO CHANGE BD COLNAMES- not necessary as we decided not to include instance/arrays anymore
better_colnames <- c(
  "f." = "",
  "31" = "sex",
  "21022" = "age",
  "22006" = "caucasian",
  "3571" = "back_pain",
  "3404" = "neck_pain",
  "3414" = "hip_pain",
  "3773" = "knee_pain",
  "2956" = "general_pain",
  "3799" = "headache",
  "4067" = "facial_pain",
  "3741" = "abdominal_pain",
  "6159" = "non-chronic")

for (column in names(better_colnames)) {
  bd <- setNames(bd, gsub(column, better_colnames[column], names(bd), fixed = TRUE))
}

# or use this vector: 
# chronic_columns <- c("back_pain", "neck_pain", "hip_pain", "knee_pain", "general_pain", "facial_pain", 
# "headache", "abdominal_pain", "sex", "age", "caucasian", "non-chronic")? => for col in chronic_columns
for (col in names(better_colnames)) {
  for (i in 0:3) {
    for (j in 0:6) {
      col_to_replace <- paste0(better_colnames[col], ".", i, ".", j)
      new_col_name <- paste0(better_colnames[col], ".instance", i + 1, ".array", j+1)
      names(bd) <- gsub(col_to_replace, new_col_name, names(bd), fixed = TRUE)
    }
  }
}   
    
# OPTION 3: CREATING A DF? // EXTRACTING TRAITS FROM DATACODE AND MERGING
codes_id <- data.frame(age = 21022,
                       sex = 31)


### PULL BD CONTENT INTO TRAIT-SPECIFIC VECTORS

# ID, age, sex and ethnic grouping were only reported once (so no instance/array to deal witb)

# ID: FID (family ID) and IID (individual ID) are both required for later uses of the phenotype file
# FID
fid <- bd[, "eid"]
# should i add drop = FALSE to ensure that the result is a dataframe?or is it fine as a vecotr
# IID
iid <- bd[, "eid"]

# Sex
sex <- bd[, grep("sex", names(bd))] # select column sex.0.0
#w<-which(is.na(Sex) & !is.na(bd.rec[,5])) #not sure what this is for
#Sex[w]<-bd.rec[w,5]

# Age
age <- bd[, grep("age", names(bd))] # select age.0.0

# Genetic ethnic grouping - Causasian
cau <- bd[, grep("caucasian", names(bd))] # select caucasian.0.0 

### Non-chronic pain
# Non-chronic pain is necessary to define chronic pain types and control so need to create 
# individual vectors for each non-chronic pain type (field 6159, answers 1:8, -3, -7, see detail above)

nc_pain <- bd[, grep("non-chronic", names(bd))]
# had this written before: pain<-sapply(pain,as.character)
nc_pain[nc_pain == -7] <- 9 # swap to avoid clash with knee (7)

# Concatenate all instances and arrays into one element per participant for each non-chronic pain type and store 
# the rows (participant) in separate vectors for each nc pain type
getpain <- function(trait) {
  res <- vector()
  for(i in 1:ncol(pain)) {                  #loop to iterate over each column of the pain data frame
    w <- grep(trait, pain[, i])                 #search for the presence of the trait in the ith column of the pain data frame and stores it in w variable
    if(length(w) != 0) res <- append(res, w)}    #checks if w contains any elements (so if trait was found in ith column). If w is not empty, the indices stored in w are appended to the res vector
  return(unique(res))}                      #returns the unique elements in the res vector (=indices of columns where the trait was found)

# PREVIOUSLY:
# nc_back <- getpain(4); tmp <- rep(0, nrow(bd)); tmp[nc_back] <- 1; nc_back <- tmp
# The result of this is "double" data type elements- takes up more memory unnecessarily
# SO Replacing with this idea:
# nc_back <- getpain(4)
# nc_back <- as.integer(1:nrow(bd) %in% nc_back) directly integer, less memory-intensive

# Replace nc_pain codes with explicit word names
better_names <- c(
  "1" = "headache",
  "2" = "facial",
  "3" = "neck",
  "4" = "back",
  "5" = "abdominal",
  "6" = "hip",
  "7" = "knee",
  "8" = "allover",
  "9" = "nopain")
for (pain_code in names(better_names)) {
  pain_indices <- getpain(pain_code) # add as.numeric to pain_code? works without
  assign(paste0("nc_", better_names[pain_code]), as.integer(1:nrow(bd) %in% pain_indices))
}
nc_pain[nc_pain == -3] <- NA # prefer not to say

# ALSO PREVIOUSLY had :
# nopain <- getpain(9); tmp <- rep(0, nrow(bd)) ;tmp[nopain] <- 1; nopain <- tmp  
# MF: not required as it is done below? <- don't understand this comment?

# REFINE NON-CHRONIC PAIN TYPES

# Store rows where all elements are NA in nc_pain
NAs <- which(apply(is.na(pain), 1, all))

# Exclude participants who reported pain all over and only NA answers in questionnaire
# Make a loop instead?
nc_back[c(which(nc_allover == 1), NAs)] <- NA
nc_neck[c(which(nc_allover == 1), NAs)] <- NA
nc_hip[c(which(nc_allover == 1), NAs)] <- NA
nc_knee[c(which(nc_allover == 1), NAs)] <- NA
nc_abdominal[c(which(nc_allover == 1), NAs)] <- NA
nc_headache[c(which(nc_allover == 1), NAs)] <- NA
nc_facial[c(which(nc_allover == 1), NAs)] <- NA

# More intricate definition of nopain: ONLY answer given by participant must be nopain,
# not valid if other pain types have been reported too or if all elements of the row were NA 
nc_nopain[nc_back == 1 | nc_neck == 1 | nc_hip == 1 | nc_knee == 1 | nc_abdominal == 1 | nc_headache == 1 | nc_facial == 1 | nc_allover == 1] <- NA
nc_nopain[NAs] <- NA 
# nopain ("none of the above") is CONTROL 

### Chronic pain

# Extract 1s only (cases)
getchron <- function(dset) {
  #tmp<-apply(dset,2,as.numeric)             #converts each value of each column to a numeric value and puts resulting matrix in tmp variable 
  w <- which(apply(is.na(dset), 1, all))
  dset[dset != 1] <- NA                   #extract indices of rows where all values are NA and store them in w variable
  tmp <- rowSums(dset, na.rm=T)                 # calculate sum of values in each row of tmp and ignore any NA values when doing so
  tmp[w] <- NA                                #makes NA rows of tmp corresponding to indices of rows where values are all NA 
  tmp[tmp == 0] <- NA
  tmp[tmp > 0] <- 1                             #replaces any non-zero values in tmp with 1 
  return(tmp)
}

# Instead of this: back <- getchron(bd[, grep("back_pain", names(bd))])
# Loop the getchron() function across pain types
# OPTION 1
for (col in names(bd)) {
  if (grepl("pain", col)) {
    pain_type <- sub("_pain.*", "", col) # so can grep for each pain_type (all instances/arrays) when applying getchron()
    assign(pain_type, getchron(bd[, grep(pain_type, names(bd))]))
  } else if (grepl("headache", col)) {
    assign("headache", getchron(bd[, grep("headache", names(bd))]))
  }
}

# OPTION 2 (more intermediate variables + checking parameters, add error message?)
for (col in names(bd)) {
  if (grepl("pain", col)) {
    pain_type <- sub("_pain.*", "", col)
    subset_cols <- grep(pain_type, names(bd))
    if (length(subset_cols) > 0) {
      assign(pain_type, getchron(bd[, subset_cols]))
    }
  } else if (grepl("headache", col)) {
    subset_cols <- grep("headache", names(bd))
    if (length(subset_cols) > 0) {
      assign("headache", getchron(bd[, subset_cols]))
    }
  }
}

# Chronic pain type AND General pain (chronic)/Pain all over (non-chronic) = uncertain phenotype, make NA
back[c(which(general == 1), which(nc_allover == 1), NAs)] <- NA
neck[c(which(general == 1), which(nc_allover == 1), NAs)] <- NA
hip[c(which(general == 1), which(nc_allover == 1), NAs)] <- NA
knee[c(which(general == 1), which(nc_allover == 1), NAs)] <- NA
abdominal[c(which(general == 1), which(nc_allover == 1), NAs)] <- NA
headache[c(which(general== 1), which(nc_allover == 1), NAs)] <- NA
facial[c(which(general == 1), which(nc_allover == 1), NAs)] <- NA

# Why is this 0 and not NA?
# MF: Add controls, we don't need to have a separate control phenotype
back[which(nopain == 1)] <- 0
neck[which(nopain == 1)] <- 0
hip[which(nopain == 1)] <- 0
knee[which(nopain == 1)] <- 0
abdominal[which(nopain == 1)] <- 0
headache[which(nopain == 1)] <- 0
facial[which(nopain == 1)] <- 0

# Pull all vectors into phenotype dataframe
dset<-data.frame(fid, iid, sex, age, cau,
                 back, neck, hip, knee, abdominal, headache, facial)

dsetC<-dset[dset$cau%in%"1",]
# write.csv(dsetC, "dsetC4.csv", row.names = FALSE)
# write.table(dsetC, file = "dsetC4.txt", sep = "\t", row.names = FALSE)

