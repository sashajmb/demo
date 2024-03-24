####### Phenotypic file 

# Include age at recruitment and reported sex
# Focus on caucasians (cau) only
# Traits: chronic pain types, exclude general pain
# Control: no pain reported, chronic or non-chronic
# (so valid if only "none of the above" from non-chronic pain question (code 6159) was reported)

# Load bd dataframe
load("../../data/raw/bd671644_orig_sasha.Robj", verbose = TRUE)

### CLARIFYING BD COLUMN NAMES (replacing numerical codes with traits)

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

### PULL BD CONTENT INTO TRAIT-SPECIFIC VECTORS

# FID
fid <- bd[, "eid"]

# IID
iid <- bd[, "eid"]

# Sex
sex <- bd[, grep("sex", names(bd))] # select column sex.0.0

# Age
age <- bd[, grep("age", names(bd))] # select age.0.0

# Genetic ethnic grouping - Causasian
cau <- bd[, grep("caucasian", names(bd))] # select caucasian.0.0 

### Non-chronic pain

nc_pain <- bd[, grep("non-chronic", names(bd))]

# Concatenate all instances and arrays into one element per participant for each non-chronic pain type and store 
# the rows (participant) in separate vectors for each nc pain type
getpain <- function(trait) {
  participant_indices <- vector()
  for(i in 1:ncol(nc_pain)) {                  
    new_participants <- grep(trait, nc_pain[, i])                 
    if(length(new_participants) != 0) participant_indices <- append(participant_indices, new_participants)}   
  return(unique(participant_indices))}  # avoid duplicates                   

# Apply getpain() to following traits based on answer codes to UKB field 6159
pain_names <- c(
  "1" = "headache",
  "2" = "facial",
  "3" = "neck",
  "4" = "back",
  "5" = "abdominal",
  "6" = "hip",
  "7" = "knee",
  "8" = "allover",
  "-7" = "nopain")

nc_pain[nc_pain == "-3"] <- NA # "prefer not to say" is non-applicable, no vector needed

for (pain_code in names(pain_names)) {
  pain_indices <- getpain(pain_code) 
  assign(paste0("nc_", pain_names[pain_code]), as.integer(1:nrow(bd) %in% pain_indices))
}

# REFINE NON-CHRONIC PAIN TYPE VECTORS

# Rows where all elements are NA in nc_pain
all_NAs <- which(apply(is.na(nc_pain), 1, all))

# Exclude participants who reported pain all over and only NA answers in questionnaire
nc_back[c(which(nc_allover == 1), all_NAs)] <- NA
nc_neck[c(which(nc_allover == 1), all_NAs)] <- NA
nc_hip[c(which(nc_allover == 1), all_NAs)] <- NA
nc_knee[c(which(nc_allover == 1), all_NAs)] <- NA
nc_abdominal[c(which(nc_allover == 1), all_NAs)] <- NA
nc_headache[c(which(nc_allover == 1), all_NAs)] <- NA
nc_facial[c(which(nc_allover == 1), all_NAs)] <- NA

# nopain: not valid if other pain types have been reported too or if all elements of the row were NA 
nc_nopain[nc_back == 1 | nc_neck == 1 | nc_hip == 1 | nc_knee == 1 | nc_abdominal == 1 | nc_headache == 1 | nc_facial == 1 | nc_allover == 1] <- NA
nc_nopain[all_NAs] <- NA 

### Chronic pain

# Extract 1s only (cases)
getchron <- function(trait_subset) {
  all_NAs_indices <- which(apply(is.na(trait_subset), 1, all))
  trait_subset[trait_subset != 1] <- NA                   
  summed <- rowSums(trait_subset, na.rm=T)                 
  summed[all_NAs_indices] <- NA                                
  summed[summed == 0] <- NA
  summed[summed > 0] <- 1                            
  return(summed)
}

# Apply getchron() across chronic pain types
pain_types <- unique(gsub("_pain.*", "", names(bd)[grep("pain", names(bd))]))
pain_types <- c(pain_types, "headache")
for (type in (pain_types)) {
  assign(type, as.integer(getchron(bd[, grep(type, names(bd))])))
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

