####### Phenotypic file 

# Include age at recruitment and reported sex
# Focus on caucasians (cau) only
# Traits: chronic pain types, exclude general pain
# Control: no pain reported, chronic or recent
# (so valid if only "none of the above" from recent pain question (code 6159) was reported)

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
  "6159" = "recent")

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


### Recent pain (experienced in the last month)

recentpain_subset <- bd[, grep("recent", names(bd))]

# Concatenate all instances and arrays into one element per participant for each recent pain type and store 
# the rows (participant) in separate vectors for each recent pain type
get_participants_per_recentpain_type <- function(trait) {
  participants <- vector()
  for(i in 1:ncol(recentpain_subset)) {                  
    new_participants <- grep(trait, recentpain_subset[, i])                 
    if(length(new_participants) != 0) {
      participants <- append(participants, new_participants)
      }
    }   
  return(unique(participant_indices))  # avoid duplicates
}

# Apply getpain() to following traits based on answer codes to UKB field 6159
recentpain_names <- c(
  "1" = "headache",
  "2" = "facial",
  "3" = "neck",
  "4" = "back",
  "5" = "abdominal",
  "6" = "hip",
  "7" = "knee",
  "8" = "allover",
  "-7" = "nopain")

# Substitute "-3" ("prefer not to say") with NA, does not need to be a vector
recentpain_subset[recentpain_subset == "-3"] <- NA

NAs_in_recentpain <- is.na(recentpain_subset) # logical (TRUE/FALSE) matrix of which element in recentpain_subset are NA
NA_participants <- which(apply(NAs_in_recentpain, 1, all)) # Rows where all elements are NA in recentpain_subset

get_recentpain_vector <- function(trait) {
  recentpain_participants <- get_participants_per_recentpain_type(trait)
  new_vector <- paste0("recent_", recentpain_names[trait])
  all_participants <- as.integer(1:nrow(bd) %in% recentpain_participants) # participants with recentpain=1, rest of participants in bd=0, whether they are control(nopain) or NA
  all_participants[NA_participants] <- NA # distinguish which 0 are really 0s and which are actually NAs using NA_participants list of indices
  assign(new_vector, all_participants)
  if(recentpain_names[trait] != "recent_allover") {
    get(new_vector)[which(recent_allover == 1)] <- NA
  }
}

# need to run the function on all_over first
get_recentpain_vector(names(recentpain_names[recentpain_names == "allover"]))
# then on rest of the recent pain types
for (pain_code in names(recentpain_names)) {
  get_recentpain_vector(pain_code) 
}

# nopain: not valid if other pain types have been reported too or if all elements of the row were NA 
recent_nopain[recent_back == 1 | recent_neck == 1 | recent_hip == 1 | recent_knee == 1 | recent_abdominal == 1 | recent_headache == 1 | recent_facial == 1] <- NA


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

# Chronic pain type AND General pain (chronic)/Pain all over (recent) = uncertain phenotype, make NA
back[c(which(general == 1), which(recent_allover == 1), NAs)] <- NA
neck[c(which(general == 1), which(recent_allover == 1), NAs)] <- NA
hip[c(which(general == 1), which(recent_allover == 1), NAs)] <- NA
knee[c(which(general == 1), which(recent_allover == 1), NAs)] <- NA
abdominal[c(which(general == 1), which(recent_allover == 1), NAs)] <- NA
headache[c(which(general== 1), which(recent_allover == 1), NAs)] <- NA
facial[c(which(general == 1), which(recent_allover == 1), NAs)] <- NA

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

