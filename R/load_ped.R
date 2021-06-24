#' @param infile Path to the input file
#' @param header T (Default) or F
#' @return A matrix of the infile
#' @export
load_ped <- function(infile, header = TRUE){

## read file as a matrix ##
  in.dt <- data.table::fread(infile, header = header)

## conduct checks ##

  #- check column names
    nameVec = c('IID', 'sample', 'FID', 'family', 'PID', 'father', 'DID',
                'MID', 'mother', 'mom', 'sex', 'gender', 'phenotype',
                'phen', 'pheno' ,'trait')
    nameVec = toupper(nameVec)
    colnames(in.dt) = toupper(colnames(in.dt))

    if(!any(colnames(in.dt) %in% nameVec)) stop(name.rules())
    colnames(in.dt)[which(colnames(in.dt) %in% c("IID", "SAMPLE"))] <- 'IID'
    colnames(in.dt)[which(colnames(in.dt) %in% c("FID", 'FAMILY'))] <- 'FID'
    colnames(in.dt)[which(colnames(in.dt) %in% c("PID", "FATHER", 'DID', 'PID'))] <- 'PID'
    colnames(in.dt)[which(colnames(in.dt) %in% c("MID", "MOTHER", "MOM"))] <- 'MID'
    colnames(in.dt)[which(colnames(in.dt) %in% c("SEX", "GENDER"))] <- 'SEX'
    colnames(in.dt)[which(colnames(in.dt) %in% c("PHENOTYPE", "PHEN", "PHENO", "TRAIT"))] <- 'PHEN'

  #- check for duplicated column names
  if (length(unique(colnames(in.dt))) < length(colnames(in.dt))) stop(col.duplicated())

  #- check if there's duplicated IDs
  if (length(unique(paste0(in.dt$IID,'-', in.dt$FID))) < length(paste0(in.dt$IID, '-', in.dt$FID))) stop(id.duplicatd())

  ## functions ##

    name.rules <- function(){
      writeLines("Column names should be as listed (not case-sensitive):
                  Sample can be 'IID', 'sample'
                  Family can be 'FID', 'family'
                  Father can be 'PID', 'dad', 'DID', 'father'
                  Mother can be 'MID', 'mother', 'mom'
                  Sex can be 'sex', 'gender'
                  Phenotype can be 'phenotype', 'phen','pheno' , 'trait'" )
    }

    value.rules <- function(){
      writeLines("Columns can contain the following values (not case-sensitive):
                  Males as 'M', 'male', 1
                  Females as 'F', 'female', 2
                  Unknown as NA, -9, 'unk'")
    }
    col.duplicated <- function(){
      writeLines('Your table has duplicated column names')

    }

    id.duplicated <- function(){
      writeLines('You have multiple samples within a family with the same ID')
    }

    in.dt
}
