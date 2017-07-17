library(readxl)
library(dplyr)

#compare two files.  
#the result:
#each file is reduces to columns that are common if both files
#both files are combined into one
#2 columns are added to the result:
#column ROW_IDENTITY: "A" value indicates that the record belongs to first file, "B" to the second file.
#column ROW_IDENTITY_DUP: "A.dup" value indicates that record is a duplicate record of another record in file A



#Save the two files in a directory and set that directory as home directory
file.differences <- function(A_file_name, B_file_name){
  
  #Load A file
  ds.A <- read_excel(A_file_name,sheet=1,skip = 0)
  
  #Load B file
  ds.B <- read_excel(B_file_name,sheet=1,skip = 0)
  
  #Set both datasets into commont columns
  common_fields <- dplyr::intersect(names(ds.A), names(ds.B))
  
  ds.A <- ds.A %>% select(common_fields)
  
  ds.B <- ds.B %>% select(common_fields)
  
  #set file row ids
  ds.A$FILE_ROW_ID <- apply(ds.A,MARGIN = 1, function(x) digest::digest(x,algo = 'sha1') )
  
  ds.B$FILE_ROW_ID <- apply(ds.B,MARGIN = 1, function(x) digest::digest(x,algo = 'sha1') )
  
  #Records in B not in A
  A_nB <- (dplyr::setdiff(ds.A,ds.B))$FILE_ROW_ID
  #Records in B not in A
  B_nA <- (dplyr::setdiff(ds.B,ds.A))$FILE_ROW_ID
  
  #set ROW_IDENTITY
  ds.A$FILE_IDENTITY <- 'A'  
  
  ds.B$FILE_IDENTITY <- 'B'
  
  ds.A$ROW_IDENTITY_DUP <- NA  
  
  ds.B$ROW_IDENTITY_DUP <- NA
  
  ds.A$ROW_IDENTITY_XNY <- NA  
  
  ds.B$ROW_IDENTITY_XNY <- NA
  
  #mark duplicate records
  ds.A[duplicated(ds.A),'ROW_IDENTITY_DUP'] <- 'A.dup'
  ds.B[duplicated(ds.B),'ROW_IDENTITY_DUP'] <- 'B.dup'
  
  #missing records differences
  ds.A[ds.A$FILE_ROW_ID %in% A_nB,'ROW_IDENTITY_XNY'] = 'A_nB'
  ds.B[ds.B$FILE_ROW_ID %in% B_nA,'ROW_IDENTITY_XNY'] = 'B_nA'
  
  ds <- bind_rows(ds.A,ds.B)
  
  return(ds)
}