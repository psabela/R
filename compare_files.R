#CREATED BY: Peter Sabela

#DESCRIPTION: compare two Excel files.

#PARAMETERS: are two excel file names with extensions. Ex. 'file1.xlsx','file1.xlsx'

#RETURNS: single data frame.  Each file is reduces to columns that are common if both files.  Then, both files are row binded into one data frame.  
   #Next, 4 columns are appended with the results:
   #column FILE_ROW_ID:      is a digest::digest(x,algo = 'sha1') of each record in the data.frame.
   #column FILE_IDENTITY:    values: "A","B".  "A" value indicates that the record belongs to the first file, "B" to the second file.
   #column ROW_IDENTITY_DUP: values: "A.dup", "B.dup".  "A.dup" value indicates that record is a duplicate record of another record in file A.  "B.dup" is a duplicate of another record in file B.
   #column ROW_IDENTITY_XNY: valeus: "A_nB","B_nA".  "A_nB" value indicates that the records exists in file A but does not exist in file B.  "B_nA"

library(readxl)
library(dplyr)

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
