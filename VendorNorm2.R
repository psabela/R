#AUTOLOAD REQUIRED PACKAGES

installed_packages <- 'pacman' %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) { install.packages("pacman") }

library(pacman)
pacman::p_load(tidyverse, readr, lubridate, NLP, tm)


#FUNCTION TO CLEAN UP VENDOR NAMES

VendorNorm2 <- function(ven, code = FALSE){
  
  #custom functions
  
  splitpaste  <- function(x,z){
    for(i in 1 : length(x))
    {
      
      #y <-unlist(strsplit(x[i], split = ' '))[1:2]
      y <-unlist(strsplit(x[i], split = ' '))
      y <- y[!(y %in% stopwords(kind='en'))]
      y <- y[1:2]
      
      if(is.na(y[2])){
        z[i] <- y
      }
      else{
        z[i]  <- paste(y[1],y[2], sep = " ")
      }
    }
    z      
  }
  
  
  
  
  ps_removeWords  <- function(x,wordList){
    for(i in 1 : length(x))
    {
      if(!is.na(x[i]))
      {
        if(!x[i] == '')
        {
          y <-unlist(strsplit(as.character(x[i]), split = ' '))
          L <- which(!{y %in% wordList})
          if(length(L)>0)
          {
            y <- y[L]
            x[i] <-  paste(y,collapse = ' ')
          }
          else
          {
            x[i] <- ''
          }
        }
      }
    }
    x    
  }
  
  ps_trimEnd <- function(x,pattern = '(s|S)$',len=1){
    for(i in 1 : length(x))
    {
      if(!is.na(x[i]))
      {
        if(!x[i] == '')
        {
          y <-unlist(strsplit(as.character(x[i]), split = ' '))
          
          for(k in seq(y))
          {
            if(grepl(pattern,y[k]))
              y[k] <- substr(y[k],1,nchar(y[k])-len)                    
          }
          x[i] <-  paste(y,collapse = ' ')
        }
      }
    }
    x    
  }
  
  ps_wordAt  <- function(x,index = 1){
    
    z <- NULL
    
    for(i in 1 : length(x))
    {
      
      #y <-unlist(strsplit(x[i], split = ' '))[1:2]
      if(is.na(x[i]))
      {
        z[i] <- x[i]
      }
      else
      {
        y <-unlist(strsplit(as.character(x[i]), split = ' '))
        y <- y[!(y %in% stopwords(kind='en'))]
        y <- y[index]
        z[i] <- y  
      }
      
    }
    z      
  }
  
  
  ps_firstTwo  <- function(x){
    
    z <- NULL
    
      y <-unlist(strsplit(x, split = ' '))
      y <- y[!(y %in% stopwords(kind='en'))]
      y <- y[1:2]
      
      if(is.na(y[2])){
        z <- y[1]
      }
      else{
        z <- paste(y[1],y[2], sep = " ")
      }
    
      z  
    }
          
  
  
  #load stopword lists
  stword <- stopwords('en')
  stword <- stword[nchar(stword) < 4]
  restaurants <- c("alehouse","bakery","bar","barbeque","bbq","bist","bistro","brasserie","brickoven","buffet"," caf","cafeteria","canteen","coffee","coffee bar","coffee shop","cuisine","fast food","gril","grill","grille","inn","kitchen","lounge","lunchroom","pizzeria","pub","restaurant","ristorante","roadhouse","rotisserie","saloon","snack bar","sushi","tapas","taproom","tavern","taverna","tearoom","wine bar")
  
  ven <- str_replace_all(ven,"[^[:graph:]]", " ") 
  ven <- tolower(ven)
  
  #strip prefixes
  ven <- gsub('^#[0-9]+','',ven) 
  ven <- gsub('act\\*',' ',ven)
  ven <- gsub('2co\\*',' ',ven)
  ven <- gsub('2co.com\\*',' ',ven)
  ven <- gsub('[0-9]avan\\*',' ',ven)
  ven <- gsub('37s\\*',' ',ven)
  ven <- gsub('ady\\*',' ',ven)
  ven <- gsub('ali\\*',' ',ven)
  ven <- gsub('bb \\*',' ',ven)
  ven <- gsub('amz\\*',' ',ven)
  ven <- gsub('avangate\\*',' ',ven)
  ven <- gsub('dri\\*',' ',ven)
  ven <- gsub('dmi\\*',' ',ven)
  ven <- gsub('de\\*',' ',ven)
  ven <- gsub('d j\\*',' ',ven)
  ven <- gsub('cvent\\*',' ',ven)
  ven <- gsub('api\\*',' ',ven)
  ven <- gsub('asi\\*',' ',ven)
  ven <- gsub('awl\\*',' ',ven)
  ven <- gsub('bal\\*',' ',ven)
  ven <- gsub('bar\\*',' ',ven)
  ven <- gsub('blk\\*',' ',ven)
  ven <- gsub('blu\\*',' ',ven)
  ven <- gsub('bls\\*',' ',ven)
  ven <- gsub('cbi\\*',' ',ven)
  ven <- gsub('eb \\*',' ',ven)
  ven <- gsub('yelpinc\\*',' ',ven)
  ven <- gsub('wpy\\*',' ',ven)
  ven <- gsub('wme\\*',' ',ven)
  ven <- gsub('travres\\*',' ',ven)
  ven <- gsub('tms\\*',' ',ven)
  ven <- gsub('tme\\*',' ',ven)
  ven <- gsub('tfs\\*',' ',ven)
  ven <- gsub('sq \\*',' ',ven)
  ven <- gsub('skr\\*',' ',ven)
  ven <- gsub('pp\\*',' ',ven)
  ven <- gsub('pmi\\*',' ',ven)
  ven <- gsub('pmt\\*',' ',ven)
  ven <- gsub('pln\\*',' ',ven)
  ven <- gsub('pli\\*',' ',ven)
  ven <- gsub('paypal\\*',' ',ven)
  ven <- gsub('pay\\*',' ',ven)
  ven <- gsub('oww\\*',' ',ven)
  ven <- gsub('opc\\*',' ',ven)
  ven <- gsub('int\\*',' ',ven)
  ven <- gsub('intuit \\*',' ',ven)
  ven <- gsub('in \\*',' ',ven)
  ven <- gsub('google\\*',' ',ven)
  ven <- gsub('ftd\\*',' ',ven)
  ven <- gsub('fs \\*',' ',ven)
  ven <- gsub('^la ','',ven)
  ven <- gsub('^le ','',ven)
  ven <- gsub('^las ','',ven)
  ven <- gsub('facebk','facebook',ven)
  ven <- gsub('\\*',' * ',ven)
  ven <- removePunctuation(ven,  preserve_intra_word_dashes = TRUE)#RemovePunctuation
  ven <- gsub("^[0][0-9]+",'',ven) #remove leading numbers that start with zero
  ven <- gsub("-",'',ven)
  ven <- gsub("^\\s+|\\s+$", "", ven)#Trim
  ven <- gsub('( st | st$|street|circle| cr | cr$|cir | rd | rd$|road|parking|garage|com$|inc$|llc$|ave$)',' ',ven) #Remove Words
  ven <- gsub("^\\s+|\\s+$", "", ven)#Trim
  ven <- stripWhitespace(ven)  #RemoveDubleSpaces
  ven <- gsub("( nw | nw$)",' ',ven) #remove ' nw '
  ven <- gsub('pennsylvania','penn',ven) #replace
  ven <- gsub('(1 800|1 800 |1800)','1800 ',ven) #replace
  ven <- gsub("(com |com$)",' ',ven)  #Remove com 
  ven <- stripWhitespace(ven)  #RemoveDubleSpaces
  ven <- gsub('[0-9]+$','',ven) #RemoveNumTail
  ven <- gsub("^\\s+|\\s+$", "", ven)#Trim
  ven <- gsub("^([a-zA-Z] )+", "",ven)# Remove lonely single characters at the head
  ven <- gsub("^\\s+|\\s+$", "", ven)#Trim
  ven <- gsub(" [a-zA-Z]$", "",ven)# Remove lonely single characters at the tail
  ven <- gsub("^\\s+|\\s+$", "", ven)#Trim
  ven <- gsub("www", "", ven)
  ven <- gsub('^ups ', 'ups', ven)
  
  ven <- ps_removeWords(ven,stword)
  ven <- ps_removeWords(ven,restaurants)
# ven <- ps_trimEnd(ven, pattern = '(s|S)$', len = 1)#remove s endings 
  ven <- stripWhitespace(ven)
  ven <- ps_firstTwo(ven)
  ven <- toupper(ven)
  
  if(code == TRUE)
  {
  
    VendorFirstTwo5 <- substr(ven,1,5)
    VendorFirstTwo5 <- gsub('[0-9]+$','',VendorFirstTwo5) #RemoveNumTail
    VendorFirstTwo5 <- gsub("^\\s+|\\s+$", "", VendorFirstTwo5)#Trim
    VendorFirstTwo5 <- gsub(" [a-zA-Z]$", "",VendorFirstTwo5)# Remove lonely single characters at the tail
      
    ven = VendorFirstTwo5
  
  }
  
  return(ven)
}
