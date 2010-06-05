matchfun2 <- function(string1,adjust1,string2,adjust2,cols=NA,matchcol1=1,matchcol2=1,
      		objmatch=rawrep,objsubset=rawrep,substr1=TRUE,substr2=TRUE)
{
  # return a subset of values from the report file (or other file)
  # subset is defined by character strings at the start and end, with integer
  # adjustments of the number of lines to above/below the two strings
  line1 <- match(string1,if(substr1){substring(objmatch[,matchcol1],1,nchar(string1))}else{objmatch[,matchcol1]})
  line2 <- match(string2,if(substr2){substring(objmatch[,matchcol2],1,nchar(string2))}else{objmatch[,matchcol2]})
  if(!is.na(cols[1])){ out <- objsubset[(line1+adjust1):(line2+adjust2),cols]
      }else{		 out <- objsubset[(line1+adjust1):(line2+adjust2), ]}
  return(out)
}
