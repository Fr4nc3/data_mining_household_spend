######################################################
# Francia F. Riesco
# Housing  EDA
######################################################

######################################################
# early version of my code I don't want to lose it 
######################################################


column_review_null <- c()
column_review_empty <- c()
for (i in colnames(joinData)){
  table_null <-table(is.na(joinData[[i]]))
  if(!is.na(table_null[2]) ){
    column_review_null <- c(column_review_null,i)
  }
}
for (i in colnames(joinData)){
  table_empty <-table(joinData[[i]] == "")
  if(!is.na(table_empty[2])){
    column_review_empty <- c(column_review_empty,i)
  }
}

for (i in column_review_null){
  joinData[[i]][is.na(joinData[[i]])] <- mean(joinData[[i]], na.rm = TRUE)
}


column_review_empty_drop <- c()
column_review_empty_fix <- c()
for (i in column_review_empty){
  empty_values_length <- length(joinData[[i]][joinData[[i]]==""])
  no_empty_values_length <- length(joinData[[i]][joinData[[i]]!=""])
  if(empty_values_length>5000){
    print(i)
    #print(summary(joinData[[i]]))
    #print(head(joinData[[i]][joinData[[i]]!=""  ])) # verified trendies values 
    print(cat("empties ", empty_values_length))
    print(cat("no emptines", no_empty_values_length))
    column_review_empty_drop <- c(column_review_empty_drop,i)
  }else{
    column_review_empty_fix <- c(column_review_empty_fix,i)
  }
  
}
print( column_review_empty_fix)
print( column_review_empty_drop)