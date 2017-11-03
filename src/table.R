tabler <- function(data, vars, crossvar) {
  # table.r returns a crosstabulation of a bunch of variables cross tabulated with the crossvar.  Categorical
  # vars return percentages.  Doubles return means.
   print(head(data))
}
  
  
  
  
  
  do.call(rbind, sapply(data[vars], function(x){
    if(is.numeric(x)==TRUE){
      means<-aggregate(x ~ crossvar, FUN=mean, na.action=na.omit)
      print(means)[,2]
    }
    else{
      prop.table(table(x, df$CLUSTER),2)
    }
  }))
  kable(list, caption = "Distribution of Adjunct Types by Institutional Characteristics")
  
  
  
}