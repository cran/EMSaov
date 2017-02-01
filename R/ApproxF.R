#' Calculate ANOVA with approximate F value
#' 
#' Calculate ANOVA with approximate F value
#' @usage ApproxF(SS.table,approx.name)
#' @param SS.table result from EMSanova  
#' @param approx.name rowname in SS.table to calculate approximate F value for the test. 
#' @export
#' @examples
#' data(film)
#' anova.result<-EMSanova(thickness~Gate*Operator*Day,data=film,
#'                         type=c("F","R","R"))
#' anova.result                         
#' ApproxF(SS.table=anova.result,approx.name="Gate") 
#' EMSanova(thickness~Gate+Operator+Day,data=film,
#'           type=c("F","R","R"),
#'           approximate=TRUE)                      
#'        
ApproxF<-function(SS.table,approx.name){
  approx.id<-NULL
  for(i in approx.name)
      approx.id<-c(approx.id,which(rownames(SS.table)==i))
  EMS<-as.character(SS.table$EMS)
  split.EMS<- lapply(EMS,function(x) strsplit(x,"[+]")[[1]])
  split.EMS.last<-lapply(split.EMS,function(x) return(x[length(x)]))  
  test.EMS<-split.EMS[[approx.id]]
  n.SE<-length(test.EMS)
  TEMP.EMS<-test.EMS[-n.SE]
  keep.id<-NULL
  keep.var<-NULL
  for(kk in 2:length(TEMP.EMS)){
    keep.id<-c(keep.id,which(split.EMS.last==TEMP.EMS[kk]))
    keep.var<-c(keep.var,TEMP.EMS[kk])  
  }
  keep.var<-keep.var[keep.id!=1]
  TEMP.EMS<-unlist(split.EMS[keep.id])
  TEMP.EMS<-TEMP.EMS[TEMP.EMS!="Error"]
  den.id<-names(table(TEMP.EMS))[table(TEMP.EMS)==1]
  ms.num<-SS.table[approx.id,3]
  ms.den<-0
  df.num<-SS.table[approx.id,3]^2/SS.table[approx.id,1]
  df.den<-0
  
  for(kk in 1:length(keep.var)){
    if(sum(keep.var[kk]==den.id)==1){
      id.i<-which(split.EMS.last==keep.var[kk])
      ms.den<-ms.den+SS.table[id.i,3]
      df.den<-df.den+SS.table[id.i,3]^2/SS.table[id.i,1]
    }else{  
      id.i<-which(split.EMS.last==keep.var[kk])
      ms.num<-ms.num+SS.table[id.i,3]
      df.num<-df.num+SS.table[id.i,3]^2/SS.table[id.i,1]    
    }
  }
  Appr.F<-ms.num/ms.den
  Appr.F.df1<-ms.num^2/df.num
  Appr.F.df2<-ms.den^2/df.den
  Appr.Pvalue<-1-stats::pf(Appr.F,Appr.F.df1,Appr.F.df2)
  return(list(Appr.F=Appr.F,df1=Appr.F.df1,df2=Appr.F.df2,Appr.Pvalue=Appr.Pvalue))
}  

