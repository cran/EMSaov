#' Calculate ANOVA with approximate F value
#' 
#' Calculate ANOVA with approximate F value
#' @usage Approx.F(SS.table,approx.id,...)
#' @param SS.table result from EMS.anova  
#' @param approx.id id's to calculate approximate F value for the test. Use rownames of SS.table
#' @param ... arguments to be passed to methods
#' @export
#' @examples
#' G<-rep(rep(1:3,each=2),6)
#' O<-rep(rep(c("A","B","C"),each=6),2)
#' D<-rep(1:2,each=18)
#' Y<-c(0.38,0.4,0.63,0.59,0.76,0.78,0.39,0.41,0.72,0.7,
#'      0.95,0.96,0.45,0.4,0.78,0.79,1.03,1.06,0.4,0.4,
#'      0.68,0.66,0.86,0.82,0.39,0.43,0.77,0.76,0.86,0.85,
#'      0.41,0.4,0.85,0.84,1.01,0.98)
#' tot.data<-data.frame(Y=Y,G=G,O=O,D=D)
#' anova.result<-EMS.anova(data.tot=tot.data,
#'                         Y.name="Y",
#'                         var.list=c("G","O","D"),
#'                         FixRan.list=c("F","R","R"))
#' anova.result                         
#' Approx.F(SS.table=anova.result,approx.id=1) 
#' EMS.anova(data.tot=tot.data,
#'           Y.name="Y",
#'           var.list=c("G","O","D"),
#'           FixRan.list=c("F","R","R"),
#'           approx.flag=TRUE)                      
#'        
Approx.F<-function(SS.table,approx.id,...){
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
  return(list(Appr.F=Appr.F,Appr.Pvalue=Appr.Pvalue))
}  

