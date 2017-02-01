#' Pooling nonsignificant interactions to Residuals
#' 
#' Pooling nonsignificant interactions to Residuals
#' @usage PooledANOVA(SS.table,del.ID)
#' @param SS.table result from EMSanova  
#' @param del.ID id's to combine sum of squares. Use rownames of SS.table
#' @export
#' @examples
#' data(film)
#' anova.result<-EMSanova(thickness~Gate*Operator*Day,data=film,
#'                         type=c("F","R","R"))
#' anova.result 
#' del.ID<-c("Gate:Day","Residuals")
#' PooledANOVA(anova.result,del.ID)

PooledANOVA<-function(SS.table,del.ID){
  temp.SS<-SS.table[,c("Df","SS")]
  temp.EMS<-as.character(SS.table$EMS)
  Model.level<-SS.table$Model.Level
  temp.ID<-del.ID[del.ID!="Residuals"]
  temp.ID<-unlist(lapply(temp.ID,function(x) which(rownames(temp.SS)==x)))
  temp.EMS<-as.character(temp.EMS)
  temp.SS[nrow(temp.SS),]<-apply(temp.SS[del.ID,],2,function(x) sum(x,na.rm=TRUE))
  temp.SS<-temp.SS[-temp.ID,]
  Model.level<-Model.level[-temp.ID]
  
  temp.SS[,3]<-temp.SS[,2]/temp.SS[,1]
  temp.split.EMS<-lapply(temp.EMS,function(x) {
    temp1<-strsplit(x,"[+]")[[1]]
    for(i in 1:length(temp.ID)){
      t.id<-grep(del.ID[i],temp1)
      if(length(t.id)!=0)
        temp1<-temp1[-t.id]
    }
    return(temp1)})
  
  temp.split.EMS<-temp.split.EMS[-temp.ID]    
  EMS.t<-lapply(temp.split.EMS,function(x) paste(x,sep="",collapse="+"))

  F.value<-NULL
  P.value<-NULL
  Signif<-NULL
  for(i in 1:nrow(temp.SS)){
    n.SE<-length(temp.split.EMS[[i]])
    SS.temp<-paste(temp.split.EMS[[i]][-n.SE],collapse="+")
    test.EMS<-temp.split.EMS[[i]]
    if(sum(temp.EMS==SS.temp)!=0){
      F.temp<-temp.SS[i,3]/temp.SS[which(EMS.t==SS.temp),3]
      pValue.temp<- 1-stats::pf(F.temp,temp.SS[i,1],
                         temp.SS[which(EMS.t==SS.temp),1])
    } else if(i!=nrow(temp.SS)&length(test.EMS)!=1){
      Appr.result<-ApproxF(data.frame(temp.SS,EMS=unlist(EMS.t)),rownames(temp.SS)[i])
      F.temp<-Appr.result$Appr.F
      pValue.temp<-Appr.result$Appr.Pvalue
    } else{
      F.temp<-NA
      pValue.temp<-NA
    }
 
    if(!is.na(pValue.temp)){
      if(pValue.temp<=0.001){
        Signif.temp <- "***"
      }else if(pValue.temp<=0.01){
        Signif.temp <- "**"
      }else if(pValue.temp<=0.05){
        Signif.temp <- "*"
      }else if(pValue.temp<=0.1){
        Signif.temp <- "."
      }else{
        Signif.temp <- ""
      }
      pValue.temp <- ifelse(round(pValue.temp,4)<0.0001,
                            "<0.0001",round(pValue.temp,4))
      F.temp <- round(F.temp,4)
    }else{
      Signif.temp <- ""
      pValue.temp <- ""
      F.temp<-""
    }
    F.value<-c(F.value,F.temp)
    P.value<-c(P.value,pValue.temp)
    Signif<-c(Signif,Signif.temp)    
  }
  
  SS.table.t<-cbind(temp.SS[,1],
                    round(temp.SS[,2],4),
                    round(temp.SS[,3],4))
  colnames(SS.table.t)<-c("Df","SS","MS")
  EMS.t<-as.character(EMS.t)
  if(!is.null(Model.level)){
    tot.result<-data.frame(SS.table.t,Fvalue=F.value,Pvalue=P.value,
                           Sig=Signif,Model.Level=Model.level,EMS=matrix(EMS.t))    
  }else{
    tot.result<-data.frame(SS.table.t,Fvalue=F.value,Pvalue=P.value,
                           Sig=Signif,EMS=matrix(EMS.t))   
  }
  rownames(tot.result)<-rownames(temp.SS) 

  return(tot.result)
}

