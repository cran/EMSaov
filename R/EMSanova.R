#' Calculate ANOVA table with EMS 
#' 
#' Calculate ANOVA table with EMS for various experimental design - factorial design, nested
#' design, mixed effect model, etc.
#' @usage EMSanova(formula,data,type=NULL,nested=NULL,
#'                  level=NULL,approximate=FALSE)
#' @param formula model formula         
#' @param data data frame for ANOVA
#' @param type the list of fixed/random for each factor. 
#'        "F" for the fixed effect, "R" for the random effect
#' @param nested the list of nested effect
#' @param level list of model level
#' @param approximate calculate approximated F for "TRUE"
#' @export
#' @examples
#' data(baseball)
#' anova.result<-EMSanova(velocity~Group+Subject+test,data=baseball,
#'                  type=c("F","R","F"),
#'                  nested=c(NA,"Group",NA),
#'                  level=c(1,1,2))
#' anova.result                  
EMSanova<-function(formula,data,type=NULL,nested=NULL,
                       level=NULL,approximate=FALSE){
  
  Call<-match.call()
  indx<-match(c("formula","data"),names(Call),nomatch=0L)
  if(indx[1]==0L) 
    stop("a 'formula' argument is required")
  temp<-Call[c(1L,indx)]
  temp[[1L]]<-quote(stats::model.frame)
  m<-eval.parent(temp)
  Terms<-attr(m,"terms")
  
  formula.t<-as.character(formula)
  Y.name<-formula.t[2]
  data.n<-strsplit(formula.t[3]," \\+ ")[[1]]
  if(data.n[1]=="."){
    var.list<-colnames(data)[colnames(data)!=Y.name]
  } else{
    temp1<-unlist(sapply(data.n,strsplit," "))
    var.list<-unique(temp1[temp1!=" " & temp1 !="*"& temp1!=""])
  } 
  ## adjust the order of X variable for multi-level model

  if(!is.null(level)){
    sort.id<-sort.list(level)
    nested<-nested[sort.id]
    level<-level[sort.id]
    var.list<-var.list[sort.id]
    type<-type[sort.id]
    #if(!is.null(n.table)) n.table[1:length(sort.id)]<-n.table[sort.id]
  }  
  if(!is.null(nested) &ifelse(length(nested)!=0,sum(!is.na(nested)),0)!=0){
    nested<-lapply(nested,function(x){ 
      xx<-strsplit(x,split="\\*")[[1]];
      temp<-NULL
      for(i in 1:length(xx))
        temp<-c(temp,which(var.list==xx[i]));
      if(length(temp)==0){
        return(NA)
      } else{
        return(temp)
      }})
  } else{
    nested<-as.list(rep(NA,length(var.list)))
  }   
  EMSflag<-FALSE
  n.table<-NULL
  for(i in 1:length(var.list)){
    temp<-table(data[,var.list[i]])
    if(sum(temp!=mean(temp))!=0)
      EMSflag<-TRUE
    n.table<-c(n.table,length(temp))
  }
  n.table<-c(n.table,mean(table(apply(data[,var.list,drop=FALSE],1,
                                      function(x) paste(x,collapse="")))))
  if(EMSflag){
    stop("EMSanova cannot handle the unbalanced design.")
  }  
  ## Change all X variables to factors
  
  data<-data[,c(var.list,Y.name)]
  for(i in var.list){
    data[,i]<-factor(data[,i])
  }
  
  ## design.M1
  
  n<-length(var.list)
  design.M1 <- NULL
  for(i in 1:n){
    design.M1<-rbind(design.M1,design.M1)
    temp1<-rep(c("",var.list[i]),each=2^(i-1))
    design.M1<-cbind(design.M1,temp1)
  }
  
  design.M1<-design.M1[-1,,drop=FALSE]
  
  ## Full model ANOVA
  
  model.F<-paste(Y.name,"~",paste(apply(design.M1,1,function(x) 
    paste(paste(x[x!=""],collapse="*"))),collapse="+"))
  model.id<-c(apply(design.M1,1,function(x) 
    paste(paste(x[x!=""],collapse=":"))),"Residuals")
  options(warn=-1)
  SS.table<-stats::anova(stats::lm(eval(model.F),
                                   data = data))[model.id,1:2]
  modelSS<-stats::anova(stats::lm(formula,
                                  data = data))[,1:2]
  options(warn=0)
  ## treat nested
  
  colnames(design.M1)<-var.list
  nest.id<-which(!is.na(nested))
  if(length(nest.id)>0){
    for(i in 1:length(nest.id)){
      for(j in 1:length(nested[[nest.id[i]]])){
        temp.list<-apply(design.M1[,1:n],1,function(x) 
          ifelse(sum(x==var.list[nest.id[i]])==0,
                 NA,var.list[nested[[nest.id[i]]][j]]))
        del.list<-which(apply(design.M1[,1:n],1,function(x) 
          sum(x==var.list[nest.id[i]])*
            sum(x==var.list[nested[[nest.id[i]]][j]]))==1)
        for(k in 1:length(del.list)){
          comb.id<-del.list[k]
          temp.k<-design.M1[comb.id,]
          temp.k<-temp.k[temp.k!="" & temp.k!=var.list[nested[[nest.id[i]]][j]]]
          temp.k<-paste(temp.k,collapse="")
          comb.id<-c(comb.id,which(apply(design.M1,1,
                                         function(x) paste(x,sep="",collapse=""))==temp.k))
          SS.temp<-apply(SS.table[comb.id,],2,sum)
          SS.table[comb.id[length(comb.id)],]<-SS.temp
        }
        design.M1<-cbind(design.M1,temp.list)
        colnames(design.M1)<-c(colnames(design.M1)[-ncol(design.M1)],"nested")
        design.M1<-design.M1[-del.list,]
        SS.table<-SS.table[-del.list,]
        
        ## nested-nested-...
        
        flag<-TRUE
        id.t<-nest.id[i]
        while(flag){
          n.id.t<-nested[[id.t]]
          temp.c<-unlist(nested[n.id.t])
          temp.c<-temp.c[!is.na(temp.c)]
          
          if(length(temp.c)==0){
            flag<-FALSE
          }else{
            for(l in 1:length(temp.c)){
              temp.j<-ncol(design.M1)        
              del.list<-which(apply(design.M1,1,function(x) 
                sum(x[1:n]==var.list[temp.c[l]])*!is.na(x[temp.j]))==1)
              design.M1<-design.M1[-del.list,]    
              SS.temp<-apply(SS.table[del.list,],2,sum)
              design.M1[which(design.M1[,temp.j] ==
                                var.list[nested[[nest.id[i]]][j]]),
                        n+temp.c[l]]<-var.list[temp.c[l]]
              sel.id<-which(design.M1[,temp.j] ==
                              var.list[nested[[nest.id[[i]]]][j]])
              SS.table[sel.id,] <-SS.table[sel.id,]+SS.temp
              SS.table<-SS.table[-del.list,]
            }  
          }
          id.t<-nested[[id.t]]
        }
      }
    } 
  }  
  
  ## EMS.table
  
  design.M1[is.na(design.M1)]<-""
  out<-apply(design.M1,1,function(x) 
    ifelse(paste(x[-(1:n)],collapse="")!="",
           paste(paste(x[1:n][x[1:n]!=""],collapse=":"),
                 "(",paste(x[-(1:n)][x[-(1:n)]!=""],collapse="*"),
                 ")",sep=""),
           paste(x[1:n][x[1:n]!=""],collapse=":")))
  rownames(SS.table)[-nrow(SS.table)]<-out
  EMS.table<-matrix(0,ncol=length(var.list)+1,nrow=length(out)+1)
  colnames(EMS.table)<-c(var.list,"Error")
  rownames(EMS.table)<-c(out,"Error")
  n.EMS<-nrow(EMS.table)
  p.EMS<-ncol(EMS.table)
  EMS.table[,p.EMS]<-n.table[p.EMS]
  EMS.table[n.EMS,]<-1
  temp<-design.M1[,1:length(var.list),drop=FALSE]
  temp.nest<-design.M1[,-c(1:length(var.list)),drop=FALSE]
  temp[temp==""]<-NA
  for(i in 1:ncol(temp)){
    if(sum(temp[,i]==var.list[i],na.rm=TRUE)!=0){
      id.t<-which(is.na(temp[,i]))
      EMS.table[id.t,i]<-n.table[i]
      if(type[i]=="R")  EMS.table[-c(id.t,n.EMS),i]<-1
    }else{
      sel.id<-which(!is.na(temp[,i]))
      EMS.table[sel.id,which(var.list==temp[sel.id,i][1])]<-1
    }   
    if(length(nest.id)>0){
      for(k in 1:ncol(temp.nest))
        EMS.table[which(temp.nest[,k]==var.list[i]),i]<-1     
    }
  }    
  
  ## EMS  
  
  temp.t<-design.M1[,1:length(var.list),drop=FALSE]
  EMS<-NULL
  n.E<-nrow(EMS.table)
  id.keep<-NULL 
  hid.flag<-NULL
  for(i in n.E:1){
    if(i!=n.E){
      sel.id<-temp.t[i,,drop=FALSE]
      if(length(nest.id)>0){
        tt<-temp.nest[i,]
        id.keep<-NULL
        for(l in 1:length(tt))
          id.keep<-c(id.keep,which(names(hid.flag)==tt[l]))
      }          
      hid.flag<-rep(TRUE,ncol(EMS.table))
      names(hid.flag)<-colnames(EMS.table)
      for(j in 1:length(var.list)){
        hid.flag[which(names(hid.flag)==sel.id[j])]<-FALSE
      }
      pick.id<-design.M1[i,]
      pick.id<-pick.id[pick.id!=""]
      temp<-apply(design.M1,1,function(x) {
        keep.t<-TRUE; 
        for(i in 1:length(pick.id)) 
          keep.t<-keep.t*(sum(x==pick.id[i])!=0)
        return(keep.t)})
      hid.flag[id.keep]<-TRUE
      temp.T<-apply(EMS.table[,hid.flag,drop=FALSE],1,prod)
      temp.T.1<-temp.T[temp.T!=0]
      temp.T.1[length(temp.T.1)]<-""
      name.temp.T<-names(temp.T.1)
      temp<-c(temp,1)[temp.T!=0]
      nn<-length(temp.T.1)    
      temp.T.1<-temp.T.1[nn:1] 
      name.temp.T<-name.temp.T[nn:1] 
      temp<-temp[nn:1] 
      temp.EMS<-paste(temp.T.1[temp==1],name.temp.T[temp==1],
                      sep="",collapse="+")
    }else{
      temp<-c(rep(0,ncol(EMS.table)-1),1)
      temp.EMS<-"Error"
    }
    EMS<-cbind(temp.EMS,EMS)
  }
  
  ## model level
  
  if(!is.null(level)){
    level.list<-sort(unique(level))
    n.L<-length(level.list)
    Model.level<-rep(level.list[n.L],nrow(SS.table)-1)    
    temp.flag<-rep(TRUE,length(Model.level))
    for(i in n.L:1){
      i.id<-which(level==i)
      for(k in i.id){
        Model.level[which((design.M1[,k]!="")*temp.flag==1)]<-i
        temp.flag[design.M1[,k]!=""]<-FALSE
      }    
    }
    Model.level<-c(Model.level,max(Model.level))
  }else{
    Model.level<-NULL
  }   
  n.t<-nrow(SS.table)
  flag.zero.MSE<-FALSE  
  
  if(SS.table[n.t,2]==0){
    SS.table[n.t,1:2]<-SS.table[n.t-1,1:2] 
    temp.name<-rownames(SS.table)[n.t]
    SS.table<-SS.table[-n.t,]
    rownames(SS.table)[n.t-1]<-temp.name
    t.EMS<-lapply(EMS,function(x) strsplit(x,"[+]")[[1]])
    del.list<-t.EMS[[n.t-1]][-1]
    for(i in 1:length(t.EMS)){
      keep.id<-NULL
      for(j in 1:length(del.list))
        keep.id<-c(keep.id,which(t.EMS[[i]]==del.list[j]))
      if(length(keep.id)!=0)
        t.EMS[[i]]<-t.EMS[[i]][-keep.id]
    }
    EMS<-unlist(lapply(t.EMS,function(x) paste(x,sep="",collapse="+")))
    EMS[n.t-1]<-EMS[n.t]
    EMS<-EMS[1:(n.t-1)]
    flag.zero.MSE<-TRUE
    Model.level<-Model.level[1:(n.t-1)]
  }
  ## Caldulate MS, F (approx.F), P-value, sig,
  
  SS.table[,3]<-SS.table[,2]/SS.table[,1]
  split.EMS<-lapply(EMS,function(x) strsplit(x,"[+]")[[1]])
  F.value<-NULL
  P.value<-NULL
  Signif<-NULL
  for(i in 1:nrow(SS.table)){
    n.SE<-length(split.EMS[[i]])
    SS.temp<-paste(split.EMS[[i]][-n.SE],collapse="+")
    if(sum(EMS==SS.temp)!=0){
      F.temp<-SS.table[i,3]/SS.table[which(EMS==SS.temp),3]
      pValue.temp<- 1-stats::pf(F.temp,SS.table[i,1],
                                SS.table[which(EMS==SS.temp),1])
    }else if(i!=nrow(SS.table) & approximate){
      test.EMS<-split.EMS[[i]]
      Appr.result<-ApproxF(SS.table=data.frame(SS.table,EMS=c(EMS)),
                           approx.name=rownames(SS.table)[i])
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
  
  SS.table.t<-cbind(SS.table[,1],SS.table[,2],SS.table[,3])
  colnames(SS.table.t)<-c("Df","SS","MS")
  if(!is.null(Model.level)){
    tot.result<-data.frame(SS.table.t,Fvalue=F.value,Pvalue=P.value,
                           Sig=Signif,Model.Level=Model.level,EMS=matrix(EMS))   
 
  }else{
    tot.result<-data.frame(SS.table.t,Fvalue=F.value,Pvalue=P.value,
                           Sig=Signif,EMS=matrix(EMS))   
  }
  rownames(tot.result)<-rownames(SS.table)  
  return(tot.result)
}

