#' Calculate ANOVA table with EMS 
#' 
#' Calculate ANOVA table with EMS for various experimental design - factorial design, nested
#' design, mixed effect model, etc.
#' @usage EMS.anova(data.tot,Y.name,var.list,FixRan.list,nested.list=NULL,
#'         model.level=NULL,n.table=NULL,approx.flag=FALSE,...)
#' @param data.tot data frame for ANOVA
#' @param Y.name the name of dependent variable
#' @param var.list the names of factors
#' @param FixRan.list the list of fixed/random for each factor. 
#'        "F" for the fixed effect, "R" for the random effect
#' @param nested.list the list of nested effect
#' @param model.level list of model level
#' @param n.table numbers of levels in each factor
#' @param approx.flag calculate approximated F for "TRUE"
#' @param ... arguments to be passed to methods
#' @export
#' @examples
#' test <- gl(2, 21, 42, labels = c("Ctl","Trt"))
#' Group <- rep(gl(3, 7, 21, labels = c("I","II","III")),2)
#' Subject <- rep(1:7,6)
#' Y<-c(26.25,24.33,22.52,29.33,28.9,25.13,29.33,
#' 27.47,25.19,23.53,24.57,26.88,27.86,28.09,
#' 22.27,21.55,23.31,30.03,28.17,28.09,27.55,
#'   29.5,27.62,25.71,31.55,31.35,29.07,31.15,
#'   28.74,26.11,25.45,25.58,27.7,28.82,28.99,
#'   22.52,21.79,23.53,30.21,28.65,28.33,27.86)
#' tot.data<-data.frame(Y=Y,test=test,Group=Group,Subject=Subject)
#' anova.result<-EMS.anova(data.tot=tot.data,
#'                         Y.name="Y",
#'                  var.list=c("Group","Subject","test"),
#'                  FixRan.list=c("F","R","F"),
#'                  nested.list=c(NA,"Group",NA),
#'                  model.level=c(1,1,2))
#' anova.result                  
EMS.anova<-function(data.tot,Y.name,var.list,FixRan.list,nested.list=NULL,
                      model.level=NULL,n.table=NULL,approx.flag=FALSE,...){
  
  ## adjust the order of X variable for multi-level model
 
  if(!is.null(nested.list)){
    if(sum(!is.na(nested.list))!=0){
      nested.list<-unlist(lapply(nested.list,function(x){ 
                  temp<-which(var.list==x);ifelse(length(temp)==0,NA,temp)}))
    }  
  }
  if(!is.null(model.level)){
    sort.id<-sort.list(model.level)
    nested.list<-var.list[nested.list[sort.id]]
    model.level<-model.level[sort.id]
    var.list<-var.list[sort.id]
    nested.list<-unlist(lapply(nested.list,function(x){ 
                   temp<-which(var.list==x);ifelse(length(temp)==0,NA,temp)}))
    FixRan.list<-FixRan.list[sort.id]
    n.table[1:length(sort.id)]<-n.table[sort.id]
  }  
  if(is.null(nested.list)){
     nested.list<-rep(NA,length(var.list))
  }   
  if(is.null(n.table)){
    for(i in 1:length(var.list)){
      n.table<-c(n.table,length(table(data.tot[,var.list[i]])))
    }
    n.table<-c(n.table,mean(table(apply(data.tot[,var.list],1,
                                function(x) paste(x,collapse="")))))
  }
  
  ## Change all X variables to factors
  
  data.tot<-data.tot[,c(var.list,Y.name)]
  for(i in var.list){
    data.tot[,i]<-factor(data.tot[,i])
  }
  
  ## design.M1
  
  n<-length(var.list)
  design.M1 <- NULL
  for(i in 1:n){
    design.M1<-rbind(design.M1,design.M1)
    temp1<-rep(c("",var.list[i]),each=2^(i-1))
    design.M1<-cbind(design.M1,temp1)
  }
  
  design.M1<-design.M1[-1,]
  
  ## Full model ANOVA
  
  model.F<-paste(Y.name,"~",paste(apply(design.M1,1,function(x) 
                  paste(paste(x[x!=""],collapse="*"))),collapse="+"))
  model.id<-c(apply(design.M1,1,function(x) 
      paste(paste(x[x!=""],collapse=":"))),"Residuals")
  options(warn=-1)
  SS.table<-stats::anova(stats::lm(eval(model.F),
                                   data = data.tot))[model.id,1:2]
  options(warn=0)
  ## treat nested
  
  colnames(design.M1)<-var.list
  nest.id<-which(!is.na(nested.list))
  
  if(length(nest.id)>0){
    for(i in 1:length(nest.id)){
      temp.list<-var.list[apply(design.M1[,1:n],1,function(x) 
        ifelse(sum(x==var.list[nest.id[i]])==0,
               NA,nested.list[nest.id[i]]))]
      del.list<-which(apply(design.M1[,1:n],1,function(x) 
        sum(x==var.list[nest.id[i]])*
          sum(x==var.list[nested.list[nest.id[i]]]))==1)
      for(k in 1:length(del.list)){
        comb.id<-del.list[k]
        temp.k<-design.M1[comb.id,]
        temp.k<-temp.k[temp.k!="" & temp.k!=var.list[nested.list[nest.id[i]]]]
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
        temp.c<-nested.list[nested.list[id.t]]
        if(is.na(temp.c)){
          flag<-FALSE
        }else{
          del.list<-which(apply(design.M1,1,function(x) 
            sum(x[1:n]==var.list[temp.c])*!is.na(x[n+i]))==1)
          design.M1<-design.M1[-del.list,]    
          SS.temp<-apply(SS.table[del.list,],2,sum)
          design.M1[which(design.M1[,n+i] ==
                            var.list[nested.list[nest.id[i]]]),
                    n+temp.c]<-var.list[temp.c]
          sel.id<-which(design.M1[,n+i] ==
                          var.list[nested.list[nest.id[i]]])
          SS.table[sel.id,] <-SS.table[sel.id,]+SS.temp
          SS.table<-SS.table[-del.list,]
        }
        id.t<-nested.list[id.t]
      }
    } 
  }  
  
  ## EMS.table
  
  design.M1[is.na(design.M1)]<-""
  out<-apply(design.M1,1,function(x) 
    ifelse(paste(x[-(1:n)],collapse="")!="",
           paste(paste(x[1:n][x[1:n]!=""],collapse=":"),
                 "(",paste(x[-(1:n)][x[-(1:n)]!=""],collapse=","),
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
  temp<-design.M1[,1:length(var.list)]
  temp.nest<-design.M1[,-c(1:length(var.list)),drop=FALSE]
  temp[temp==""]<-NA
  for(i in 1:ncol(temp)){
    if(sum(temp[,i]==var.list[i],na.rm=TRUE)!=0){
      id.t<-which(is.na(temp[,i]))
      EMS.table[id.t,i]<-n.table[i]
      if(FixRan.list[i]=="R")  EMS.table[-c(id.t,n.EMS),i]<-1
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
  
  temp.t<-design.M1[,1:length(var.list)]
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
  
  if(!is.null(model.level)){
    level.list<-sort(unique(model.level))
    n.L<-length(level.list)
    Model.level<-rep(level.list[n.L],nrow(SS.table)-1)    
    temp.flag<-rep(TRUE,length(Model.level))
    for(i in n.L:1){
      i.id<-which(model.level==i)
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
    }else if(i!=nrow(SS.table) & approx.flag){
      test.EMS<-split.EMS[[i]]
      Appr.result<-Approx.F(SS.table=data.frame(SS.table,EMS=c(EMS)),approx.id=i)
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
  
  SS.table.t<-cbind(SS.table[,1],
                     round(SS.table[,2],4),
                     round(SS.table[,3],4))
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

