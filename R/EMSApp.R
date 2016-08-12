#' Shiny App for the analysis of variance in various experimental designs
#' 
#' Shiny App for the analysis of variance in various experimental designs
#' @usage EMSaovApp()
#' @export
#' @examples
#' #EMSaovApp()

EMSaovApp<-function(){
  EMSaov.env<-new.env()
  EMS_app=shiny::shinyApp(
    ui=shiny::fluidPage(
      shiny::headerPanel("Shiny Application for ANOVA with EMS"),
      shiny::fileInput("outputfile",label="File input"),
      shiny::br(),
      shiny::wellPanel(  
        shiny::fluidRow(shiny::column(3,shiny::uiOutput("choose_Yvar"))),
        shiny::fluidRow(
          shiny::column(2,shiny::uiOutput("choose_Xvar")),
          shiny::column(2,shiny::uiOutput("choose_type")),
          shiny::column(2,shiny::uiOutput("choose_level")),
          shiny::column(2,shiny::uiOutput("choose_nested")),
          shiny::column(2,shiny::uiOutput("choose_split"))
        ),
        shiny::submitButton("Submit")
      ),
      shiny::hr(),
      shiny::mainPanel(
        shiny::tabsetPanel(
          shiny::tabPanel("EDA-main effect",shiny::plotOutput("EDA1")),
          shiny::tabPanel("EDA-interaction",shiny::plotOutput("EDA2")),     
          shiny::tabPanel("ANOVA table",shiny::tableOutput("result1"),
                          shiny::p(paste("Signif. codes : <0.0001 '***'",
                                 "0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1"))),
          shiny::tabPanel("ANOVA table with Approx. F",
                          shiny::tableOutput("result2"),
                          shiny::p(paste("Signif. codes : <0.0001 '***'",
                                  "0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1"))),
          shiny::tabPanel("Pooled ANOVA",
                          shiny::uiOutput("choose_ANOVA"),
                          shiny::submitButton("Submit1"),
                          shiny::tableOutput("result3"))
        )    
      )
    ), #end ui

    server<-function(input,output,session){ 
      EMSaov.env$outputData<-NULL
      EMSaov.env$outANOVA<-NULL

      Dataset<-shiny::reactive({
        if(is.null(input$outputfile)){
          return(data.frame())
        }
       EMSaov.env$outputData<-data.frame(do.call("read.csv",
                                        list(input$outputfile$datapath))) 
        return(EMSaov.env$outputData)
      })
     
      output$choose_Yvar<-shiny::renderUI({
        if(is.null(input$outputfile))
          return()
        if(identical(Dataset(),'')||identical(Dataset(),data.frame())) 
          return(NULL)      
       EMSaov.env$outputData<-Dataset()
       EMSaov.env$NUM<-dim(EMSaov.env$outputData)[2] #Num of all variable  ##
       EMSaov.env$Class<-sapply(apply(EMSaov.env$outputData,2,unique),length)  ##  
        EMSaov.env$Colnames<-colnames(EMSaov.env$outputData)##
        shiny::selectInput("Yvar",label="Y variable",c("",EMSaov.env$Colnames))    
      })
     
      output$choose_Xvar<-shiny::renderUI({
        if(is.null(input$outputfile))
          return()    
        if(is.null(input$outputfile)|is.null(EMSaov.env$outputData)){
          choice.temp<-c(" "," ") 
        }else{
          choice.temp<-c(EMSaov.env$Colnames)
        }      
        shiny::checkboxGroupInput("Xvar","X variable",choices=choice.temp) 
      })  

      output$choose_type<-shiny::renderUI({
        if(is.null(input$outputfile))
          return()    
        if(is.null(input$outputfile)|is.null(EMSaov.env$outputData)){
          choice.temp<-c(" "," ")
        }else{
          choice.temp<-c(EMSaov.env$Colnames)
        } 
        shiny::checkboxGroupInput("type","Random Effect",choices=choice.temp) 
      }) 

      makenumericButton<-function(n){
        if(n==1){
          shiny::numericInput(paste0("level",n),
                      label=paste0("[# of categories] ",EMSaov.env$Colnames[n]),
                      value=EMSaov.env$Class[n])
        }else{
          shiny::numericInput(paste0("level",n),label=EMSaov.env$Colnames[n],
                              value=EMSaov.env$Class[n])
        }
      }
      WidgetVector<-shiny::reactive({lapply(X=1:EMSaov.env$NUM,
                                            FUN=makenumericButton)})
      output$choose_level<-shiny::renderUI({
        if(is.null(input$outputfile)|is.null(EMSaov.env$outputData)){
          return()  
        }else{
          shiny::tagList(WidgetVector())
        }
      }) 
     
      makeselectButton<-function(n){
        if(n==1){
          shiny::selectInput(paste0("nested",n),
                     label=paste0("[nested]\n ",EMSaov.env$Colnames[n]),
                     c("None",EMSaov.env$Colnames))
        }else{
          shiny::selectInput(paste0("nested",n),
                             label=EMSaov.env$Colnames[n],
                             c("None",EMSaov.env$Colnames))
        }
      }
     
      WidgetVector2<-shiny::reactive({lapply(X=1:EMSaov.env$NUM,
                                             FUN=makeselectButton)})
      output$choose_nested<-shiny::renderUI({
        if(is.null(input$outputfile)| is.null(EMSaov.env$outputData)){
          return()  
        }else{
          shiny::tagList(WidgetVector2())
        }
      }) 
     
      makenumericButton2<-function(n){
        if(n==1){
          shiny::numericInput(paste0("split",n),
                      label=paste0("[model level] ",EMSaov.env$Colnames[n]),
                      value=1)
        }else{
          shiny::numericInput(paste0("split",n),
                              label=EMSaov.env$Colnames[n],value=1)
        }
      }
     
      WidgetVector3<-shiny::reactive({lapply(X=1:EMSaov.env$NUM,
                                             FUN=makenumericButton2)})
      output$choose_split<-shiny::renderUI({
        if(is.null(input$outputfile)|is.null(EMSaov.env$outputData)){
          return()  
        }else{
          shiny::tagList(WidgetVector3())
        }
      }) 
     
      output$EDA1<-shiny::renderPlot({
        if(is.null(input$outputfile)|is.null(EMSaov.env$outputData)| 
           is.null(input$Xvar)|is.null(input$Yvar)){
          return()
        }else{
          X<-EMSaov.env$outputData[,input$Xvar]
          Y<-EMSaov.env$outputData[,input$Yvar]
          p<-length(input$Xvar)
          r<-ceiling(sqrt(p))
          graphics::par(mfrow=c(1,p))
          for(i in 1:p){
            graphics::plot(Y~factor(X[,i]),xlab=input$Xvar[i],ylab=input$Yvar)
            graphics::points(1:length(table(X[,i])),tapply(Y,X[,i],mean),
                             col=2,pch=16,cex=1.5)
          }  
        }
      })
     
      output$EDA2<-shiny::renderPlot({
        if(is.null(input$outputfile)|is.null(EMSaov.env$outputData)| 
           is.null(input$Xvar)|is.null(input$Yvar)){
          return()
        }else{
          X<-EMSaov.env$outputData[,input$Xvar]
          Y<-EMSaov.env$outputData[,input$Yvar]
          p<-length(input$Xvar)
          r<-ceiling(sqrt(p*(p-1)/2))
          graphics::par(mfrow=c(r,r))
          for(i in 1:(p-1)){
            for(j in (i+1):p){
              temp.group<-as.numeric(X[,j])
              r<-length(table(X[,i]))
              graphics::matplot(c(-0.5,r),range(Y),type="n",
                     xlab=input$Xvar[i],ylab=input$Yvar,
                     main=paste(input$Xvar[i],"*", input$Xvar[j]))
              temp.table<-names(table(X[,j]))
              for(k in 1:length(temp.table)){
                graphics::lines(1:length(table(X[temp.group==k,i])),
                  tapply(Y[temp.group==k],X[temp.group==k,i],mean),lty=k,col=k)
              }
              graphics::legend(-0.5,max(Y),temp.table,lty=1:r,col=1:r,
                               title=input$Xvar[j])
            }
          }
        }
      })
     
      output$result1<-shiny::renderTable({
        if(is.null(input$outputfile)| is.null(EMSaov.env$outputData) | 
           is.null(input$Xvar)| is.null(input$Yvar)){
          return()
        }else{
          X<-EMSaov.env$outputData[,input$Xvar]
          Y<-EMSaov.env$outputData[,input$Yvar]
          for(i in 1:EMSaov.env$NUM){
           EMSaov.env$Class[i]<-input[[paste0("level",i)]]
          }  #inputEMSaov.env$Class   
          level<-EMSaov.env$Class[c(input$Xvar)]
          level<-c(level,mean(table(X)))

          Type<-matrix("F",nrow=length(input$Xvar))
          rownames(Type)<-input$Xvar
          Type[input$type,]<-"R"
          type<-c(Type)
         
          nested<-NULL
          for(i in 1:EMSaov.env$NUM){
            nest<-input[[paste0("nested",i)]]
            if(is.null(nest)){
              nested[i]<-""
            }else{
              nested[i]<-nest
            }
          }
          names(nested)<-EMSaov.env$Colnames
          nested<-nested[input$Xvar]
          n<-length(input$Xvar)
         
         #split
          split<-NULL
          for(i in 1:EMSaov.env$NUM)
            split[i]<-input[[paste0("split",i)]]
          names(split)<-EMSaov.env$Colnames
         
          split<-split[c(input$Xvar)]
          split<-split[!is.na(split)]
          var.list<-input$Xvar      
          nest.temp<-rep(NA,length(nested))
          for(i in 1:length(nested))
            nest.temp[i]<-ifelse(nested[i]=="",NA,which(var.list==nested[i]))
          nested<-nest.temp
          if(sum(split==1)==length(split)) 
            split<-NULL      
          data.tot<-EMSaov.env$outputData[,c(input$Xvar,input$Yvar)]
          out<- EMS.anova(data.tot=data.tot,
                                 Y.name=input$Yvar,
                                 var.list=input$Xvar,
                                 FixRan.list=type,                        
                                 nested.list=input$Xvar[nested],
                                 model.level=split)
        }      
      })
     
      output$result2<-shiny::renderTable({
        if(is.null(input$outputfile)|is.null(EMSaov.env$outputData)| 
           is.null(input$Xvar)| is.null(input$Yvar)){
          return()
        }else{
          X<-EMSaov.env$outputData[,input$Xvar]
          Y<-EMSaov.env$outputData[,input$Yvar]
         
          for(i in 1:EMSaov.env$NUM){
           EMSaov.env$Class[i]<-input[[paste0("level",i)]]
          }  #inputEMSaov.env$Class     
         
          level<-EMSaov.env$Class[c(input$Xvar)]
          level<-c(level,mean(table(X)))
          Type<-matrix("F",nrow=length(input$Xvar))
          rownames(Type)<-input$Xvar
          Type[input$type,]<-"R"
          type<-c(Type)
         
         #nested   
          nested<-NULL
          for(i in 1:EMSaov.env$NUM){
            nest<-input[[paste0("nested",i)]]
            if(is.null(nest)){
              nested[i]<-""
            }else{
              nested[i]<-nest
            }
          }
          names(nested)<-EMSaov.env$Colnames
          nested<-nested[input$Xvar]
          n<-length(input$Xvar)
          split<-NULL
          for(i in 1:EMSaov.env$NUM)
            split[i]<-input[[paste0("split",i)]]
          names(split)<-EMSaov.env$Colnames
          split<-split[c(input$Xvar)]
          split<-split[!is.na(split)]
          var.list<-input$Xvar      
          nest.temp<-rep(NA,length(nested))
          for(i in 1:length(nested))
            nest.temp[i]<-ifelse(nested[i]=="None",
                                 NA,which(var.list==nested[i]))
          nested<-nest.temp
          if(sum(split==1)==length(split)) 
            split<-NULL      
          data.tot<-EMSaov.env$outputData[,c(input$Xvar,input$Yvar)]
          out<- EMS.anova(data.tot=data.tot,
                                 Y.name=input$Yvar,
                                 var.list=input$Xvar,
                                 FixRan.list=type,                        
                                 nested.list=input$Xvar[nested],
                                 model.level=split,
                                 approx.flag=TRUE)
          EMSaov.env$outANOVA<-out
        }      
      })
     
      output$choose_ANOVA <-  shiny::renderUI({
        if(is.null(input$outputfile))
          return()
        if(identical(Dataset(),'')||identical(Dataset(),data.frame())||
           is.null(EMSaov.env$outANOVA)) 
          return(NULL)  
        Rnames<-rownames(EMSaov.env$outANOVA)
        shiny::checkboxGroupInput("ANOVA","Combine ANOVA table",choices=Rnames)  
      })
     
      output$result3<-shiny::renderTable({
        if(is.null(input$outputfile)|is.null(EMSaov.env$outputData)| 
           is.null(input$Xvar)|is.null(input$Yvar)|is.null(input$ANOVA)){
          return()
        }else{
          sel.id<-NULL
          temp.input<-unique(c(input$ANOVA,"Residuals"))
          for(i in temp.input)
            sel.id<-c(sel.id,which(rownames(EMSaov.env$outANOVA)==i))
          if(length(sel.id)>1){
            temp.SS<-EMSaov.env$outANOVA[,1:2]
            temp.SS$Df<-as.numeric(as.character(temp.SS$Df))
            temp.SS$SS<-as.numeric(as.character(temp.SS$SS))
            Residuals<-apply(temp.SS[sel.id,],2,sum)
            temp.SS<-rbind(temp.SS[-sel.id,],Residuals)
            rownames(temp.SS)[nrow(temp.SS)]<-"Residuals"
            temp.EMS<-c(as.character(EMSaov.env$outANOVA$EMS)[-sel.id],
               as.character(EMSaov.env$outANOVA$EMS)[nrow(EMSaov.env$outANOVA)])
            del.ID<-temp.input
            Model.level<-EMSaov.env$outANOVA$Model.Level
            Model.level<-c(Model.level[-sel.id],
                           Model.level[length(Model.level)])
            out<- PooledANOVA(EMSaov.env$outANOVA,del.ID)
          }else{
            out<-EMSaov.env$outANOVA 
          }
        }      
      })
    }#end server
  )#end App
  shiny::runApp(EMS_app,launch.browser=TRUE)
}
 
 
 

 