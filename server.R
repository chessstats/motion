
library(shiny)
library(plotly)
library(DT)

dt<-read.table("ratinghist.txt")
lists<-sprintf("%04d",dt$date)
load("players.dat")
names(players)<-paste("X",as.character(names(players)),sep="")
fideids<-names(players)
playernames<-as.character(players)
#cat("ids",fideids[1:5],"names",playernames[1:5])
dt$year=2000+dt$year/100

chartsize<<-800
startyear<<-2001

shinyServer(function(input, output, session){
  
  drawchart<- function(){
    output$trendPlot <- renderPlotly({
      
      selp<-input$control
      if(length(selp)==0) {
        selp=c("X1503014","X2016192")
      }
      
      starti<-1
      for(j in 1:nrow(dt)){
        if(dt$year[j]>=startyear){
          starti<-j
          break
        }
      }
      
      dteff<-dt[starti:nrow(dt),]
      
      command<-paste("p<-plot_ly(dteff,type='line',x=year,y=",selp[1],",name='",players[[selp[1]]],"')",sep="")
      #print(command)
      eval(parse(text=command))
      
      if(length(selp)>1) for(j in 2:length(selp)) {
        command=paste("p<-add_trace(p,type='line',x=year,y=",selp[j],",name='",players[[selp[j]]],"')",sep="")
        #print(command)
        eval(parse(text=command))
      }
      
      p<-layout(p,height=round(chartsize/1.5),width=chartsize,xaxis=list(title=''),yaxis=list(title=''),margins=list(pad=10))
      
      p
    })
  }
  
  observeEvent(input$rplayers,{
    #print("rplayers")
    session$sendCustomMessage(type="setPlayers",message=list(fideids,playernames,lists))
  })
  
  observeEvent(input$rlist,{
    #print("rlist")
    i<-input$rlist+1
    date<-dt$date[i]
    dt2<-dt[,2:length(fideids)]
    dt2<-dt2[,order(-dt2[i,])]
    playersordered=players[colnames(dt2)]
    ranklistnames=as.character(playersordered)
    ranklistrtgs=as.character(dt2[i,])
    ranklist<-data.frame(ranklistnames,ranklistrtgs)
    #print("render")
    dto<-datatable(ranklist)
    output$dataTable<- renderDataTable(dto)
  })
  
  observeEvent(input$reqchart,{
    cparams<-input$reqchart
    setchartsize<-cparams[1]
    setstartyear<-cparams[2]
    if(setchartsize>0){
      chartsize<<-setchartsize
    }
    if(setstartyear>0){
      startyear<<-setstartyear
    }
    drawchart()
  })
  
  observeEvent(input$control,{
    drawchart()
    if(FALSE){
    tablestr="<table id='mytable' class='display'><thead><tr><th>head</th></tr></thead><tbody><tr><td>body</td></tr></tbody>"
    session$sendCustomMessage(type="setTable",message=list("tablecont",tablestr,"mytable"))
    plotly='[{"x": [1, 2, 3, 4, 5], "y": [1, 2, 4, 8, 16] },{"x": [6, 2, 3, 4, 5], "y": [10, 3, 5, 7, 16] }]'
    layout='{"margin": { "t": 0 } }'
    session$sendCustomMessage(type="setPlot",message=list("plotlycont",plotly,layout))
    svgcontent='<svg width="400" height="180"><rect x="50" y="20" rx="20" ry="20" width="150" height="150" style="fill:red;stroke:black;stroke-width:5;opacity:0.5" /></svg>'
    session$sendCustomMessage(type="setInnerHTML",message=list("svgcont",svgcontent))
    }
  })
  
})
