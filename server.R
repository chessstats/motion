
library(shiny)
library(plotly)

dt<-read.table("dt.txt")
players<-colnames(dt[2:(length(dt[1,])-2)])
dt$year=2000+dt$year/100

chartsize<<-800
startyear<<-2001

shinyServer(function(input, output, session){
  
  drawchart<- function(){
    output$trendPlot <- renderPlotly({
      
      selp=input$control
      if(length(selp)==0) {
        selp=c("Carlsen..Magnus","Nakamura..Hikaru")
      }
      
      starti<-1
      for(j in 1:nrow(dt)){
        if(dt$year[j]>=startyear){
          starti<-j
          break
        }
      }
      
      dteff<-dt[starti:nrow(dt),]
      
      command<-paste("p<-plot_ly(dteff,type='scatter',x=year,y=",selp[1],",name='",selp[1],"')",sep="")
      #print(command)
      eval(parse(text=command))
      
      if(length(selp)>1) for(j in 2:length(selp)) {
        command=paste("p<-add_trace(p,type='scatter',x=year,y=",selp[j],",name='",selp[j],"')",sep="")
        #print(command)
        eval(parse(text=command))
      }
      
      p<-layout(p,height=round(chartsize/1.5),width=chartsize)
      
      p
    })
  }
  
  observeEvent(input$rplayers,{
    session$sendCustomMessage(type="setPlayers",message=players)
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
