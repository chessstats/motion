
library(shiny)
library(plotly)

dt<-read.table("dt.txt")
players<-colnames(dt[2:(length(dt[1,])-2)])
dt$year=2000+dt$year/100

shinyServer(function(input, output, session){
  
  output$trendPlot <- renderPlotly({
    
    selp=c("Carlsen..Magnus","Nakamura..Hikaru")
    
    command<-paste("p<-plot_ly(dt,type='scatter',x=year,y=",selp[1],",name='",selp[1],"')",sep="")
    #print(command)
    eval(parse(text=command))
    
    if(length(selp)>1) for(j in 2:length(selp)) {
      command=paste("p<-add_trace(p,type='scatter',x=year,y=",selp[j],",name='",selp[j],"')",sep="")
      #print(command)
      eval(parse(text=command))
    }
    
    p<-layout(p)
    
    p
  })
  
  observeEvent(input$rplayers,{
    session$sendCustomMessage(type="setPlayers",message=players)
  })
  
  observeEvent(input$control,{
    tablestr="<table id='mytable' class='display'><thead><tr><th>head</th></tr></thead><tbody><tr><td>body</td></tr></tbody>"
    session$sendCustomMessage(type="setTable",message=list("tablecont",tablestr,"mytable"))
    plotly='[{"x": [1, 2, 3, 4, 5], "y": [1, 2, 4, 8, 16] },{"x": [6, 2, 3, 4, 5], "y": [10, 3, 5, 7, 16] }]'
    layout='{"margin": { "t": 0 } }'
    session$sendCustomMessage(type="setPlot",message=list("plotlycont",plotly,layout))
    svgcontent='<svg width="400" height="180"><rect x="50" y="20" rx="20" ry="20" width="150" height="150" style="fill:red;stroke:black;stroke-width:5;opacity:0.5" /></svg>'
    session$sendCustomMessage(type="setInnerHTML",message=list("svgcont",svgcontent))
  })
  
})
