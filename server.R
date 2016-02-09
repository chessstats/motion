
library(shiny)

shinyServer(function(input, output, session){
  
  observeEvent(input$control,{
    tablestr="<table id='mytable' class='display'><thead><tr><th>head</th></tr></thead><tbody><tr><td>body</td></tr></tbody>"
    session$sendCustomMessage(type="setTable",message=list("mytablecont",tablestr,"mytable"))
    plotly='[{"x": [1, 2, 3, 4, 5], "y": [1, 2, 4, 8, 16] },{"x": [6, 2, 3, 4, 5], "y": [10, 3, 5, 7, 16] }]'
    layout='{"margin": { "t": 0 } }'
    session$sendCustomMessage(type="setPlot",message=list(plotly,layout))
    svgcontent='<svg width="400" height="180"><rect x="50" y="20" rx="20" ry="20" width="150" height="150" style="fill:red;stroke:black;stroke-width:5;opacity:0.5" /></svg>'
    session$sendCustomMessage(type="setInnerHTML",message=list("svgcont",svgcontent))
  })
  
})
