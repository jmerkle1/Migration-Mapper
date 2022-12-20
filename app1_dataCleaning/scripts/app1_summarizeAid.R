updateSummaryStats<-function(){

  if(nrow(pointsForMap)==0){
    output$dateRangeInfo <- renderUI({
      HTML(HTML(paste0('<span>Earliest Observation: NA<br>Latest Observation: NA</span>')))
    })
    output$pointsPerTimeInfo <- renderUI({
      HTML('<span>Avg points per day: NA<br>Avg points per month: NA</span>')
    })
    output$fixRateInfo <- renderUI({
      HTML(paste0('<span>Median fix rate: NA</span>'))
    })
    output$mcpInfo <- renderUI({
      HTML('<span>MCP Area: NA</span>')
    })


    return()
  }

  minDate<-min(pointsForMap$newMasterDate)
  maxDate<-max(pointsForMap$newMasterDate)
  output$dateRangeInfo <- renderUI({
    HTML(HTML(paste0('<span>Earliest Observation: ',minDate,'<br>Latest Observation: ',maxDate,'</span>')))
  })

  pointsPerDay<-mean(table(factor(format(pointsForMap$newMasterDate,"%D"))),na.rm=TRUE)
  pointsPerMonth<-data.frame(table(factor(format(pointsForMap$newMasterDate,"%m"))))

  theseMonths<-unique(format(pointsForMap$newMasterDate,"%m"))

  daysString<-paste0('<span>Avg points per day: ',round(pointsPerDay,1),'<span><br>TOTAL POINTS PER MONTH<br>')

  monthsString<-''
  for(i in 1:length(theseMonths)){
    thisMonthIndex<-theseMonths[i]
    thisMonthName<-month.abb[as.numeric(thisMonthIndex)]
    thisMonthCount<-pointsPerMonth[which(pointsPerMonth[,1]==thisMonthIndex),2]
    monthsString<-paste0(monthsString,'<span>',thisMonthName,': ',thisMonthCount,'<span><br>')
  }

  pointsString<-paste0(daysString,monthsString)
  output$pointsPerTimeInfo <- renderUI({
    HTML(pointsString)
  })

  fixRate<-median(pointsForMap$fixRateHours,na.rm=TRUE)
  output$fixRateInfo <- renderUI({
    HTML(paste0('<span>Median fix rate: ',round(fixRate,1),' hours</span>'))
  })

  if(nrow(pointsForMap)<6){
    output$mcpInfo <- renderUI({
      HTML('<span>No MCP info! Too few points for this individual</span>')
    })    
    }else{
      mcp100<-mcp(pointsForMap,percent=100,unout='km2')$area
      mcp99<-mcp(pointsForMap,percent=99,unout='km2')$area
      mcp95<-mcp(pointsForMap,percent=95,unout='km2')$area
      output$mcpInfo <- renderUI({
        HTML('<span>MCP 100% - Area: ',round(mcp100,digits=1),' km2</span><br><span>MCP 99% - Area: ',round(mcp99,digits=1),' km2</span><br><span>MCP 95% - Area: ',round(mcp95,digits=1),' km2</span>')
      })
    }



  
  

}
