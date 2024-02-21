plotInit<-function(){

  if(is.null(configOptions$stringFormat)){
    configOptions$stringFormat<<-"%Y-%m-%d %H:%M:%S"
  }

  seqCols<<-c('#1b9e77','#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02','#a6761d','#666666')

  yearBio<<-reactive({
    tempIndex<-input$currentIndividualSelector
    tempYear<-paste0(migtime[which(migtime$id_bioYear==tempIndex),"bioYearFull"],'-',format(as.Date(bioYearStartDate), "%m-%d"))
    return(tempYear)
  })

  yearBioNext<<-reactive({
    tempIndex<-input$currentIndividualSelector
    tempYear<-paste0(as.numeric(migtime[which(migtime$id_bioYear==tempIndex),"bioYearFull"])+1,'-',format(as.Date(bioYearStartDate)-1, "%m-%d"))
    return(tempYear)
  })

  dataForPlots<<-reactive({

    tempIndex<-input$currentIndividualSelector    
    tempDat<-importedDatasetMaster[importedDatasetMaster$id_bioYear == tempIndex,c('newMasterDate','newUid')]
    tempUid<<-tempDat[1,'newUid']
    minDate<<-min(tempDat[,'newMasterDate'])
    maxDate<<-max(tempDat[,'newMasterDate'])

    sixMonthMin<-as.Date(minDate)-182
    sixMonthMax<-as.Date(maxDate)+182

    plotData<-importedDatasetMaster[
      importedDatasetMaster$id_bioYear == tempIndex & 
      importedDatasetMaster$problem != 1 &
      importedDatasetMaster$mortality != 1,
      c('newMasterDate','nsdBio','displacementBio','speed','elev','FPT50','FPT150','FPT300')
    ]

    plotDataSixMonth<-importedDatasetMaster[
      which(importedDatasetMaster$newUid == tempUid & 
      as.Date(importedDatasetMaster$newMasterDate)>=sixMonthMin & 
      as.Date(importedDatasetMaster$newMasterDate)<=sixMonthMax) &
      importedDatasetMaster$problem != 1 &
      importedDatasetMaster$mortality != 1,
      c('newMasterDate','nsdBio','displacementBio','speed','elev','FPT50','FPT150','FPT300')
    ]

    


    tempList<<-list()
    tempList$plotData<<-plotData
    tempList$plotDataSixMonth<<-plotDataSixMonth

    return(tempList)
  })

  dataForPlotsTwoYears<<-reactive({


  })
  
  dataForSequencePlots<<-reactive({
    trig<-input$nsdTypeSelect
    tempD<-importedDatasetMaster[
      importedDatasetMaster$id_bioYear == currentIndividual,
      c('newMasterDate','nsdBio','displacementBio','speed','elev')
    ]

    seq1<<-input$dateSlider1
    seq2<<-input$dateSlider2
    seq3<<-input$dateSlider3
    seq4<<-input$dateSlider4
    seq5<<-input$dateSlider5
    seq6<<-input$dateSlider6
    seq7<<-input$dateSlider7
    seq8<<-input$dateSlider8

    sequencesData<-tempD[
      which(as.Date(tempD$newMasterDate)>=as.Date(seq1[1])-1 & as.Date(tempD$newMasterDate)<=as.Date(seq1[2])+1),]

    for(i in 2:totalSequences){
      thisSeq<-get(paste0('seq',i))
      rowsToBind<-tempD[
        which(as.Date(tempD$newMasterDate)>=as.Date(thisSeq[1])-1 & as.Date(tempD$newMasterDate)<=as.Date(thisSeq[2])+1),]
      sequencesData<-rbind(sequencesData,rowsToBind)
    }

    sequencesData$index<-c(1:nrow(sequencesData))

    sequencesDataa<<-sequencesData

    return(sequencesData)
  })

  getIndexForSequencePlots<<-function(thisDate){
    thisFirstIndex<-sequencesDataa[as.Date(sequencesDataa$newMasterDate)==as.Date(thisDate),'index'][1]
    return(thisFirstIndex)
  }

  seq1min<<-reactive({
    tempData<-as.Date(input$dateSlider1[1])
    return(tempData)
  })

  seq1max<<-reactive({
    tempData<-as.Date(input$dateSlider1[2])
    if(format(tempData,"%m%d")=='0229'){
      tempData=tempData-1
    }
    if(format(tempData,"%m%d")== format(as.Date(bioYearStartDate)-1,"%m%d") & format(seq1min(),"%m%d")==format(as.Date(bioYearStartDate),"%m%d")){
      return(seq1min())
    } else{
      return(tempData)
    }
    })

    seq2min<<-reactive({
      tempData<-as.Date(input$dateSlider2[1])
      return(tempData)
    })

    seq2max<<-reactive({
      tempData<-as.Date(input$dateSlider2[2])
      if(format(tempData,"%m%d")=='0229'){
        tempData=tempData-1
      }
      if(format(tempData,"%m%d")== format(as.Date(bioYearStartDate)-1,"%m%d") & format(seq2min(),"%m%d")==format(as.Date(bioYearStartDate),"%m%d")){
        return(seq2min())
      } else{
        return(tempData)
      }
      })


      seq3min<<-reactive({
        tempData<-as.Date(input$dateSlider3[1])
        return(tempData)
      })

      seq3max<<-reactive({
        tempData<-as.Date(input$dateSlider3[2])
        if(format(tempData,"%m%d")=='0229'){
          tempData=tempData-1
        }
        if(format(tempData,"%m%d")== format(as.Date(bioYearStartDate)-1,"%m%d") & format(seq3min(),"%m%d")==format(as.Date(bioYearStartDate),"%m%d")){
          return(seq3min())
        } else{
          return(tempData)
        }
        })

        seq4min<<-reactive({
          tempData<-as.Date(input$dateSlider4[1])
          return(tempData)
        })

        seq4max<<-reactive({
          tempData<-as.Date(input$dateSlider4[2])
          if(format(tempData,"%m%d")=='0229'){
            tempData=tempData-1
          }
          if(format(tempData,"%m%d")== format(as.Date(bioYearStartDate)-1,"%m%d") & format(seq4min(),"%m%d")==format(as.Date(bioYearStartDate),"%m%d")){
            return(seq4min())
          } else{
            return(tempData)
          }
          })


          seq5min<<-reactive({
            tempData<-as.Date(input$dateSlider5[1])
            return(tempData)
          })

          seq5max<<-reactive({
            tempData<-as.Date(input$dateSlider5[2])
            if(format(tempData,"%m%d")=='0229'){
              tempData=tempData-1
            }
            if(format(tempData,"%m%d")== format(as.Date(bioYearStartDate)-1,"%m%d") & format(seq5min(),"%m%d")==format(as.Date(bioYearStartDate),"%m%d")){
              return(seq5min())
            } else{
              return(tempData)
            }
            })

            seq6min<<-reactive({
              tempData<-as.Date(input$dateSlider6[1])
              return(tempData)
            })

            seq6max<<-reactive({
              tempData<-as.Date(input$dateSlider6[2])
              if(format(tempData,"%m%d")=='0229'){
                tempData=tempData-1
              }
              if(format(tempData,"%m%d")== format(as.Date(bioYearStartDate)-1,"%m%d") & format(seq6min(),"%m%d")==format(as.Date(bioYearStartDate),"%m%d")){
                return(seq6min())
              } else{
                return(tempData)
              }
              })

              seq7min<<-reactive({
                tempData<-as.Date(input$dateSlider7[1])
                return(tempData)
              })

              seq7max<<-reactive({
                tempData<-as.Date(input$dateSlider7[2])
                if(format(tempData,"%m%d")=='0229'){
                  tempData=tempData-1
                }
                if(format(tempData,"%m%d")== format(as.Date(bioYearStartDate)-1,"%m%d") & format(seq7min(),"%m%d")==format(as.Date(bioYearStartDate),"%m%d")){
                  return(seq7min())
                } else{
                  return(tempData)
                }
                })

              seq8min<<-reactive({
                tempData<-as.Date(input$dateSlider8[1])
                return(tempData)
              })

              seq8max<<-reactive({
                tempData<-as.Date(input$dateSlider8[2])
                if(format(tempData,"%m%d")=='0229'){
                  tempData=tempData-1
                }
                if(format(tempData,"%m%d")== format(as.Date(bioYearStartDate)-1,"%m%d") & format(seq8min(),"%m%d")==format(as.Date(bioYearStartDate),"%m%d")){
                  return(seq8min())
                } else{
                  return(tempData)
                }
                })


    output$plot0 <- renderPlot({
      sequencesPlot<-ggplot(dataForSequencePlots(), aes(index, get(nsdType))) +
        annotate("rect", xmin=getIndexForSequencePlots(seq1min()), xmax=getIndexForSequencePlots(seq1max()), ymin=0, ymax=Inf,alpha=0.65, fill=seqCols[1])+
        annotate("rect", xmin=getIndexForSequencePlots(seq2min()), xmax=getIndexForSequencePlots(seq2max()), ymin=0, ymax=Inf,alpha=0.65, fill=seqCols[2])+
        annotate("rect", xmin=getIndexForSequencePlots(seq3min()), xmax=getIndexForSequencePlots(seq3max()), ymin=0, ymax=Inf,alpha=0.65, fill=seqCols[3])+
        annotate("rect", xmin=getIndexForSequencePlots(seq4min()), xmax=getIndexForSequencePlots(seq4max()), ymin=0, ymax=Inf,alpha=0.65, fill=seqCols[4])+
        annotate("rect", xmin=getIndexForSequencePlots(seq5min()), xmax=getIndexForSequencePlots(seq5max()), ymin=0, ymax=Inf,alpha=0.65, fill=seqCols[5])+
        annotate("rect", xmin=getIndexForSequencePlots(seq6min()), xmax=getIndexForSequencePlots(seq6max()), ymin=0, ymax=Inf,alpha=0.65, fill=seqCols[6])+
        annotate("rect", xmin=getIndexForSequencePlots(seq7min()), xmax=getIndexForSequencePlots(seq7max()), ymin=0, ymax=Inf,alpha=0.65, fill=seqCols[7])+
        annotate("rect", xmin=getIndexForSequencePlots(seq8min()), xmax=getIndexForSequencePlots(seq8max()), ymin=0, ymax=Inf,alpha=0.65, fill=seqCols[8])+
        geom_line(size= 0.5, color="black")+
        geom_point(size = 1.5, color="red")+
        xlab('Index')+
        ylab(plotLabel)+
        theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))
        # scale_x_date(breaks=as.Date(dataForSequencePlots()$newMasterDate))
        print(sequencesPlot)
    })


  output$plot1 <- renderPlot({
    nsdPlot<-ggplot(dataForPlots()$plotData, aes(as.Date(newMasterDate,format=configOptions$stringFormat), get(nsdType))) +
      annotate("rect", xmin=seq1min(), xmax=seq1max(), ymin=0, ymax=Inf,alpha=0.65, fill=seqCols[1])+
      annotate("rect", xmin=seq2min(), xmax=seq2max(), ymin=0, ymax=Inf,alpha=0.65, fill=seqCols[2])+
      annotate("rect", xmin=seq3min(), xmax=seq3max(), ymin=0, ymax=Inf,alpha=0.65, fill=seqCols[3])+
      annotate("rect", xmin=seq4min(), xmax=seq4max(), ymin=0, ymax=Inf,alpha=0.65, fill=seqCols[4])+
      annotate("rect", xmin=seq5min(), xmax=seq5max(), ymin=0, ymax=Inf,alpha=0.65, fill=seqCols[5])+
      annotate("rect", xmin=seq6min(), xmax=seq6max(), ymin=0, ymax=Inf,alpha=0.65, fill=seqCols[6])+
      annotate("rect", xmin=seq7min(), xmax=seq7max(), ymin=0, ymax=Inf,alpha=0.65, fill=seqCols[7])+
      annotate("rect", xmin=seq8min(), xmax=seq8max(), ymin=0, ymax=Inf,alpha=0.65, fill=seqCols[8])+
      geom_line(size= 0.5, color="black")+
      geom_point(size = 1.5, color="red")+
      xlab('Date')+
      ylab(plotLabel)+
      theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))+
      scale_x_date(limits = as.Date(c(yearBio(),yearBioNext())))
      print(nsdPlot)
  })

  output$plot7 <- renderPlot({
    nsdPlot<-ggplot(dataForPlots()$plotDataSixMonth, aes(as.Date(newMasterDate,format=configOptions$stringFormat), get(nsdType))) +
      annotate("rect", xmin=seq1min(), xmax=seq1max(), ymin=0, ymax=Inf,alpha=0.65, fill=seqCols[1])+
      annotate("rect", xmin=seq2min(), xmax=seq2max(), ymin=0, ymax=Inf,alpha=0.65, fill=seqCols[2])+
      annotate("rect", xmin=seq3min(), xmax=seq3max(), ymin=0, ymax=Inf,alpha=0.65, fill=seqCols[3])+
      annotate("rect", xmin=seq4min(), xmax=seq4max(), ymin=0, ymax=Inf,alpha=0.65, fill=seqCols[4])+
      annotate("rect", xmin=seq5min(), xmax=seq5max(), ymin=0, ymax=Inf,alpha=0.65, fill=seqCols[5])+
      annotate("rect", xmin=seq6min(), xmax=seq6max(), ymin=0, ymax=Inf,alpha=0.65, fill=seqCols[6])+
      annotate("rect", xmin=seq7min(), xmax=seq7max(), ymin=0, ymax=Inf,alpha=0.65, fill=seqCols[7])+
      annotate("rect", xmin=seq8min(), xmax=seq8max(), ymin=0, ymax=Inf,alpha=0.65, fill=seqCols[8])+
      geom_line(size= 0.5, color="black")+
      geom_point(size = 1.5, color="red")+
      xlab('Date')+
      ylab(plotLabel)+
      theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))+
      scale_x_date(limits = as.Date(c(min(dataForPlots()$plotDataSixMonth$newMasterDate),max(dataForPlots()$plotDataSixMonth$newMasterDate))))
      print(nsdPlot)
  })

  output$plot2 <- renderPlot({
      speedPlot<-ggplot(dataForPlots()$plotData, aes(as.Date(newMasterDate,format=configOptions$stringFormat), speed)) +
      annotate("rect", xmin=seq1min(), xmax=seq1max(), ymin=0, ymax=Inf,alpha=0.65, fill=seqCols[1])+
      annotate("rect", xmin=seq2min(), xmax=seq2max(), ymin=0, ymax=Inf,alpha=0.65, fill=seqCols[2])+
      annotate("rect", xmin=seq3min(), xmax=seq3max(), ymin=0, ymax=Inf,alpha=0.65, fill=seqCols[3])+
      annotate("rect", xmin=seq4min(), xmax=seq4max(), ymin=0, ymax=Inf,alpha=0.65, fill=seqCols[4])+
      annotate("rect", xmin=seq5min(), xmax=seq5max(), ymin=0, ymax=Inf,alpha=0.65, fill=seqCols[5])+
      annotate("rect", xmin=seq6min(), xmax=seq6max(), ymin=0, ymax=Inf,alpha=0.65, fill=seqCols[6])+
      annotate("rect", xmin=seq7min(), xmax=seq7max(), ymin=0, ymax=Inf,alpha=0.65, fill=seqCols[7])+
      annotate("rect", xmin=seq8min(), xmax=seq8max(), ymin=0, ymax=Inf,alpha=0.65, fill=seqCols[8])+
      geom_line(size= 0.5, color="black")+
      geom_point(size = 1.5, color="red")+
      xlab('Date')+
      ylab('Speed (km/hr)')+
      theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))+
      scale_x_date(limits = as.Date(c(yearBio(),yearBioNext())))
      print(speedPlot)
  })

  output$plot3 <- renderPlot({
    elevPlot<-ggplot(dataForPlots()$plotData, aes(as.Date(newMasterDate,format=configOptions$stringFormat), elev)) +
      annotate("rect", xmin=seq1min(), xmax=seq1max(), ymin=min(dataForPlots()$plotData$elev), ymax=max(dataForPlots()$plotData$elev),alpha=0.65, fill=seqCols[1])+
      annotate("rect", xmin=seq2min(), xmax=seq2max(), ymin=min(dataForPlots()$plotData$elev), ymax=max(dataForPlots()$plotData$elev),alpha=0.65, fill=seqCols[2])+
      annotate("rect", xmin=seq3min(), xmax=seq3max(), ymin=min(dataForPlots()$plotData$elev), ymax=max(dataForPlots()$plotData$elev),alpha=0.65, fill=seqCols[3])+
      annotate("rect", xmin=seq4min(), xmax=seq4max(), ymin=min(dataForPlots()$plotData$elev), ymax=max(dataForPlots()$plotData$elev),alpha=0.65, fill=seqCols[4])+
      annotate("rect", xmin=seq5min(), xmax=seq5max(), ymin=min(dataForPlots()$plotData$elev), ymax=max(dataForPlots()$plotData$elev),alpha=0.65, fill=seqCols[5])+
      annotate("rect", xmin=seq6min(), xmax=seq6max(), ymin=min(dataForPlots()$plotData$elev), ymax=max(dataForPlots()$plotData$elev),alpha=0.65, fill=seqCols[6])+
      annotate("rect", xmin=seq7min(), xmax=seq7max(), ymin=min(dataForPlots()$plotData$elev), ymax=max(dataForPlots()$plotData$elev),alpha=0.65, fill=seqCols[7])+
      annotate("rect", xmin=seq8min(), xmax=seq8max(), ymin=min(dataForPlots()$plotData$elev), ymax=max(dataForPlots()$plotData$elev),alpha=0.65, fill=seqCols[8])+
      geom_line(size= 0.5, color="black")+
      geom_point(size = 1.5, color="red")+
      xlab('Date')+
      ylab('Elevation (meters)')+
      theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))+
      scale_x_date(limits = as.Date(c(yearBio(),yearBioNext())))+
      scale_y_continuous(limits=c(min(dataForPlots()$plotData$elev),max(dataForPlots()$plotData$elev)))
      print(elevPlot)
  })

  output$plot4 <- renderPlot({
    fpt50Plot<-ggplot(dataForPlots()$plotData, aes(as.Date(newMasterDate,format=configOptions$stringFormat), FPT50)) +
      annotate("rect", xmin=seq1min(), xmax=seq1max(), ymin=0, ymax=Inf,alpha=0.65, fill=seqCols[1])+
      annotate("rect", xmin=seq2min(), xmax=seq2max(), ymin=0, ymax=Inf,alpha=0.65, fill=seqCols[2])+
      annotate("rect", xmin=seq3min(), xmax=seq3max(), ymin=0, ymax=Inf,alpha=0.65, fill=seqCols[3])+
      annotate("rect", xmin=seq4min(), xmax=seq4max(), ymin=0, ymax=Inf,alpha=0.65, fill=seqCols[4])+
      annotate("rect", xmin=seq5min(), xmax=seq5max(), ymin=0, ymax=Inf,alpha=0.65, fill=seqCols[5])+
      annotate("rect", xmin=seq6min(), xmax=seq6max(), ymin=0, ymax=Inf,alpha=0.65, fill=seqCols[6])+
      annotate("rect", xmin=seq7min(), xmax=seq7max(), ymin=0, ymax=Inf,alpha=0.65, fill=seqCols[7])+
      annotate("rect", xmin=seq8min(), xmax=seq8max(), ymin=0, ymax=Inf,alpha=0.65, fill=seqCols[8])+
      geom_line(size= 0.5, color="black")+
      geom_point(size = 1.5, color="red")+
      xlab('Date')+
      ylab('Hours')+
      theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))+
      scale_x_date(limits = as.Date(c(yearBio(),yearBioNext())))
      print(fpt50Plot)
  })

  output$plot5 <- renderPlot({
    fpt150Plot<-ggplot(dataForPlots()$plotData, aes(as.Date(newMasterDate,format=configOptions$stringFormat), FPT150)) +
      annotate("rect", xmin=seq1min(), xmax=seq1max(), ymin=0, ymax=Inf,alpha=0.65, fill=seqCols[1])+
      annotate("rect", xmin=seq2min(), xmax=seq2max(), ymin=0, ymax=Inf,alpha=0.65, fill=seqCols[2])+
      annotate("rect", xmin=seq3min(), xmax=seq3max(), ymin=0, ymax=Inf,alpha=0.65, fill=seqCols[3])+
      annotate("rect", xmin=seq4min(), xmax=seq4max(), ymin=0, ymax=Inf,alpha=0.65, fill=seqCols[4])+
      annotate("rect", xmin=seq5min(), xmax=seq5max(), ymin=0, ymax=Inf,alpha=0.65, fill=seqCols[5])+
      annotate("rect", xmin=seq6min(), xmax=seq6max(), ymin=0, ymax=Inf,alpha=0.65, fill=seqCols[6])+
      annotate("rect", xmin=seq7min(), xmax=seq7max(), ymin=0, ymax=Inf,alpha=0.65, fill=seqCols[7])+
      annotate("rect", xmin=seq8min(), xmax=seq8max(), ymin=0, ymax=Inf,alpha=0.65, fill=seqCols[8])+
      geom_line(size= 0.5, color="black")+
      geom_point(size = 1.5, color="red")+
      xlab('Date')+
      ylab('Hours')+
      theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))+
      scale_x_date(limits = as.Date(c(yearBio(),yearBioNext())))
      print(fpt150Plot)
  })

  output$plot6 <- renderPlot({
    fpt300Plot<-ggplot(dataForPlots()$plotData, aes(as.Date(newMasterDate,format=configOptions$stringFormat), FPT300)) +
      annotate("rect", xmin=seq1min(), xmax=seq1max(), ymin=0, ymax=Inf,alpha=0.65, fill=seqCols[1])+
      annotate("rect", xmin=seq2min(), xmax=seq2max(), ymin=0, ymax=Inf,alpha=0.65, fill=seqCols[2])+
      annotate("rect", xmin=seq3min(), xmax=seq3max(), ymin=0, ymax=Inf,alpha=0.65, fill=seqCols[3])+
      annotate("rect", xmin=seq4min(), xmax=seq4max(), ymin=0, ymax=Inf,alpha=0.65, fill=seqCols[4])+
      annotate("rect", xmin=seq5min(), xmax=seq5max(), ymin=0, ymax=Inf,alpha=0.65, fill=seqCols[5])+
      annotate("rect", xmin=seq6min(), xmax=seq6max(), ymin=0, ymax=Inf,alpha=0.65, fill=seqCols[6])+
      annotate("rect", xmin=seq7min(), xmax=seq7max(), ymin=0, ymax=Inf,alpha=0.65, fill=seqCols[7])+
      annotate("rect", xmin=seq8min(), xmax=seq8max(), ymin=0, ymax=Inf,alpha=0.65, fill=seqCols[8])+
      geom_line(size= 0.5, color="black")+
      geom_point(size = 1.5, color="red")+
      xlab('Date')+
      ylab('Hours')+
      theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))+
      scale_x_date(limits = as.Date(c(yearBio(),yearBioNext())))
      print(fpt300Plot)
  })





}
