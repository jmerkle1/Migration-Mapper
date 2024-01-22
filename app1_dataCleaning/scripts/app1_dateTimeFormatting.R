showDateTimeSelectionPanel<-function(){
  hide(id='uidSeletorRow',anim=TRUE)
  print('showing date time')
  if(thisIsTestingRun){
    hasObjectsHandler()
  }
  showElement(id = 'dateTimeRow', anim = TRUE)


  ##------------------ add to date columns whenever one is selected
    observeEvent(input$dateColumnSelector, {
      print('date column select change')
      dateColumns <<- input$dateColumnSelector
      print(dateColumns)
    })

    observeEvent(input$doneChoosingDateColumnsButton,{
      processDates();
      })

  ##------------------ show the first 20 rows of data  
  rowsToShow<-st_drop_geometry(importedDatasetMaster[1:20,])
  if('timestamp'%in%names(rowsToShow)){
    rowsToShow$timestamp<-as.character(rowsToShow$timestamp)
  }  
  output$dateConfigTable1 <- renderTable(rowsToShow)

  # uniqueColumns<-names(importedDatasetMaster@data)
  uniqueColumns<-names(importedDatasetMaster)
  selectedDateColumns<-NULL

  updateCheckboxGroupInput(session,
    "dateColumnSelector",
      choices=uniqueColumns,
      selected=selectedDateColumns,
      inline=TRUE
    )
}

processDates<-function(){

  if(!exists('dateColumns')){
    modalMessager('ERROR','You need to select the column(s) which contain your dates')
    dtvRunning<<-FALSE;
    return()
  }

  if(length(dateColumns)==0){
    modalMessager('ERROR','You need to select the column(s) which contain your dates')
    dtvRunning<<-FALSE;
    return()
  }

  dateColumns <<- input$dateColumnSelector

  if(is.null(dateColumns)){
    modalMessager('ERROR','You need to select the column(s) which contain your dates')
    dtvRunning<<-FALSE;
    return()
  }



  hideElement(id = 'dateTimeRow', anim = TRUE)
  showElement(id = 'dateTimeElementsRow', anim = TRUE)




  ##------------------ show the first 20 rows of data
  rowsToShow<-st_drop_geometry(importedDatasetMaster[1:20,])
  if('timestamp'%in%names(rowsToShow)){
    rowsToShow$timestamp<-as.character(rowsToShow$timestamp)
  }
  output$dateConfigTable2 <- renderTable(rowsToShow)
  ###-- create multiselectors for each of the selected date/time columns
  lapply(1:length(dateColumns), function(i) {
    tempVar<-paste("dateConfigUi",i,sep="")
    tempSelectorName<-paste(dateColumns[i],"Indicator",sep="")


    #if the columnFieldList exists in the progress tracker, we have to fill
    #in the appropriate selections in the selectors - furthermore, they have to be
    # in a specific order or R will rearrange elements
    if('columnFieldList'%in%names(configOptions)){
      if(dateColumns[i]%in%names(configOptions$columnFieldList)){
        # these are the date time elements that have already been selected for
        # this field dateColumns[i]
        selectedColumns<-configOptions$columnFieldList[[dateColumns[i]]]
        #these are the selected key and value from the datetime list
        selectedKeys<-dateTimesList[configOptions$columnFieldList[[dateColumns[i]]]]
        #next we need to grab those list() items from the date times flipped
        #to reconstruct the select menu in the correct order
        newSelectedColumns<-c()
        for(j in 1:length(selectedKeys)){
          newSelectedColumns<-c(newSelectedColumns,selectedKeys[[j]])
        }
        #these are the elements that need to be added to select menu first in
        #appropriate order
        includedDateTimeElements<-dateTimesListFlipped[newSelectedColumns]
        excludedDateTimeElements<-which(!names(dateTimesListFlipped) %in% newSelectedColumns)
        excludedDateTimeElements<-dateTimesListFlipped[excludedDateTimeElements]
        newOrderedChoices<-c(includedDateTimeElements,excludedDateTimeElements)

        # updateSelectInput(session, tempSelectorName, label = NULL, selected = selectedColumns,newOrderedChoices)
        print('rendering ')
        print(tempSelectorName)
        output[[tempVar]] <- renderUI({
          selectInput(
            tempSelectorName,
            paste("Choose date time elements for: ",
            dateColumns[i],sep=""),
            selected=selectedColumns,
            newOrderedChoices,
            multiple=TRUE
            )
        })

        observeEvent(input[[tempSelectorName]], {
          collectDateTimeElements(input[[tempSelectorName]],dateColumns[i])
        },ignoreInit=TRUE)
    }
  } else{
    selectedColumns<-NULL
    print('rendering ')
    print(tempSelectorName)
    output[[tempVar]] <- renderUI({
      selectInput(
        tempSelectorName,
        paste("Choose date time elements for: ",
        dateColumns[i],sep=""),
        selected=selectedColumns,
        dateTimesListFlipped,
        multiple=TRUE
        )
    })

    observeEvent(input[[tempSelectorName]], {
      collectDateTimeElements(input[[tempSelectorName]],dateColumns[i])
    },ignoreInit=TRUE)
  }



    observeEvent(input$testButton, {
      d<-c("%Y" ,"%m" ,"%d" ,"%H" ,"%M" ,"%S")
      updateSelectInput(session, tempSelectorName, label = NULL, choices = NULL, selected = d)
    },ignoreInit=TRUE)
  })
}


## this fires every times there is a change to the date/time selector
  ## the values are then stored in an object where the col name is the key
  ## and the value is the order of date/time elements in that value
  columnFieldList<<-list()
  collectDateTimeElements<-function(selectorValues,column){
    columnFieldList[[column]]<<-selectorValues
  }




  dateTimeValidator<-function(){
    dtvRunning<<-TRUE
    shinyjs::disable("yearStartDateSelector")
    #recreate the validatorObject anytime process dates is clicked
    #in case back actions and changes
    if(length(columnFieldList)==0){
      modalMessager('ERROR','You have not selected value for your date time fields. Please try again.')
      dtvRunning<<-FALSE
      return()
    }
    validatorObject<<-list()
    for(i in 1:length(columnFieldList)){
      # these are the names of all fields in columnFieldList
      tempNames<-names(columnFieldList)
      tempColumn<-tempNames[i]
      #these are all the dt elements in that column
      tempColumnItems<-columnFieldList[[i]]
      #since there could be multiple DT elements in one column
      #we'll iterate through even if there is only one
      for(j in 1:length(tempColumnItems)){
        #this is the particular dt value in that column
        tempValue<-tempColumnItems[j]
        ## j ends up being the position in that column
        updateKeyObj(tempValue,j,tempColumn)
      }
      ##if its the last iteration then fire the next fxn
      if(i==length(columnFieldList)){
        hasObjectsHandler()
      }
    }
  }

  updateKeyObj<-function(tempValue,j,column){
    #tempvariable for literal dt value
    tempDTelementType<-dateTimeLookup[[tempValue]]
    #check if this has already been recorded in the validator object
    # print(!is.null(validatorObject[[tempDTelementType]]))
    # send this object to the
    # if there is ampm data.. we'll store this in a seperate validatorObject and return
    if(tempValue=='%p'){
      ampmtime<<-TRUE
      validatorObjectAmPm<<-list()
      validatorObjectAmPm[[tempDTelementType]]<<-c(tempDTelementType,j,tempValue,column)
      return()
    }
    validatorObject[[tempDTelementType]]<<-c(tempDTelementType,j,tempValue,column)
  }


  hasObjectsHandler<-function(){
    startTime<<-Sys.time()
    progressIndicator('Processing Dates.. Please wait','start')
    loadingScreenToggle('show','processing dates')
    # importedDatasetMaster@data$newMasterDate<<--999
    importedDatasetMaster$newMasterDate<<--999

    # if there happens to be am/pm time.. lets quickly create a vector to hold these
    # we'll paste it back to the time later
    if(ampmtime){
      # this is the column where ampm lives
      tempDtCol<-validatorObjectAmPm[[1]][4]
      # temp vector of this column
      # ampm<<-importedDatasetMaster@data[,tempDtCol]
      ampm<<-importedDatasetMaster[,tempDtCol]
      # remove everything in this column thats not a,m,p,A,M,P
      ampm<<-gsub("[^amp|AMP]","",ampm)
    }



    #  since the loop below runs from all possible date time elements
    # errors are thrown when not all seconds, minutes etc have been selected
    # need to run this looop based in validatorObject
    selectedDateTimeElements<-names(validatorObject)
    validatorsLength<-length(selectedDateTimeElements)
    for(j in 1:validatorsLength){
      processingInfo<-paste0(sprintf("%.1f",round((j/(validatorsLength+1)*100),1)),'% ','complete')
      progressIndicator(processingInfo,'update',j/(validatorsLength+1))
      # what is the type
      tempDtType<-selectedDateTimeElements[j]

      # what position is it in this specific column?
      tempIndexOfLoc<-as.numeric(validatorObject[[tempDtType]][2])
      # lookup the posix type (?necessary?)
      # tempDtPosixType<-validatorObject[[validatorValues[[j]]]][3]
      # which column is it in
      tempDtCol<-validatorObject[[tempDtType]][4]

      # -------------
      # NEW 2019 - DEALING WITH TWO DIGIT %y
      # -------------
      if(tempDtType=='year'){
        if(validatorObject$year[3]=='%y'){
          # importedDatasetMaster@data[,tempDtCol]<<-paste0('20',importedDatasetMaster@data[,tempDtCol])
          importedDatasetMaster[,tempDtCol]<<-paste0('20',importedDatasetMaster[,tempDtCol])
        }
      }
      # -------------
      # -------------

      # create a temp vector for this DT element named as such
      # assign(selectedDateTimeElements[j],importedDatasetMaster@data[,tempDtCol])
      # assign(selectedDateTimeElements[j],importedDatasetMaster[,tempDtCol])
      assign(selectedDateTimeElements[j],importedDatasetMaster[[tempDtCol]])
      
      # remove all the non numeric characters and spaces and sub
      # with commas -- the get command grabs the above variabled named as
      # month day year etc etc
      tempDateDataObjSplt<-gsub("[^0-9.]",',',get(selectedDateTimeElements[j]))
      # split on the commas
      tempDateDataObjSplt<-strsplit(tempDateDataObjSplt,",")
      # remove the empties
      tempDateDataObjSplt<-lapply(tempDateDataObjSplt,function(x){
        x[!x ==""]
      })





      # this generates a vector of T/F checking for invalid data time positions
      # in tempDateDateObjSplt... this helps weed out strange date time elements
      # for example a merged dataset where some time elements have H,M,S and
      # other only have H,M
      naList<-do.call(rbind, lapply(tempDateDataObjSplt, function(x) is.na(x[tempIndexOfLoc])))

      if(any(naList)){
        firstErrorRow<-which(naList)[1]
        # firstErrorRowData<-importedDatasetMaster@data[firstErrorRow,tempDtCol]
        firstErrorRowData<-importedDatasetMaster[firstErrorRow,tempDtCol]
        errorMsg<-paste0('You have date time elements in your data that have a
        different number of elements than you indicated. For example, row
        ',firstErrorRow,' column ',tempDtCol,' = ',firstErrorRowData)
        modalMessager('ERROR',errorMsg)
        dtvRunning<<-FALSE
        progressIndicator('Done importing dates','stop')
        loadingScreenToggle('hide','processing dates')
        # break
        return()
      }

      # assign back to the temp holder just the correct index
      # of that date time element
      assign(selectedDateTimeElements[j],sapply(tempDateDataObjSplt,"[[",tempIndexOfLoc))
      if(j==validatorsLength){
        if(!exists('year')){
          modalMessager('ERROR','no year')
          dtvRunning<<-FALSE
          loadingScreenToggle('hide','processing dates')
          progressIndicator('Done importing dates','stop')
          return()
        }
        if(!exists('month')){
          modalMessager('ERROR','no month')
          dtvRunning<<-FALSE
          loadingScreenToggle('hide','processing dates')
          progressIndicator('Done importing dates','stop')
          return()
        }
        if(!exists('day')){
          modalMessager('ERROR','no day')
          dtvRunning<<-FALSE
          loadingScreenToggle('hide','processing dates')
          progressIndicator('Done importing dates','stop')
          return()
        }
        if(!exists('hour')){
          modalMessager('ERROR','no hour')
          dtvRunning<<-FALSE
          loadingScreenToggle('hide','processing dates')
          progressIndicator('Done importing dates','stop')
          return()
        }
        if(!exists('minute')){
          minute<-"00"
        }
        if(!exists('second')){
          second<-"00"
        }

        # need to check for any decimals in the data
        checkForDec<-function(element,from){          
          element<-as.numeric(element)
          isDecimal<-testInteger(element)
          if(!isDecimal){
            message<-paste0('You have decimals in one of you date elements.
            You need to remove decimals from the data and start over. Decimals were
            found in column ',from,'.')
            modalMessager('ERROR',message)
            dtvRunning<<-FALSE
            progressIndicator('Done importing dates','stop')
            return(FALSE)
          }
        }

        testInteger <- function(x){
          test <- all.equal(x, as.integer(x), check.attributes = FALSE)
          if(test == TRUE){ return(TRUE) }
          else { return(FALSE) }
        }

        checkForDec(year,'year')
        checkForDec(month,'month')
        checkForDec(day,'day')
        checkForDec(hour,'hour')
        checkForDec(minute,'minute')
        if(testInteger(second)){
          oldSec<<-second
          second<<-round(second)
        }

        newDate<<-paste(year,month,day,sep="-")
        newTime<<-paste(hour,minute,second,sep=":")
        if(ampmtime){
          newTime<<-paste0(newTime,' ',ampm)
          rm(ampm)
        }
        newDateTime<<-paste(newDate,newTime,sep=" ")
        combineDateElements(newDateTime)
      }
    }


  }

  combineDateElements<-function(newDateTime){

    endTime<<-Sys.time()
    endTime-startTime

    stringFormat<-"%Y-%m-%d %H:%M:%S"

    if(ampmtime){
      stringFormat<-"%Y-%m-%d %I:%M:%S %p"
    }

    importedDatasetMaster$dateTest<<-newDateTime
    
    importedDatasetMaster$newMasterDate<<-tryCatch({
      as.POSIXct(strptime(
        newDateTime,
        format = stringFormat),
        tz =selectedTimezone
      )},
      error = function(cond) {
        modalMessager('ERROR',cond)
        return()
      },
      warning = function(cond) {
        modalMessager('Warning',cond)
        return()
      }
    )

    
    naDatesLength<<-nrow(importedDatasetMaster[is.na(as.Date(importedDatasetMaster$newMasterDate)),])
    configOptions$naDates<<-NULL
    configOptions$naDatesLength<<-naDatesLength    
    if(naDatesLength==nrow(importedDatasetMaster)){
      modalMessager('ERROR','Your selection of date elements failed.
      Check your selection of date/time elements and try again')
      dtvRunning<<-FALSE
      progressIndicator('Done importing dates','stop')
      return()
    }
    ### if NA dates are produced
    if(naDatesLength>0){
        naDatesObservations<-importedDatasetMaster[is.na(as.Date(importedDatasetMaster$newMasterDate)),]
        modalMessager('Warning',paste0('You had ',naDatesLength,' NA dates in your dataset. These have been saved to your working directory as naDates.csv if you would like to review them.'))        
        importedDatasetMaster<<-importedDatasetMaster[!is.na(as.Date(importedDatasetMaster$newMasterDate)),]
        progressIndicator('Done importing dates','stop')
        createUniqueIdsHanlder()
      } else{
        progressIndicator('Done importing dates','stop')
        createUniqueIdsHanlder()
      }
  }


  createUniqueIdsHanlder<-function(){
    progressIndicator('Creating unique IDs','start')
    progressIndicator('Success','stop')
    movementParamsProcessing();
  }


##------------------ lookup for R tech values and literal values
dateTimesList<-list(
  '%H'='Hour (1-24)',
  '%I'='Hour (1-12)',
  '%p'='AM/PM',
  '%M'='Minute (1-60)',
  '%S'='Second (0-60)',
  '%m'='Month (number: 1-12)',
  '%d'='Day (number: 1-31)',
  '%y'='Year (two digits: 17)',
  '%Y'='Year (four digits: 2017)',
  '%mon'='Month (abbreviated: Jan)',
  '%month'='Month (full name: January)'
  )

  ##------------------ reverse lookup
  dateTimesListFlipped<-list(
    'Hour (1-24)'='%H',
    'Hour (1-12)'='%I',
    'AM/PM'='%p',
    'Minute (1-60)'='%M',
    'Second (0-60)'='%S',
    'Month (number: 1-12)'='%m',
    'Day (number: 1-31)'='%d',
    'Year (two digits: 17)'='%y',
    'Year (four digits: 2017)'='%Y'

    )



possibleTimeSeperators<-c(':',' ')
possibleDateSeperators<-c('/','-','\\',' ','No Seperator')

dates<<-c('month','day','year')
times<<-c('hour','minute','second')



dateTimeLookup<<-list(
  '%H'='hour',
  '%I'='hour',
  '%M'='minute',
  '%S'='second',
  '%m'='month',
  '%d'='day',
  '%Y'='year',
  '%y'='year',
  '%p'='am/pm'

  )

validatorValues<<-c('year','month','day','hour','minute','second')
selectedTimezone<<-"GMT"
ampmtime<<-FALSE
