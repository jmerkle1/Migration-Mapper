updateTable<-function(tableName,column,where,value){

  if(!exists('dbConnection')){
    dbConnection <<- dbConnect(RSQLite::SQLite(), paste0(masterWorkingDirectory,'//workingDb.db'))
  }

  thisUpdateQuery<<-paste('UPDATE',tableName,'SET',column,'=',value,where,sep=' ')
  print(thisUpdateQuery)
  thisSend<-dbSendQuery(dbConnection,thisUpdateQuery)
  dbClearResult(thisSend)


}


updateMasterTableFromDatabase<-function(){
  dbResults<-data.frame(dbGetQuery(dbConnection, "SELECT * FROM importedDatasetMaster"))  
  importedDatasetMaster$comments<<-dbResults$comments
  importedDatasetMaster$problem<<-dbResults$problem
  importedDatasetMaster$mortality<<-dbResults$mortality

}
