library(data.table)

saveLastLoadedFile = function(inputFile1DataPath){
  x = data.table("name" = 'lastLoadedFile', "value" = inputFile1DataPath)
  write.csv(x, file = getConfigFilePath())
}

getLastLoadedFile = function(){
  #tryCatch()
  x = read.csv(getConfigFilePath())
  x1 = data.table(x)
  x1[name == 'lastLoadedFile',]$value
}

getConfigFilePath = function(){
  configFileName = ".vitaminator.csv"
  userHome = Sys.getenv("HOME")
  paste(userHome, configFileName, sep ="/")
}