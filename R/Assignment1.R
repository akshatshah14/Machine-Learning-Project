#question 1


readfiles<- function(id = 1:332,directory) {
  if(!length(grep(directory,getwd()))>0)
  {
    setwd(file.path(getwd(), directory))
  }

  for (i in id)
  {
    if (i <10) {
      data <- read.csv(paste("0","0", as.character(i), ".csv", sep=""),  header = T, na.strings=c("NA","NaN", " "))

    }
    else if (i>=10 & i<100) {
      data <- read.csv(paste("0", as.character(i), ".csv", sep=""),  header = T, na.strings=c("NA","NaN", " "))

    }

    else       {
      data <- read.csv(paste(as.character(i), ".csv", sep=""),header = T, na.strings=c("NA","NaN", " "))
    }
    return(data)
  }
}




myfunction <-function(data,pollutant)
{

  if(tolower(pollutant)=="nitrate"){

    result_nitrate = mean(data$nitrate, na.rm = TRUE)
    return(result_nitrate)
  }

  else if(tolower(pollutant) == "sulfate"){
    result_sulfate = mean(data$sulfate, na.rm = TRUE)
    return(result_sulfate)
  }

}

#' @export
mean_function <- function(directory){
  Path <- readline(prompt = "Enter the path of the folder specdata: ")
  setwd(Path)
  monitor <- as.integer(readline(prompt="Enter monitor : "))
  pollutant = readline(prompt = "Enter the pollutant has to be either nitrate or sulfate: ")

  if(monitor>=1 && monitor<=332){
    data = readfiles(monitor,directory)
    if(tolower(pollutant)=="sulfate" || tolower(pollutant)=="nitrate"){
      mean_of_pollutant=myfunction(data, pollutant)
      print(mean_of_pollutant)
    }else {
      print ("Pollutant has to be either sulfate or nitrate")
    }

  } else {
    print("Monitor out of range")
  }
}


#Question 2


#' @export
parentfunction <- function(directory)
{
  Path <- readline(prompt = "Enter the path of the folder specdata: ")
  setwd(Path)
  if(!length(grep(directory,getwd()))>0)
  {
    setwd(file.path(getwd(), directory))
  }
  threshold = readline(prompt = "Enter the threshold: ")
  threshold = as.integer(threshold)

  correlation_cal <- function(filename)
  {
    data =read.csv(file.path(getwd(),filename))
    data = data[complete.cases(data),]
    no_rows=nrow(data)
    if(no_rows > threshold)
    {

      correlation =cor(data$sulfate,data$nitrate,use="complete.obs")
      return (correlation)
    }

  }

  a=lapply(list.files(getwd()),correlation_cal)
  a=unlist(a)
  print(a)
  if(is.null(a))
  {
    x <- vector(mode="numeric", length=0)
    x
  }
}

#Question 3


#' @export
plotting <- function(directory)
{
  Path <- readline(prompt = "Enter the path of the folder specdata: ")
  setwd(Path)
  if(!length(grep(directory,getwd()))>0)
  {
    setwd(file.path(getwd(), directory))
  }
  threshold = readline(prompt = "Enter the threshold: ")
  threshold = as.integer(threshold)

  plot_function <- function(filename)
  {
    data =read.csv(file.path(getwd(),filename))
    data = data[complete.cases(data),]
    no_rows=nrow(data)

    if(no_rows > threshold)
    {
      return(filename)
    }

  }

  a=lapply(list.files(getwd()),plot_function)
  a=unlist(a)
  if(!is.null(a))
  {
    print(a)
    read_file = sample(a,1)
    print("File that has been picked up")
    print(read_file)
    plot_file_name = read.csv(read_file)
    plot_file_name=plot_file_name[complete.cases(plot_file_name),]
    plot(plot_file_name$sulfate, plot_file_name$nitrate)

  }
}


