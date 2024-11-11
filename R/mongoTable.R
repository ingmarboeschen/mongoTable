#' mongoTable
#' 
#' Function to create one and two dimensional frequency tables from a MongoDB connection.
#' @importFrom mongolite mongo
#' @param connection character. A mongo connection object initiated with mongolite::mongo().
#' @param x character. A field variable for which frequencies should be counted.
#' @param y character. An optional second field variable for which frequencies should be counted.
#' @param query character. An optional MongoDB query for data subset selection (e.g.: '\{\"year\": 2024\}').
#' @param sort logical. If TRUE, the output is sorted by frequency. 
#' @param decreasing logical. If TRUE and sort==TRUE, the output is returned with decreasing frequencies. If TRUE and sort==FALSE, level names are returned in decreasing manner. 
#' @param limit integer. Defines the maximum length/dimensions of output. 
#' @param lowerize logical. All levels in one dimensional tables will be lowerized.
#' @return A one or two dimensional frequency table.
#' @keywords MongoDB frequency table
#' @export
#' @examples 
#' # use mongolite::mongo() to connect to a MongoDB instance (demo server)
#' mon <- mongolite::mongo("mtcars", url =
#' "mongodb+srv://readwrite:test@cluster0-84vdt.mongodb.net/test")
#' if(mon$count() > 0) mon$drop()
#' mon$insert(mtcars)
#' stopifnot(mon$count() == nrow(mtcars))
#' 
#' ############################################
#' ## Create a one dimensional frequency table 
#' # for all x
#' mongoTable(connection = "mon", x = "cyl")
#' 
#' # create a one dimensional frequency table for all x matching a query
#' mongoTable(connection="mon", x="cyl", query = '{\"mpg\": {\"$gt": 20}}')
#' 
#' ############################################
#' ## Create a two dimensional frequency table
#' 
#' # for all x and y
#' mongoTable(con = "mon", x = "cyl", y = "gear")
#' 
#' # for all x and y matching a query
#' mongoTable(con="mon", x = "cyl", y = "gear", query = '{\"mpg\": {\"$gt": 20}}')

###############################################################
mongoTable<-function(connection,x,y=NULL,query="{}",
                      lowerize=FALSE,limit=NULL,sort=FALSE,decreasing=TRUE){

  if(!exists(connection)) return(message(paste0("Connection '",connection,"' not found. You must initialize a connection to MongoDB with mongolite::mongo().")))
  # 1 dimensional table
  if(is.null(y)){
    tab<-eval(parse(text=connection))$aggregate(paste0('[',
                                                       ifelse(query!="{}",paste0('{"$match" : ',query,'}, '),""),
                                                       '{ "$unwind": "$',x,'" },',
                                                       ifelse(lowerize==TRUE,paste0('{"$project": {"',x,'": {"$toLower": "$',x,'"}}},'),''),
                                                       '{ "$group": {"_id": "$',x,'","count": { "$sum": 1 }}}]'),
                                                options = '{"allowDiskUse" : true}')
    
    if(dim(tab)[1]!=0&dim(tab)[2]!=0){
      # create table
      n<-tab[,1]
      tab<-tab[,2]
      names(tab)<-n
      tab<-as.table(tab)
      tab<-tab[which(names(tab)!="list()")]
      # sort names if all are numeric
      suppressWarnings(
        isNum<-sum(names(tab)==as.numeric(names(tab)),na.rm=TRUE)==length(names(tab)))
      if(isNum) tab<-tab[order(as.numeric(names(tab),decreasing=decreasing))]
      # sort x levels by name if is not numeric
      if(!isNum&sort==FALSE) tab<-tab[order(names(tab),decreasing=decreasing)]
      if(sort==TRUE) tab<-sort(tab,decreasing=decreasing)
      # reduce to limit of most frequent detections
      if(decreasing&!is.null(limit)) if(limit<length(tab)) tab<-tab[1:limit]
      if(!decreasing&!is.null(limit)) if(limit<length(tab)) tab<-tab[length(tab):(length(tab)-limit+1)]
      # convert to table and set dimname 
      tab<-as.table(tab)
      names(dimnames(tab))<-x
      
    }else tab<-NULL
    
    return(tab)
    
  }# end 1 dimensional table
  
  #######################
  # 2 dimensional table  
  if(!is.null(y)){
  tab<-eval(parse(text=connection))$aggregate(paste0('[',
                            ifelse(query!="{}",paste0('{"$match" : ',query,'}, '),""),
                            '{ "$group": {"_id": {',
                            paste0('"',x,'": "$',x,'",'),
                            paste0('"',gsub(".*\\.","",y),'": "$',y,'"'),
                            '}, "count": { "$sum": 1 } }} ]')
  )
  
  
  # return NULL if there's no match
  if(is.null(unlist(tab))) return(NULL)
  # return NULL if both fields are missing in data  
  if(ncol(tab[[1]])==0){
    return(NULL)
  }
  # extract values and levels 
  values<-unlist(tab[[2]])
  xLevel<-factor(unlist(tab[[1]][,1]))
  xLevels<-levels((xLevel))
  
  # return 1 dimensional table if there are only values in x or y
  if(ncol(tab[[1]])==1){
   warning("One of the two field variables is not present within the selected data.")
    return(NULL) 
#    return(cbind(tab[,1],count=tab[,2]))
    }
  
  yLevel<-tab[[1]][,2]
  yLevels<-unique(unlist(yLevel))
  
  # return table if only matches in x or y
  if(length(xLevels)==1|length(yLevels)==1) return(cbind(tab[,1],count=tab[,2]))
  
  # convert from list to vectors
  tim<-unlist(lapply(yLevel,length))
  values<-rep(values,times=tim)
  xLevel<-rep(xLevel,times=tim)
  yLevel<-unlist(yLevel)
  
  # create frequency matrix
    res<-matrix(0,ncol=length(yLevels),nrow=length(xLevels))
    for(j in 1:length(xLevels)){
      for(i in 1:length(yLevels)){
        ind<-grep(paste0("^",gsub("[\\(\\)]","",gsub("\\+","[+]",yLevels[i])),"$"),gsub("[\\(\\)]","",yLevel[xLevel==xLevels[j]]))
        if(length(ind)>0) res[j,i]<-sum(values[xLevel==xLevels[j]][ind])
      }
      }
    
    colnames(res)<-yLevels
    rownames(res)<-xLevels
    
    # sort col names if all are numeric
    if(ncol(res)>1){
    suppressWarnings(
      isNum<-sum(colnames(res)==as.numeric(colnames(res)),na.rm=TRUE)==length(colnames(res)))
    if(isNum) res<-res[,order(as.numeric(colnames(res)))]
    }
    
    # sort by frequency
    if(sort==TRUE) res<-res[order(rowSums(res),decreasing=decreasing),]
    if(sort==TRUE) res<-res[,order(colSums(res),decreasing=decreasing)]

    # reduce to limited table
    if(!is.null(limit)) if(limit<nrow(res)) res<-res[1:limit,]
    if(!is.null(limit)) if(limit<ncol(res)) res<-res[,1:limit]
    
    # convert to table with dimnames
    res<-as.table(res)
    dimnames(res)<-list(x = rownames(res),
                        y = colnames(res))
    names(dimnames(res))<-c(x,y)

    return(res)
  }
}

