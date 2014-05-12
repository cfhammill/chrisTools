#En 2022 tomorrow


#Directly import flow cytometry files
#takes .csv for pre-extracted data tables
#I have yet to implement behaviour for .fcs but stay tuned


#' Import Flow Cytometry Files (.csv)
#'
#' Imports one or many .csv files containing flow cytometry data
#' @param fileNames The .csv files you wish to import
#' @keywords import flow
#' @export
#' @examples
#' importFlows()
#' 
importFlows <- function(fileNames){
  
  fileNames <- sub(".csv","",fileNames,ignore.case=T)
  filesExtended <- paste(fileNames,".csv",sep="")
  
  if(length(fileNames) == 1)
  {
    
    flows <- read.csv(filesExtended)
    attr(flows,"class") <- c("flow","data.frame")
    
  } else {
    
    flows <- lapply(seq(along=fileNames), function(i){
      temp <- read.csv(filesExtended[i])
      attr(temp,"class") <- c("flow","data.frame")
      temp
    })
    
    attr(flows,"class") <- c("flowList","list")
    
    names(flows) <- fileNames
    
  }

  flows
}

#Pass each flow to plot flow
plot.flowList <- function(f, ...){
  par(ask=T)
  lapply(f, plot, ...)
  par(ask=F)
}

#Parent plot method for flow files
#Can take a wide array of inputs, and generate 3 distinct plots (so far)
#Allows for various default axis labels and plot titles to be generated

plot.flow <- function(flow, j=2, type="density", main=NULL, 
                      xlab=NULL, ylab=NULL, col=NULL, ...){
  
  #Keep track of original flow file name
  name <- deparse(substitute(flow))
  
  if(!is.null(col) && col == "cluster"){
    col <- as.numeric(getClusters(flow))
  } else if(is.null(col)){
    col=1
  }
  
  #Check columns to plot, determine plot type
  if(length(j) >= 3 || type=="m" || type=="matrix"){
    
    if(length(j) >= 3){
      plot.flow.matrix(flow[,j], main=main, name=name,col=col, ...)
      } else {
      plot.flow.matrix(flow,main=main, name=name,col=col, ...)
    }                                
    
  } else if(length(j) == 2){
    
    #Error checking
    if(type != "density"){print("Type specification in 2d plots not implemented")
                          return(invisible(NULL))}
    
    #Pass to scatter plot sub-method
    plot.flow.scatter2d(flow,j=j, 
                        main=main, xlab=xlab,
                        ylab=ylab, name=name,
                        col=col, ... )
    
  } else if(length(j) == 1){
    #Allow choices of density and histogram, case insensitivity built in
    #passes to histogram and density sub-methods
    switch(tolower(type),
           
           d=,density = plot.flow.density(flow,
                                          j=j, main=main, xlab=xlab,
                                          ylab=ylab, name=name, ...),
           
           h=, histogram= plot.flow.histogram(flow,
                                              j=j, main=main, xlab=xlab,
                                              ylab=ylab, name=name,...),
           
           b=, bhattacharyya= plot.flow.bhattacharyya(flow,
                                                      j=j, main=main, xlab=xlab,
                                                      ylab=ylab, name=name,...),
           
{print("This is not a valid plot type"); return(invisible(NULL))}) #printed error message
    
  }
}

#Scatter Plot Matrix

plot.flow.matrix <- function(flow, j, main, name, col,...){ 
  par(mfcol=rep(length(flow[1,]),2))

  sapply(seq(along=flow[1,]), function(i){
   sapply(seq(along=flow[1,]), function(j){
     plot(flow, j=c(i,j), col=col)
   })
  })
  
  par(mfcol=c(1,1))
}

#Plot density, if main or ylab passed as nulls, compute defaults

plot.flow.density <- function(flow, j, main, xlab, ylab, name,...){
  
  if(is.null(main)){
    main <- paste("Density Plot of", names(flow)[j], "for", name)
  }
  
  if(is.null(ylab)){
    ylab= "Density"
  }
  
  plot(density(flow[,j]),main=main,xlab=xlab,ylab=ylab,...)
}

#Plot histogram, if main or ylab passed as nulls compute defaults

plot.flow.histogram <- function(flow, j, main, xlab, ylab, name,...){
  
  if(is.null(main)){
    main <- paste("Histogram of", names(flow)[j], "for", name)
  }
  
  if(is.null(ylab)){
    ylab= "Probability"
  }
  
  hist(flow[,j],main=main,ylab=ylab,xlab=xlab,prob=T,...)
}

plot.flow.bhattacharyya <-  function(flow, j, main, xlab, ylab, name,...){
  
  if(is.null(main)){
    main <- paste("1st Derivative Normal Plot of", names(flow)[j], "for", name)
  }
  
  if(is.null(ylab)){
    ylab= "delta Density"
  }
  
  d <- density(flow[,j])
  
  d2 <- d$y[-1]
  d1 <- d$y[-(length(d2)+1)]
  
  d$x <- d$x[-1]
  d$y <- d2 - d1
                
  plot(d, main=main,xlab=xlab,ylab=ylab,...)
}


#Plot a 2d scatter plot if xlab, ylab, or main passed as null compute defaults

plot.flow.scatter2d <- function(flow, j, main, xlab, ylab, name, pch=".", ...){
  if(is.null(main)){
    main <- paste("Scatter Plot of", names(flow[j[1]]), "vs.", names(flow)[j[2]], "for", name)
  }
  
  if(is.null(xlab)){
    xlab <- names(flow)[j[1]]
  }

  if(is.null(ylab)){
    ylab <- names(flow)[j[2]]
  }

  plot(x = flow[,j[1]], y = flow[,j[2]], main=main, xlab=xlab, ylab=ylab,pch=pch, ...)
}




#Extract one element from a flowList  
"[[.flowList" <- function(fl,i){
  attr(fl,"class") <- "list"
  flow <- fl[[i]]
  attr(flow,"class") <- c("flow","data.frame")
  flow
}

#Extract by name from flowList
"$.flowList" <- function(fl,i){
  attr(fl,"class") <- "list"
  flow <- fl[[i]]
  attr(flow,"class") <- c("flow","data.frame")
  flow
}

#Extract a sub-flowList from a flowList
"[.flowList" <- function(fl,i){
    attr(fl,"class") <- "list"
    flow <- fl[i]
    attr(flow,"class") <- c("flowList","list")
    flow
}

#Subset a flow file with matrix-esque notation
"[.flow" <- function(f,...)
{
  attr(f,"class") <- "data.frame"
  
  flow <- f[...] 
  
  if(is.data.frame(flow)){
    attr(flow,"class") <- c("flow","data.frame")
  }
  
  flow
}


#Generic trim function
trim <- function(f,...){UseMethod("trim",f)}

#Flow list trim function, just passes args and flows into the flow version
trim.flowList <- function(f, ...){
  f <- lapply(f, trim, ...)
  attr(f, "class") <- c("flowList","list")
  f
}

#Perform various trimming operations on a flow set
#similar to gating idea of regular flow stuff
trim.flow <- function(f, j, min=NULL, max=NULL, percentile = NULL, n = NULL, ...){
  
  nb <- !is.null(n)
  mn <- !is.null(min); mx <- !is.null(max); per <- !is.null(percentile)
  qmin <- numeric(0); qmax <- numeric(0)
  
  #If n is specified ignore everything else, take middle n
  if(nb){
    
    bot <- (length(f[,j]) - n)/2
    top <- length(f[,j]) - ceiling(bot) - 1
    bot <- floor(bot)
    
    return(f[bot:top,])
  }
  
  #If percentile specified standardize to a fraction of 1
  if(per){
    if(percentile >= 1) percentile <- percentile / 100 
  }
  
  #No args error catch
  if(!mn & !mx & !per){
    print("Please specify some trimming parameters"); return(invisible(NULL))
  }
  
  #Just minimum
  if(mn & !mx & !per){
    return(f[f[,j] > min,])
  }
  
  #Just maximum
  if(!mn & mx & !per){
    return(f[f[,j] < max])
  }
  
  #Min and max
  if(mn & mx & !per){
    return(f[f[,j] < max & f[,j] > min,])
  }
  
  #Just percentile
  if(!mn & !mx & per){
    qmin <- quantile(f[,j], probs= (1-percentile)/2)
    qmax <- quantile(f[,j], probs= 1 - (1-percentile)/2)
    return(f[f[,j] > qmin & f[,j] < qmax,])
  }
  
  #Min and percentile
  if(mn & !mx & per){
     f <- f[f[,j] > min,]
     qmin <- quantile(f[,j], probs= (1-percentile)/2)
     qmax <- quantile(f[,j], probs= 1 - (1-percentile)/2)
     return(f[f[,j] > qmin & f[,j] < qmax,])
  }
  
  #Max and percentile
  if(!mn & mx & per){
    f <- f[f[,j] < max,]
    qmin <- quantile(f[,j], probs= (1-percentile)/2)
    qmax <- quantile(f[,j], probs= 1 - (1-percentile)/2)
    return(f[f[,j] > qmin & f[,j] < qmax,])
  }
  
  #The whole Shebang
  if(mn & mx & per){
    f <- f[f[,j] > min & f[,j] < max,]
    qmin <- quantile(f[,j], probs= (1-percentile)/2)
    qmax <- quantile(f[,j], probs= 1 - (1-percentile)/2)
    return(f[f[,j] > qmin & f[,j] < qmax,])
  }
}


#cluster analysis generic
cluster <- function(f, ...){UseMethod("cluster",f)}

#cluster one flow, takes options for Kmeans - for now
cluster.flow <- function(f, ..., j=1:length(f), type="kmeans", append=F){
   clus <- switch(tolower(type),
         k=,kmeans= kmeans(f[,j], ...),
         {print("this clustering method not initialized"); return(f)})
   
   if(append){
     f <- cbind(f,clus$cluster)
     names(f)[length(f)] <- "cluster"
   }
   
   attr(f, "class") <- c("flow", "data.frame")
   attr(f, "clusters") <- clus
   return(f)
}

getClusterObject <- function(f){UseMethod("getClusterObject", f)}

getClusterObject.flow <- function(f){attr(f, "clusters")}

getClusterObject.flowList <- function(f){lapply(f, getClusterObject)}

getClusters <- function(f){UseMethod("getClusters", f)}

getClusters.flow <- function(f){getClusterObject(f)$cluster}

getClusters.flowList <- function(f){sapply(getClusterObject(f), "[[", "cluster")}

cluster.flowList <- function(f, ..., type="kmeans"){
  f <- lapply(f, cluster, ..., type=type)
  attr(f, "class") <- c("flowList","list")
  f
}

flowMacro <- function(operations=NULL, variable=NULL){
  
  if(is.null(operations)){
    f <- function(x){}
    attr(f, "class") <- c("flowMacro", "function")
    return(f)
  }
  
  operations <- paste(operations[!operations==""], collapse="; ")
  
  if(!is.null(variable)){operations <- gsub(variable, "x", operations)}
  
  f <- function(x){}
  exps <- parse(text=operations)
  b <- as.call(c(as.name("{"), exps))
  
  body(f) <- b
  
  attr(f, "class") <- c("flowMacro", "function")

  return(f)
}

addOperations <- function(macro, operations, variable=NULL, position=NULL){
  
  newOps <- paste(operations, collapse=";")
  newOps <- strsplit(newOps, "[;\n]", perl="t")[[1]]

  if(!is.null(variable)){newOps <- gsub(variable,"x", newOps)}
  
  oldOps <- deparse(body(macro))
  oldOps <- oldOps[-c(1,length(oldOps))]
  
  ops <- if(is.null(position) || position > length(oldOps)){
    c(oldOps,newOps)
  } else {
    c(oldOps[1:(position-1)], newOps, oldOps[position:length(oldOps)])
  }
  
  ops <- paste(ops[!ops==""], collapse="; ")
  
  f <- function(x){}
  
  exps <- parse(text=ops)
  exps <- as.call(c(as.name("{"), exps))
  
  body(f) <- exps
  
  attr(f, "class") <- c("flowMacro", "function")
  return(f)
}

removeOperations <- function(macro, positions){
  
  oldOps <- deparse(body(macro))
  oldOps <- oldOps[-c(1,length(oldOps))]
  
  ops <- oldOps[-positions]
  
  f <- function(x){}
  
  exps <- parse(text=ops)
  exps <- as.call(c(as.name("{"), exps))
  
  body(f) <- exps
  
  attr(f, "class") <- c("flowMacro", "function")
  return(f)
}

editOperations <- function(macro, operations, position){
  newOps <- paste(operations, collapse=";")
  newOps <- strsplit(newOps, "[;\n]", perl="t")[[1]]
  f <- macro
  
  if(length(newOps) != length(position) & length(position) != 1){
    
    print("Invalid replacement or position length"); return(macro)
    
  } else if(length(newOps) == length(position)){
    
    sapply(seq(along=position), function(i){
      f <<- addOperations(removeOperations(f, position[i]), newOps[i], position=position[i])
    })
    
  } else if(length(position) == 1){
    
    f <- addOperations(removeOperations(f, position), newOps, position)
    
  }
  
  return(f)
}

editMacro <- function(m){
  edit(m)
}

edit.flowMacro <- function(m){
  attr(m, "class") <- "function"
  m <- edit(m)
  attr(m, "class") <- c("flowMacro", "function")
  return(m)
}
