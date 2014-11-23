"importDefaults" <-
function(...) {
  if(is.null(sys.call(-1))) 
    stop("importDefaults is only valid inside a function call") 
  calling.fun <- as.character(match.call(call=as.call(sys.call(-1)))[1])
  all.defaults <- getDefaults(calling.fun)
  envir <- as.environment(-1)
  #passed.args <- names(sapply(match.call(call=as.call(sys.call(-1)))[-1],deparse))
  passed.args <- names(as.list(match.call(
                       definition=eval(parse(text=calling.fun)),
                       call=as.call(sys.call(-1)))))[-1]
  formal.args <- names(formals(as.character(sys.call(-1))))
  default.args <- names(which(sapply(all.defaults,function(x) !is.null(x))==TRUE))
  for(arg in formal.args) {
    if(!arg %in% passed.args) {
      if(arg %in% default.args) {
        if(typeof(all.defaults[arg])=='list') {
          assign(arg, as.vector(all.defaults[arg][[1]]),envir=envir)
        } else {
          assign(arg, as.vector(unlist(all.defaults[arg][[1]])),envir=envir)
        }
      }
    }
  }
}
".importDefaults" <- importDefaults
"useDefaults" <-
function(name)
{
  env <- as.environment(-1)
  if(is.function(name)) name <- deparse(substitute(name))
  if(!is.function(eval(parse(text=name))))
    stop("argument must be a function")
  # If function already makes use of 'importDefaults()' skip...
  if(identical(grep('importDefaults()',
     deparse(body(get(name,env=globalenv())))),integer(0))) {
  if(class(body(name)) == "{") {
    new.fun.body <- as.call(parse(text=
                    c(deparse(body(name))[1],
                    '.importDefaults()',
                    deparse(body(name))[-1])))[[1]]
  } else {  # functions lacking enclosures
    new.fun.body <- as.call(parse(text=
                    c("{",
                    '.importDefaults()',
                    deparse(body(name)),"}")))[[1]]
  }
  if(exists(name,env,inherits=FALSE))
    assign(paste('.',name,'.orig',sep=''),get(name,env),env)
  assign(name,as.function(c(formals(name),new.fun.body)),env)
  } 
}

"unDefaults" <-
function(name)
{
  if(!identical(grep('\\.importDefaults()',deparse(body(name))),
               integer(0))) {
    env <- as.environment(-1)
    if(is.function(name)) name <- deparse(substitute(name))
    if(exists(paste('.',name,'.orig',sep=''),env,inherits=FALSE)) {
      assign(name,get(paste('.',name,'.orig',sep=''),env),env)
      remove(list=paste('.',name,'.orig',sep=''),envir=env)
    } else {
      remove(list=name,envir=env)
    }
  } else { 
    stop(paste(dQuote("useDefaults"),"not set for",substitute(name)))
  }
}

"setDefaults" <-
function(name,...) {
  if(is.function(name)) 
    name <- deparse(substitute(name))
  default.name <- paste(name,"Default",sep=".")
  all.defaults <- getDefaults(name)
  if(is.null(all.defaults)) {
    all.defaults <- sapply(names(formals(name)),
                           function(x) x=NULL)
  }
  all.defaults <- as.list(all.defaults)
  new.defaults <- list(...)
  for(arg.name in names(all.defaults)) {
    if(arg.name %in% names(new.defaults)) 
      if(!is.null(new.defaults[[arg.name]])) {
        all.defaults[[arg.name]] <- new.defaults[[arg.name]]
      } else {
        all.defaults[[arg.name]] <- NULL
      }
  }
  if(length(all.defaults)==0) {
    unsetDefaults(name,confirm=FALSE)
  } else {
    env <- as.environment(-1)
    eval(parse(text=paste('options(',default.name,'=list(',
         paste(paste(names(all.defaults),'=',
         lapply(all.defaults,function(x) {
           if(is.character(x)) {
             deparse(x)
           } else {
             x
           }})),collapse=','),'))',
         sep='')),envir=env)
  }
}

"unsetDefaults" <-
function(name,confirm=TRUE) {
  importDefaults()
  if(is.function(name)) name <- deparse(substitute(name))
  if(is.null(getDefaults(name))) 
    stop(paste("no Defaults set for",sQuote(name)))
  remove.yes <- TRUE
  if(confirm) {
    CONFIRMATION <- readline(prompt=
            paste("Are you sure you want to remove",
                  sQuote(name),"defaults? (N): "))
    if(toupper(substr(CONFIRMATION,1,1))!="Y") {
      remove.yes <- FALSE
      cat(paste(sQuote(name),"Defaults NOT removed\n"))
    } else {
      if(confirm)
        cat(paste(sQuote(name),"Defaults removed!\n"))
    }
  }
  if(remove.yes) {
    default.name <- paste(name,"Default",sep=".")
    env <- as.environment(-1)
    eval(parse(text=paste('options(',default.name,'=NULL)',sep='')),envir=env)
  }
}
"getDefaults" <-
function(name=NULL,arg=NULL) {
  if(is.function(name)) name <- deparse(substitute(name))
  if(!is.null(name)) {
    if(length(name) > 1) {
      if(!is.character(name))
        stop(paste(sQuote('name'),"must be a character vector",
                   "or visible function")) 
      all.names=list()
    }
    for(each.name in name) {
      default.name <- paste(each.name,"Default",sep=".")
      if(is.null(arg)) {
        if(exists('all.names',inherit=FALSE)) {
          all.names[[each.name]] <- options(default.name)[[1]]
        } else {
          return(options(default.name)[[1]])
        }
      } else {
        default.list <- list()
        for(each.arg in arg) {
          default.list[[each.arg]] <- options(default.name)[[1]][[each.arg]]
        }
        if(exists('all.names',inherit=FALSE)) {
          all.names[[each.name]] <- default.list
        } else {
          return(default.list)
        }
      }
    }
    return(all.names)
  } else {
    all.options <- names(options())
    all.Defaults <-as.character(
                     sapply(all.options[grep('.Default',all.options)],
                       FUN=function(x) {
                         strsplit(x,'\\.')[[1]][[1]]
                       })
                   )
    if(identical(all.Defaults,character(0))) return(NULL)
    return(all.Defaults)
  }
}
