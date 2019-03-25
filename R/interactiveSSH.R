#  interactiveSSH - sending shell commands over ssh
#  Copyright (C) 2019  Georg Schnabel
#  
#  interactiveSSH is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.
#  
#  interactiveSSH is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#  
#  You should have received a copy of the GNU General Public License
#  along with this program.  If not, see <https://www.gnu.org/licenses/>

#' Initiate interactive SSH session
#'
#' @param login login in the form user@host
#' @param password password of ssh connection
#' @param pwfile file with password in the first line (overrides parameter \code{password})
#' @param port ssh server port
#' @param share enable connection multiplexing (e.g. in combination with rsync)
#' @param tempdir.loc directory to store all connection related files
#' @param verbosity level of verbosity of information messages
#' @param timeout.cmd maximum allowed duration of a command to finish
#' @param timeout.con maximum allowed duration to establish connection
#' @param PS1 desired bash prompt (this string should not appear in the output of commands)
#' @param regexPS1 regular expression to capture the bash prompt (can equal \code{PS1} if only alphanumeric characters)
#'
#' @details
#' The list returned by this function contains the following functions:
#' \tabular{ll}{
#' \code{execBash(cmds)} \tab execute the commands given in character vector \code{cmds} and return the results
#' as a list of character vectors\cr
#' \code{send(cmds)} \tab send the commands given in \code{cmds}\cr
#' \code{read} \tab read the output of the ssh connection (attention, may also contain the send commands) \cr
#' \code{readRaw} \tab read the raw output (do split output string using RETURN and LINEFEED)\cr
#' \code{sendFile(src,dest,\cr chunk.size=1024)}\tab transmit a file over the ssh connection (very slow, around kbyte/sec)\cr
#' \code{getSocketFile()}\tab return the full path to the socket file (only useful if \code{share=TRUE})\cr
#' \code{getPwfile()}\tab return the full path to the password file\cr
#' \code{closeCon()}\tab close the connection and clean up temporary files\cr
#' }
#' @return A list (think object) with functions to execute bash commands remotely, see \code{details}
#' @note
#' \itemize{
#'   \item Do not forget to close the connection afterwards with \code{closeCon()}
#'   \item If \code{PS1} is NULL, the bash prompt will not be changed and the function \code{execBash} will not be available.
#' }
#' @export
initInteractiveSSH <- function(login,password=NULL,pwfile=NULL,
                               port=22, share=FALSE,
                               tempdir.loc, verbosity=1,
                               timeout.cmd=20, timeout.con=20,
                               PS1="autoPrompt>+>",
                               regexPS1 = NULL) {



  # assertions
  stopifnot(is.null(password) || (length(password)==1 && is.character(password)),
            is.null(pwfile) || (length(pwfile)==1 && is.character(pwfile)),
            is.character(password) || is.null(password),
            is.character(pwfile) || is.null(pwfile),
            is.null(pwfile) || file.exists(pwfile))

  # defaults
  defaults <- list(timeout.cmd=timeout.cmd,
                   timeout.con=timeout.con,
                   verbosity=verbosity)

  # global vars
  logfile <- NULL
  inPipe <- NULL
  outPipe <- NULL
  inpHnd <- NULL
  outHnd <- NULL
  tempPwfile <- NULL
  socketFile <- NULL

  # internal functions

  loadDefaults <- function() {
    parfrm <- parent.frame()
    nm <- names(defaults)
    for (i in seq_along(nm))
      if (is.null(parfrm[[ nm[i] ]]))
        assign(nm[i],defaults[[i]],parfrm)
  }

  timedOut <- function(start,delay) {
    elTime <- difftime(Sys.time(),start,units="secs")
    elTime > delay 
  }
   
  # just a semantically meaningful acronym for timedOut
  waitedMinInt <- function(start,delay) {
    timedOut(start,delay)
  }

  printInfo <- function(msg,verb) {
    loadDefaults()
    if (verb<=verbosity)
      cat(paste0(msg,"\n"))
  }


  getPwfile <- function() {
    tempPwfile
  }

  getSocketFile <- function() {
    socketFile
  }

  readRaw <- function () {
    tryCatch(rawToChar(readBin(outHnd,what="raw",n=16384,size=1)),
                    error = function(e) character(0))
  }

  sendRaw <- function(str) {
    writeBin(str, inpHnd, size=1)
    flush(inpHnd)
  }

  send <- function(str) {
    sendRaw(paste0(paste0(str,collapse="\n"),"\n"))
  }

  read <- function() {
    res <- strsplit(readRaw(),"\r?\n")
    if (length(res)>0)
      res[[1]]
    else
      character(0)
  }

  readAndWait <- function(minwait=0.1, maxwait=10) {
    loadDefaults()
    answer <- character(0)
    start <- Sys.time()
    while (!timedOut(start,maxwait) &&
           (!waitedMinInt(start,minwait) || length(answer)==0)) {
      Sys.sleep(0.1)
      newData <- read()
      if (length(newData)>0) {
        answer <- c(answer,newData)
        start <- Sys.time()
      }
    }
    answer
  }

 
  execBashBasic <- function(cmdstr, timeout.cmd=NULL) {

    loadDefaults()
    readRaw()
    send("")
    ret <- list()
    res <- NULL
    post <- character(0)
    start <- Sys.time()
    for (cmdIdx in seq_along(cmdstr)) {
      curCmd <- cmdstr[cmdIdx]
      curRet <- character(0)
      stopifnot(!grepl('\n',curCmd,fixed=TRUE))
      send(curCmd)
      newData <- post
      while (TRUE) {
        Sys.sleep(0.01)
        newChunk <- readRaw()
        # parse data if new chunk received
        if (length(newChunk)>0) {
          newData <- c(newData, strsplit(newChunk,"\r?\n")[[1]])
          start <- Sys.time() # received data, reset timeout counter
          promptIdx <- grep(regexPS1,newData)

          # received complete output
          if (length(promptIdx)==2) {
            if (!isTRUE(share)) {
              tmpStr <- sub(regexPS1,"",newData[promptIdx[1]])
              if (nchar(tmpStr)>0) shift <- 1 else shift <- 2
              curRet <- newData[(promptIdx[1]+shift):(promptIdx[2]-1)]
              post <- newData[promptIdx[2]:length(newData)]
            }
            else
            {
              curRet <- NULL
              tmpStr <- sub(regexPS1,"",newData[promptIdx[1]])
              if (nchar(tmpStr)>0) curRet <- c(curRet, tmpStr)
              if (diff(promptIdx)>1) curRet <- c(curRet, newData[(promptIdx[1]+1):(promptIdx[2]-1)])
              tmpStr <- sub(regexPS1,"",newData[promptIdx[2]])
              if (nchar(tmpStr)>0) curRet <- c(curRet, tmpStr)
              tmpStr <- newData[promptIdx[2]]
              tmpStr2 <- regmatches(tmpStr,regexec(regexPS1,tmpStr))[[1]]
              post <- tmpStr2
            }
            ret[[cmdIdx]] <- curRet
            break
          }
        }
        if (timedOut(start,timeout.cmd))
          stop("ssh - probably connection timed out or command execution took too long")
      }
    }
    if (length(ret)==0)
      ret <- list(character(0))

    ret
  }

  execBash <- function(cmdstr, timeout.cmd=NULL, retry=TRUE) {

    if (is.null(PS1))
      stop("The bash prompt must be specified via the PS1 argument to use this function")
    if (isTRUE(retry)) {
      res <- try(execBashBasic(cmdstr,timeout.cmd),silent=TRUE)
      if (isTRUE(class(res)=="try-error")) {
        closeCon()
        initCon()
        res <- execBashBasic(cmdstr,timeout.cmd)
      }
    } else {
      res <- execBashBasic(cmdstr,timeout.cmd)
    }
    res
  }


  sendFile2 <- function(src, dest) {
    loadDefaults()
    readRaw()
    send(c("",
           "set +o emacs +o vi",
           paste0("rm '",dest,"'"),
           paste0("xxd -r -p <<'EOF' > '",dest,"'")))
    hex <- system(paste0("xxd -p '",src,"'"),intern=TRUE)
    lineMax <- 100
    idx <- 1
    while (idx <= length(hex)) {
      curLines <- hex[idx:min(idx+lineMax-1,length(hex))]
      send(curLines)
      lineCounter <- 0
      while (lineCounter < length(curLines)) {
        Sys.sleep(0.001)
        lineCounter <- lineCounter + length(read())
      }
      idx <- idx + lineMax
    }
    send("EOF")
    while (! any(grepl("EOF",read(),fixed=TRUE)))
      Sys.sleep(0.001)
  }

  sendFile <- function(src, dest, chunk.size=1024) {

    #stop("currently has issues, seems not to transfer the whole file")
    stopifnot(is.numeric(chunk.size), chunk.size>=4, !any(grepl("'",c(src,dest))))

    execBashBasic(paste0("printf '' > '",dest,"'"))
    #system(paste0("printf '' > '",dest,"'"))

    hnd <- file(src,open="rb")
    combChars <- character(0)
    chunkCounter <- 0
    while (TRUE) {
      newData <- readBin(hnd,what="integer",signed=FALSE,n=chunk.size,size=1)
      isPrintable <- (newData >= utf8ToInt(("0")) & newData <= utf8ToInt("9")) |
        (newData >= utf8ToInt(("a")) & newData <= utf8ToInt("z")) |
        (newData >= utf8ToInt(("A")) & newData <= utf8ToInt("Z"))
      printChars <- intToUtf8(newData[isPrintable],multiple = TRUE)
      noprintChars <- sprintf("\\x%02x",as.integer(newData[!isPrintable]))
      newCombChars <- character(length(newData))
      newCombChars[isPrintable] <- printChars
      newCombChars[!isPrintable] <- noprintChars
      combChars <- c(combChars,newCombChars)
      cutIdx <- match(FALSE,cumsum(nchar(combChars)) <= chunk.size,nomatch=length(combChars)+1)-1

      printInfo(paste0("ssh - sending chunk ...",chunkCounter<-chunkCounter+1),2)
      sendStr <- paste0(combChars[1:cutIdx],collapse="")
      cmdstr <- paste0("printf '",sendStr,"' >> '",dest,"'")
      #system(cmdstr)
      execBashBasic(paste0("printf '",sendStr,"' >> '",dest,"'"))

      if (cutIdx>=length(combChars) && length(newData)==0)
        break
      if (cutIdx < length(combChars))
        combChars <- combChars[(cutIdx+1):length(combChars)]
      else
        combChars <- character(0)
    }
    close(hnd)
  }


  initCon <- function() {

    # assertions
    stopifnot(is.null(logfile),
              is.null(inPipe),is.null(outPipe),
              is.null(inpHnd),is.null(outHnd),is.null(tempPwfile))

    # defaults
    loadDefaults()

    # folder names
    mytempdir <<- generateTempdir(tempdir.loc)
    system(paste0("chmod 700 '",mytempdir,"'"))

    logfile <<- file.path(mytempdir,"logfile")
    inPipe <<- file.path(mytempdir,"sshin.pipe")
    outPipe <<- file.path(mytempdir,"sshout.pipe")
    socketFile <<- file.path(mytempdir,"sshsock")

    # prepare filepaths
    tempPwfile <<- file.path(mytempdir,"login")
    sshpass_prefix_cmd <- paste0("sshpass -f'",tempPwfile,"' ") 
    if (is.null(pwfile) && !is.null(password)) {
      writeLines(password,tempPwfile)
    } else if (!is.null(pwfile) && is.null(password)) {
      password <- readLines(pwfile,n=1)
      writeLines(password,tempPwfile)
    } else {
      writeLines("nothing",tempPwfile)
      sshpass_prefix_cmd <- ""
    }
    system(paste0("chmod 600 '",tempPwfile,"'"))

    # prepare master connection
    if (isTRUE(share)) {
      cmdstr <- paste0(sshpass_prefix_cmd,
                       "ssh -p'", port,"' -nNf -E '",logfile,"' ",
                       "-o ControlMaster=yes -o ControlPath='",socketFile,"' ",login)
      retcode <- system(cmdstr)
      if (retcode != 0) {
        stop(paste0("Some error occurred during the connection using the command:\n",
	            cmdstr))
      }
    }

    # prepare interactive ssh connection
    if (!file.exists(inPipe))
      system(paste0("mkfifo -m 600 ",inPipe))
    if (!file.exists(outPipe))
      system(paste0("mkfifo -m 600 ",outPipe))

    cmdstr <- paste0(sshpass_prefix_cmd,
                     "ssh -p'",port,"' -tt -E '",logfile,"' ",
                     if (isTRUE(share))
                      paste0("-o ControlMaster=no -o ControlPath='",socketFile,"' ")
                     else "",
                     " '",login,"' < '",inPipe,"' > '",outPipe,"'")

    printInfo("ssh - connecting to remote machine...",1)
    system(cmdstr,wait=FALSE)

    inpHnd <<- fifo(inPipe,open="w+b",blocking=FALSE)
    outHnd <<- fifo(outPipe,open="rb",blocking=FALSE)

    # restrict file access rights
    start <- Sys.time()
    while (!file.exists(logfile)) {
      if (timedOut(start,5)) {
        closeCon()
        stop(paste0("expecting creation of logfile ",
                  logfile, " but it does not happen (fast enough)"))
      } 
      Sys.sleep(0.1)
    }

    system(paste0("chmod 600 '",logfile,"' '",inPipe,"' '",outPipe,"' '",tempPwfile,"'"))
    if (isTRUE(share))
      system(paste0("chmod 600 '",socketFile,"'"))

    # wait for text on stdout to indicate successful login
    answer <- readAndWait(0.5, 10)

    if (length(answer)==0) {
      closeCon()
      stop(paste0("unable to establish connection!\n",
                  "Try the following command manually to see what's wrong:\n",
                  cmdstr))
    }

    # wait until all text has been printed
    answer <- c(answer,readAndWait(0.1,0.5))

    # prepare the prompt
    # if PS1=NULL in the argument list
    # we return immediately without trying to change the prompt
    # function execBash will then not be available
    if (is.null(PS1))
      return(TRUE)

    send(c(paste0("PS1='",PS1,"'"),""))
    answer <- readAndWait(0.1, 5)
    send("")  # hack: if using a shared connection
              # the command prompt is repeated several times on the same line
              # but sending just something gives a single prompt in the last 
              # transmitted line
    answer <- readAndWait(0.1, 5)
    
    if (!isTRUE(grepl(PS1,answer[length(answer)],fixed=TRUE))) {
      closeCon()
      stop("unable to remove bash prompt via PS1 - is it really bash?!")
    }

    # if not regex to capture PS1 specified by user
    # try to construct one
    if (is.null(regexPS1))
      regexPS1 <<- escapeRegex(answer[length(answer)])

    # make sure it is really bash
    thisShell <- execBashBasic("echo $0")[[1]]
    if (!grepl("bash",thisShell,fixed=TRUE)) {
      closeCon()
      stop(paste0("this is not bash but ",thisShell))
    }

    printInfo("ssh - ...connection established",1)
    TRUE
  }

  closeCon <- function() {
    # kill the ssh processes
    if (!is.null(logfile)) {
      psOut <- system(paste0("ps -ewwo pid,cmd | grep -F '",logfile,"'"),intern=TRUE)
      pat <- paste0("^ *([[:digit:]]+) +.*$")
      matchedPsOut <- regmatches(psOut,regexec(pat,psOut))
      psIds <- unlist(lapply(matchedPsOut,function(x) x[2]))
      if (length(psIds)>0)
        system(paste0("kill ",paste0(psIds,collapse=" ")))
    }
    # close handles and cleanup
    try({
      writeLines("",tempPwfile)
      inpHnd <<- close(inpHnd)
      outHnd <<- close(outHnd)
      file.remove(c(logfile,inPipe,outPipe))
      file.remove(tempPwfile)
    },silent=TRUE)
    # reset globalvars
    logfile <<- NULL
    inPipe <<- NULL
    outPipe <<- NULL
    inpHnd <<- NULL
    outHnd <<- NULL
    tempPwfile <<- NULL
    socketFile <<- NULL

    return(NULL)
  }


  initCon()

  list(send=send,read=read,readRaw = readRaw,
       execBash=execBash, sendFile=sendFile, sendFile2=sendFile2,
       initCon=initCon,closeCon=closeCon,
       logfile=logfile, getPwfile=getPwfile, getSocketFile=getSocketFile)
}
