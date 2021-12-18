#' File providers
#' @import dplyr
#' @export
AbstractFileProvider = R6::R6Class("AbstractFileProvider", public = list(
  
  sep = NULL,
  
  #' @description New file provider
  #' @param sep - platform separator
  #' @param ... for compatibility
  #' @return the provider
  initialize = function(sep = .Platform$file.sep) {
    self$sep = sep
  },
  
  #' @param directory - the directory
  #' @return a dataframe containing the full path relative to root of this provider (path) and file / directory flag (isDir)
  listFiles = function(directory=".") stop("abstract function"),
  
  #' @return a dataframe containing the full path relative to root of this provider (path) of files and directories and file / directory flag (isDir)
  listAllFiles = function(directory=".") {
    files = self$listFiles(directory)
    recurse = files %>% filter(isDir) %>% purrr::pmap(function(path,isDir) self$listAllFiles(path))
    return(bind_rows(files,recurse))
  },
  
  relativePath = function(parts) {
    parts = parts %>% stringr::str_remove(pattern = "^\\.") %>%
      stringr::str_remove(pattern = paste0("^",self$sep)) %>%
      stringr::str_remove(pattern = paste0(self$sep,"$"))
    parts = parts[parts!=""] %>% paste0(collapse = self$sep)
    if (parts=="") parts = "."
    return(parts)
  },
  
  print=function() {invisible(self)}
))

#' Read only file providers
#' @export
ReadOnlyFileProvider = R6::R6Class("ReadOnlyFileProvider", inherit=AbstractFileProvider, public = list(
  
  getFile = function(filename) stop("abstract function"),
  
  getZip = function(filename, asFile=FALSE, unzipDir = tempdir()) {
    tmpFile = self$getFile(filename)
    if(stringr::str_ends(filename,"zip")) {
      # gets the first file in a zip archive
      
      unzipped = archive::archive(tmpFile)
      fileout = paste0(unzipDir,"/",unzipped$path[[1]])
      if (!fs::file_exists(fileout)) {
        archive::archive_extract(tmpFile, files = 1, dir = unzipDir)
      }
      return(fileout)
        
    } else {
      return(tmpFile)
    }
  },
  
  processFiles = function(func, paths) {
      pathsDf = tibble::tibble(path = paths)
      out = pathsDf %>% purrr::pmap(function(path,...) {
        return(func(self$getFile(path)))
      })
      pathsDf$value = out
      return(pathsDf)
  },
  print=function() {invisible(self)}
))

## SFTPOverSSHFileProvider ----

#' Read only file providers
#' @export
SFTPOverSSHFileProvider = R6::R6Class("SFTPOverSSHFileProvider", inherit=ReadOnlyFileProvider, public = list(
  hostName = NULL,
  ssh = NULL,
  sshkey = NULL,
  user = NULL,
  password=NULL,
  sshSession = NULL,
  tmpdir = NULL,
  
  #' @description SFTP over SSH tunnel file provider
  #' @param hostname - sftp host
  #' @param ssh - ssh tunnel host
  #' @param sshkey - ssh key file
  #' @param user - sftp username
  #' @param password - sftp password
  #' @param ... for compatibility
  #' @return the provider
  initialize = function(config, hostName = config$hostName, ssh=config$ssh, sshkey=config$sshkey, user=config$user, password=config$password, ...) {
    super$initialize(sep="/")
    self$hostName=hostName
    self$ssh=ssh
    self$sshkey=sshkey
    self$user=utils::URLencode(user, reserved = TRUE)
    self$password=utils::URLencode(password, reserved = TRUE)
    self$tmpdir=paste0(tempdir(check = TRUE),"/sftp-over-ssh/",sprintf("%1.0f",as.numeric(Sys.time())*1000))
  },
  finalize = function() {
    message("Cleaning up SFTP over SSH...")
    try(ssh::ssh_disconnect(self$sshSession),silent=TRUE)
    unlink(self$tmpdir, recursive=TRUE, force=TRUE)
  },
  curlCommand = function(relativePath) {
    if(relativePath == ".") relativePath="./"
    return(paste0(
      'curl -k -s "',
      'sftp://',self$user,":",self$password,"@",self$hostName,":22/",relativePath,
      '"'
    ))
  },
  getSession = function() {
    connected = tryCatch(ssh::ssh_info(self$sshSession)$connected, error=function(e) FALSE)
    if (!connected) {
      self$sshSession = ssh::ssh_connect(self$ssh,keyfile=self$sshkey)
    }
    return(self$sshSession)
  },
  listFiles = function(directory = ".") {
    directory = paste0(self$relativePath(directory),"/")
    vv = textConnection("vector","w")
    ssh::ssh_exec_wait(
      self$getSession(),
      command=self$curlCommand(directory), 
      std_out = vv)
    vector = textConnectionValue(vv)
    close(vv)
    vector2 <- gsub(" ", ";", vector)
    vector3 <- gsub(";+", ";", vector2)
    df <- data.frame("files" = vector3, stringsAsFactors = F)
    df2 <- df %>% tidyr::separate(files, c("rights", "links", "ownername", "ownergroup", "filesize", "t1", "t2", "t3", "name"), sep = ";", extra = "merge")
    df2$path <- gsub(";", " ", df2$name)
    df2$isDir <- ifelse(grepl("^d.*", df2$rights), TRUE, FALSE )
    df2 = df2 %>% select(path,isDir) %>% filter(!path %in% c(".","..")) %>% 
      mutate(dir=directory) %>%
      mutate(path = ifelse(dir=="./",path,paste0(directory,path)))
    return(df2%>% select(-dir))
  },
  getFile = function(filename) {
    filename = self$relativePath(filename)
    dir.create(self$tmpdir, showWarnings = FALSE, recursive = TRUE)
    tmp = tempfile(tmpdir = self$tmpdir) # have to save locally :-(
    ssh::ssh_exec_wait(
      self$getSession(),
      command=self$curlCommand(filename), 
      std_out = tmp)
    return(tmp)
  },
  print=function() {invisible(self)}
))

## SFTPFileProvider ----

#' Read only file providers
#' @export
SFTPFileProvider = R6::R6Class("SFTPFileProvider", inherit=ReadOnlyFileProvider, public = list(
  #' @description SFTP over SSH tunnel file provider
  #' @param hostname - sftp host
  #' @param user - sftp username
  #' @param password - sftp password
  #' @param ... for compatibility
  #' @return the provider
  initialize = function(config, hostName = config$hostName, user=config$user, password=config$password, ...) {
    stop("not yet implemented")
  },
  print=function() {invisible(self)}
))

## S3FileProvider ----

#' Read only file providers
#' @export
S3FileProvider = R6::R6Class("S3FileProvider", inherit=ReadOnlyFileProvider, public = list(
  bucket = NULL,
  key = NULL,
  secret = NULL,
  region = NULL,
  tmpdir = NULL,
  
  #' @description SFTP over SSH tunnel file provider
  #' @param config - config object
  #' @param bucket - S3 bucket name
  #' @param key - S3 access key
  #' @param secret - S3 secret key
  #' @param region - S3 region
  #' @param ... for compatibility
  #' @return the provider
  initialize = function(config, bucket = config$bucket, key=config$accesskey, secret=config$secretkey, region=config$region, cache=tempdir(check = TRUE), ...) {
    super$initialize(sep="/")
    self$bucket=bucket
    self$key=key
    self$secret=secret
    self$region =region
    self$tmpdir=paste0(cache,"/s3")
  },
  finalize = function() {
    message("Cleaning up S3...")
    oldfiles = file.info(list.files(self$tmpdir, recursive = TRUE, full.names = TRUE)) %>% filter(atime<Sys.Date()-5) %>% rownames()
    unlink(oldfiles, force=TRUE)
  },
  listAllFiles = function(directory=".") {
    directory = self$relativePath(directory)
    if(directory==".") directory=""
    tmp = aws.s3::get_bucket(bucket = self$bucket, key = self$key, secret=self$secret, region=self$region,prefix=directory,max=Inf)
    return(tibble(
      path=tmp %>% purrr::map_chr(~ .x$Key) %>% unname()) %>% mutate(
      isDir=path %>% stringr::str_ends("/")))
  },
  listFiles = function(directory=".") {
    tmp = self$listAllFiles(directory) 
    if (directory!=".") tmp = tmp %>% filter(path %>% stringr::str_detect(paste0(directory,"/[^/]+/?$")))
    return(tmp)
  },
  getFile = function(filename) {
    dir.create(self$tmpdir, showWarnings = FALSE, recursive = TRUE)
    tmp = paste0(self$tmpdir,"/",openssl::md5(serialize(filename, connection = NULL))) # have to save locally :-(
    if (!fs::file_exists(tmp)) {
      aws.s3::save_object(filename,file=tmp,bucket = self$bucket, key = self$key, secret=self$secret, region=self$region)
    }
    return(tmp)
  },
  getZip = function(filename, asFile=FALSE) {
    super$getZip(filename, asFile=asFile, unzipDir = paste0(self$tmpdir,"/unzip"))
  },
  print=function() {invisible(self)}
))

## LocalFileProvider ----

#' Read only file providers
#' @export
LocalFileProvider = R6::R6Class("LocalFileProvider", inherit=ReadOnlyFileProvider, public = list(
  root=NULL,
  absolutePath = function(parts) {
    if(parts==".") return(self$root)
    return(self$root %>% paste0(self$relativePath(parts)))
  },
  
  #' @description Local filesystem provider
  #' @param root - the top level directory from which to search for files
  #' @param sep - the platform file separator
  #' @param ... for compatibility
  #' @return the provider
  initialize = function(config, root=config$root, sep = .Platform$file.sep, ...) {
    super$initialize(sep)
    self$root= root %>% normalizePath(mustWork = FALSE) %>% paste0(sep) 
  },
  listFiles = function(directory=".") {
    out = tibble(path=dir(self$absolutePath(directory))) 
    if (directory!=".") out$path = out$path %>% purrr::map_chr(~self$relativePath(c(directory,.x))) 
    out = out %>% mutate(isDir=dir.exists(path)) 
    return(out)
  },
  getFile = function(filename) {
    return(self$absolutePath(filename))
  },
  print=function() {invisible(self)}
))

#' Read only file providers
#' @export
HttpFileProvider = R6::R6Class("HttpFileProvider", inherit=ReadOnlyFileProvider, public = list(
  print=function() {invisible(self)}
))
