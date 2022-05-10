if( !require( "httr" ) ) {
  install.packages( "httr" )
}
library( httr )
if( !require( "jsonlite" ) ) {
  install.packages( "jsonlite" )
}
library( jsonlite )
if( !require( "base64enc" ) ) {
  install.packages( "base64enc" )
}
library( base64enc )

# widePost.r -- a collection of functions that enable access to WIDE server data using R
#
# Usage: source( "widePost.r" )  ...from R console/script, with current directory set to folder with widePost.r
#
# to enable some debugging, such as the HTTP request/response headers and JSON request, enter the following:
#  options( verbose = 1 )
# to disable subsequent debugging, use:
#  options( verbose = 0 )

.WIDE <- new.env()
.WIDE$clientVersionDate <- "2016-06-27-1400"
.WIDE$clientVersion <- c( "1", "0" )
# list of servers that the user has logged in to...
.WIDE$connectedServers <- list()

# wideLogin -- connect to a WIDE data API server
#  serverURL: the URL of the server data API script
#  username/password: login credentials
# returns false in the event of an error, or a serverHandle (integer > 0) value to be passed to subsequent widePost functions

wideLogin<-function( serverURL, username, password )
{
  # application.php requires parameters "Username", "Password" and "cls_logon_submit" as follows...
  postBody<-list( Username = username, Password = password, cls_logon_submit = 1 )
  loginResponse <- httr::POST( serverURL, body = postBody, encode = "form" )
  loginContent <- content( loginResponse, "text" )
  
  # error handling
  if( !jsonlite::validate( loginContent ) ) {
    # php syntax errors do not get encoded as JSON, so check for beginning and ending curly braces...
    if( ( substr( loginContent, 1, 1 ) != "{" ) &&
        ( substr( loginContent, nchar( loginContent ), nchar( loginContent ) ) != "}" ) ) {
      print( paste( " php syntax error: ", loginContent ) )
    } else {
      print( paste( " invalid JSON: ", loginContent ) )
    }
    return( FALSE )
  }
  
  jsonData <- fromJSON( loginContent )
  
  if( "error" %in% names( jsonData ) ) {
    print( jsonData )
    return( FALSE )
  }
  
  # successful login
  print( jsonData$status )
  
  # check for version differences
  if( .WIDE$clientVersion[ 1 ] == jsonData$version[ 1 ] ) {
    if( .WIDE$clientVersion[ 2 ] == jsonData$version[ 2 ] ) {
      print( paste( "(0) WIDE interface ready - client/server version", paste( .WIDE$clientVersion, collapse = "." ),
                    " (", .WIDE$clientVersionDate, ")" ) )
      versionStatus <- 0
    } else if( .WIDE$clientVersion[ 2 ] > jsonData$version[ 2 ] ) {
      versionStatus <- 1
      print( paste0( "(1) WIDE interface ready - server version ", jsonData$version[ 1 ], ".", jsonData$version[ 2 ],
                     " (", jsonData$versionDate, "), client: ", paste( .WIDE$clientVersion, collapse = "." ), " (",
                     .WIDE$clientVersionDate, ")" ) )
    } else {
      versionStatus <- -1
      cat( paste0( "(-1) WIDE interface ready - server version ", jsonData$version[ 1 ], ".", jsonData$version[ 2 ],
                   " (", jsonData$versionDate, "), client: ", paste( .WIDE$clientVersion, collapse = "." ), " (",
                   .WIDE$clientVersionDate, ")\nPlease consider downloading a new client\n" ) )
    }
  } else if( .WIDE$clientVersion[ 1 ] > jsonData$version[ 1 ] ) {
    versionStatus <- 1
    print( paste0( "(1) WIDE interface ready - server version ", jsonData$version[ 1 ], ".", jsonData$version[ 2 ],
                   " (", jsonData$versionDate, "), client: ", paste( .WIDE$clientVersion, collapse = "." ), " (", .WIDE$clientVersionDate, ")" ) )
  } else {
    versionStatus <- -1
    print( paste0( "(-1) FATAL ERROR: client is out of date - server version ", jsonData$version[ 1 ], ".", jsonData$version[ 2 ],
                   " (", jsonData$versionDate, "), client: ", paste( .WIDE$clientVersion, collapse = "." ), " (", .WIDE$clientVersionDate,
                   ")\nPlease consider downloading a new client\n" ) )
  }
  
  # create and store server handle to be returned to the user
  if( versionStatus == -1 ) {
    return( -1 )
  }
  .WIDE$connectedServers <- append( .WIDE$connectedServers, serverURL )
  return( length( .WIDE$connectedServers ) )
}

# wideDataSelect -- retrieve one or more records from a database table
#  e.g.,
#   wideDataSelect(serverHandle,"lkpStreamCode",list("StreamName"))
#   wideDataSelect(serverHandle,"lkpStreamCode",list("StreamName","StreamCode"),orderBy=list("StreamName"))
#   wideDataSelect(serverHandle,"lkpStreamCode",list("StreamName","StreamCode"),orderBy=list("StreamName ASC"))
#   wideDataSelect(serverHandle,"lkpStreamCode",list("StreamName","StreamCode"),orderBy=list("StreamName DESC"))
#   wideDataSelect(serverHandle,"lkpStreamCode",list("StreamName","StreamCode"),orderBy=list('"1" DESC'))
#   wideDataSelect(serverHandle,"lkpStreamCode",list("StreamName","StreamCode"),orderBy=list('1 DESC'))
#   wideDataSelect(serverHandle,"lkpStreamCode",list("StreamName","StreamCode"),orderBy=list(1))
#   wideDataSelect(serverHandle,"lkpStreamCode",list("StreamName","StreamCode"),list(StreamName=c("like","%Black%")))
#   wideDataSelect(serverHandle,"lkpStreamCode",list("StreamName","StreamCode"),list(StreamName=c("like","%Black%"),StreamCode=c("like","bl%")),list(1),"OR")
# returns a data.frame if successful; otherwise, returns NULL

wideDataSelect<-function( serverHandle, table, colsSelected, where = NULL, orderBy = NULL, logical = "AND", geoConstraint = 0 )
{
  if( getOption( "wideFunCall", 0 ) ) {
    print( call( "wideDataSelect", serverHandle = serverHandle, table = table, colsSelected = colsSelected,
                 where = where, orderBy = orderBy, logical = logical, geoConstraint = geoConstraint ) )
  }
  if( ( serverHandle < 1 ) || ( serverHandle > length( .WIDE$connectedServers ) ) ) {
    stop( paste0( "Invalid server handle, must be one of: [", seq_len( length( .WIDE$connectedServers ) ), "]" ) )
  }
  
  # Note that the "xxx=" prefix in each "element" of the jsonPost list must be specified (i.e., it can't be left out
  #  of the jsonPost list() below)...
  # e.g., jsonPost<-list(table="lkpStreamCode",action="select",colsSelected=list("StreamName"))
  jsonPost<-list( table = table, action = "select", colsSelected = colsSelected, colsWhere = where, orderbyCols = orderBy, logical = logical, geoConstraint = geoConstraint )
  
  if( getOption( "verbose" ) ) {
    print( jsonPost )
    postResponse<-httr::POST( .WIDE$connectedServers[[serverHandle]], body = jsonPost, encode = "json", verbose() )
  } else {
    postResponse<-httr::POST( .WIDE$connectedServers[[serverHandle]], body = jsonPost, encode = "json" )
  }
  # for some circumstances (e.g., application development, testing, etc.) it may be useful to save the basic postResponse content
  #  to a file...
  # write( paste( "Content of postResponse ", content( postResponse, "text" ) ), "c:/temp/routput.txt" )
  
  postContent<-content( postResponse, "text" )
  
  if( !jsonlite::validate( postContent ) ) {
    if( ( substr( postContent, 1, 1 ) != "{" ) &&
        ( substr( postContent, nchar( postContent ), nchar( postContent ) ) != "}" ) ) {
      print( paste( " php syntax error: ", postContent ) )
    } else {
      print( paste( " invalid json: ", postContent ) )
    }
    return( NULL )
  }
  
  jsonData <- fromJSON( postContent )
  
  if( "error" %in% names( jsonData ) ) {
    print( jsonData )
    return( NULL )
  }
  
  # find all of the binary columns which must be decoded
  binaryCols <- Filter( function( colName ) { jsonData[[ 1, colName ]] == 1 }, names( jsonData ) )
  
  # removing a row from a dataframe with one column ends up converting that data frame into a vector;
  # to prevent this from happening, use drop=FALSE
  jsonData <- jsonData[ -1, , drop = FALSE ]
  
  # if the data frame isn't empty after removing the row, renumber all of the rows
  if( nrow( jsonData ) ) {
    row.names( jsonData ) <- 1:nrow( jsonData )
  }
  
  # if there are any binary columns to decode, do that now... (base64decode cannot NA, so those must be explicitly
  #  returned back as NA)
  # NA_character_ is returned because that is the type of NA value used in the shinyapp DataTable
  if( length( binaryCols ) ) {
    jsonData[[ binaryCols ]] <- sapply( jsonData[[ binaryCols ]], function( i ) {
      if( is.na( i ) ) {
        return( NA_character_ )
      }
      # for some reason, without the paste (... collapse), the as.character(...) ends up as type list
      #  in the final jsonData dataframe
      return( paste0( "0x", as.character( base64decode( i ) ), collapse = "" ) )
    } )
  }
  
  return( jsonData )
}

# wideDataSpatialConstraint -- define a geospatial constraint for subsequent queries with
#   spatial constraints enabled
#  e.g.,

# return TRUE if successful; otherwise, return FALSE
wideDataSpatialConstraint <- function( serverHandle, table = NULL, column = NULL, where = NULL, comparisonResult = NULL, logical="AND") {
  
  if( getOption( "wideFunCall", 0 ) ) {
    print( call( "wideDataSpatialConstraint", serverHandle = serverHandle, table = table, column = column, where = where,
                 comparisonResult = comparisonResult, logical = logical ) )
  }
  if( ( serverHandle < 1 ) || ( serverHandle > length( .WIDE$connectedServers ) ) ) {
    stop( paste0( "Invalid server handle, must be one of: ", seq_len( length( .WIDE$connectedServers ) ) ) )
  }
  
  jsonPost<-list( table = table, action = "geospatialConstraint", column = column, colsWhere = where,
                  comparisonResult = comparisonResult, logical = logical )
  
  if( getOption( "verbose" ) ) {
    print( jsonPost )
    postResponse<-httr::POST( .WIDE$connectedServers[[serverHandle]], body = jsonPost, encode = "json", verbose() )
  } else {
    postResponse<-httr::POST( .WIDE$connectedServers[[serverHandle]], body = jsonPost, encode = "json" )
  }
  
  postContent<-content( postResponse, "text" )
  
  if( !jsonlite::validate( postContent ) ) {
    if( ( substr( postContent, 1, 1 ) != "{" ) &&
        ( substr( postContent, nchar( postContent ), nchar( postContent ) ) != "}" ) ) {
      print( paste( " php syntax error: ", postContent ) )
    } else {
      print( paste( " invalid json: ", postContent ) )
    }
    return( FALSE )
  }
  
  jsonData <- fromJSON( postContent )
  print( jsonData )
  
  if( "error" %in% names( jsonData ) ) {
    return( FALSE )
  }
  return( TRUE )
}

# wideDataUpdate -- update one or more records of a database table
#  e.g.,
# wideDataUpdate(serverHandle = 1L, table = "lkpStreamCode", cols = list(StreamName = "Teststream1"), where =list(StreamCode = c("=", "teststr")))
# return TRUE if successful; otherwise, return FALSE

wideDataUpdate<-function( serverHandle, table, cols, where, logical = "AND" )
{
  if( getOption( "wideFunCall", 0 ) ) {
    print( call( "wideDataUpdate", serverHandle = serverHandle, table = table, cols = cols, where = where, logical = logical ) )
  }
  if( ( serverHandle < 1 ) || ( serverHandle > length( .WIDE$connectedServers ) ) ) {
    stop( paste0( "Invalid server handle, must be one of: ", seq_len( length( .WIDE$connectedServers ) ) ) )
  }
  
  # Note that the "xxx=" prefix in each "element" of the jsonPost list must be specified (i.e., it can't be left out of the jsonPost list() below)...
  jsonPost<-list( table = table, action = "update", cols = cols, colsWhere = where, logical = logical )
  
  if( getOption( "verbose" ) ) {
    print( jsonPost )
    postResponse<-httr::POST( .WIDE$connectedServers[[serverHandle]], body = jsonPost, encode = "json", verbose() )
  } else {
    postResponse<-httr::POST( .WIDE$connectedServers[[serverHandle]], body = jsonPost, encode = "json" )
  }
  
  postContent<-content( postResponse, "text" )
  
  if( !jsonlite::validate( postContent ) ) {
    if( ( substr( postContent, 1, 1 ) != "{" ) &&
        ( substr( postContent, nchar( postContent ), nchar( postContent ) ) != "}" ) ) {
      print( paste( " php syntax error: ", postContent ) )
    } else {
      print( paste( " invalid json: ", postContent ) )
    }
    return( FALSE )
  }
  
  jsonData <- fromJSON( postContent )
  print( jsonData )
  
  if( "error" %in% names( jsonData ) ) {
    return( FALSE )
  }
  
  return( TRUE )
}

# return TRUE if successful; otherwise, return FALSE

wideDataInsert<-function( serverHandle, table, cols )
{
  if( getOption( "wideFunCall", 0 ) ) {
    print( call( "wideDataInsert", serverHandle = serverHandle, table = table, cols = cols ) )
  }
  if( ( serverHandle < 1 ) || ( serverHandle > length( .WIDE$connectedServers ) ) ) {
    stop( paste0( "Invalid server handle, must be one of: ", seq_len( length( .WIDE$connectedServers ) ) ) )
  }
  
  jsonPost<-list( table = table, action = "insert", cols = cols )
  
  if( getOption( "verbose" ) ) {
    print( jsonPost )
    postResponse<-httr::POST( .WIDE$connectedServers[[ serverHandle ]], body = jsonPost, encode = "json", verbose() )
  } else {
    postResponse<-httr::POST( .WIDE$connectedServers[[ serverHandle ]], body = jsonPost, encode = "json" )
  }
  
  postContent<-content( postResponse, "text" )
  
  if( !jsonlite::validate( postContent ) ) {
    if( ( substr( postContent, 1, 1 ) != "{" ) &&
        ( substr( postContent, nchar( postContent ), nchar( postContent ) ) != "}" ) ) {
      print( paste( " php syntax error: ", postContent ) )
    } else {
      print( paste( " invalid JSON: ", postContent ) )
    }
    return( FALSE )
  }
  
  jsonData <- fromJSON( postContent )
  print( jsonData )
  
  if( "error" %in% names( jsonData ) ) {
    return( FALSE )
  }
  return( TRUE )
}

# return TRUE if successful; otherwise, return FALSE

wideDataDelete<-function( serverHandle, table, where, logical = "AND" )
{
  if( getOption( "wideFunCall", 0 ) ) {
    print( call( "wideDataDelete", serverHandle = serverHandle, table = table, where = where, logical = logical ) )
  }
  if( ( serverHandle < 1 ) || ( serverHandle > length( .WIDE$connectedServers ) ) ) {
    stop( paste0( "Invalid server handle, must be one of: ", seq_len( length( .WIDE$connectedServers ) ) ) )
  }
  
  jsonPost<-list( table = table, action = "delete", colsWhere = where, logical = logical )
  
  if( getOption( "verbose" ) ) {
    print( jsonPost )
    postResponse<-httr::POST( .WIDE$connectedServers[[ serverHandle ]], body = jsonPost, encode = "json", verbose() )
  } else {
    postResponse<-httr::POST( .WIDE$connectedServers[[ serverHandle ]], body = jsonPost, encode = "json" )
  }
  
  postContent<-content( postResponse, "text" )
  if( !jsonlite::validate( postContent ) ) {
    if( ( substr( postContent, 1, 1 ) != "{" ) &&
        ( substr( postContent, nchar( postContent ), nchar( postContent ) ) != "}" ) ) {
      print( paste( " php syntax error: ", postContent ) )
    } else {
      print( paste( " invalid JSON: ", postContent ) )
    }
    return( FALSE )
  }
  
  jsonData <- fromJSON( postContent )
  print( jsonData )
  
  if( "error" %in% names( jsonData ) ) {
    return( FALSE )
  }
  return( TRUE )
}

# example:
#	data <- wideDataSelect("lkpStreamCode", list("*"), orderBy=list("StreamCode DESC"))
#	exportCSV(data, "c:/temp/output.csv")

exportCSV<-function( data, filepath )
{
  if( getOption( "wideFunCall", 0 ) ) {
    print( call( "exportCSV", data = data, filepath = filepath ) )
  }
  
  # PARAMETERS (row.names, na)
  # - do not include row numbers
  # - save null values as conventional nulls instead of empty strings (i.e., "column1","column2",,"column4" instead of "column1","column2","","column4")
  write.csv( data, filepath, row.names = FALSE, na = "" )
}

# to read a csv, see read.csv(...)
