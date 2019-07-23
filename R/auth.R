# Store various secrets needed to download Satellite data.
# This method is not encrypted, it stores the credentials in plain text.
# It is only as secure as the location of the file and the R session.
#
# 

# The easy way, save to a file - format? json

.clear_secrets <-function(path="~/.secrets/luna.json") {
  # Delete the secrets file for security or if corrupted
  # TODO: Should this be exposed to the end user?
  unlink(path)
  
  return(TRUE)
}

.create_secrets <- function(secrets, path="~/.secrets/luna.json") {
  
  path <- path.expand(path)
  
  if (!file.exists(path)) {
    dir.create(dirname(path), recursive = TRUE)
  }
  
  jsonlite::write_json(secrets, path, pretty = TRUE)
  Sys.chmod(path, mode = "0600") # Should we lock to read only when not updating?

  return(TRUE)
}

.load_secrets <- function(path="~/.secrets/luna.json") {
  # Load an existing secrets file
  
  path <- path.expand(path)
  
  if (!file.exists(path)) {
    stop("Can't find secret file: '", path, "'")
  }
  
  secrets <- jsonlite::read_json(path, simplifyVector = TRUE)
  return(secrets)
}


.save_secret <- function(id, username, password, update=TRUE) {
  # Save a new secret to the secrets file, or update and existing one
  
  # Load the secrets file if not in memory
  # TODO: keep the secrets in memory?
  secrets <- .load_secrets()
  
  # If the secret exists update it otherwise append
  if ( !is.null(secrets[[id]]) ) {
    if ( update==TRUE ) {
      secrets[[id]] <- list(username=username, password=password)
    } else {
      return(print(paste0("Secret for ", id," already exists")))
    }
  } else {
    secrets[[id]] <- list(username=username, password=password)
  }
  
  # Write the secrets to file
  verify <- .create_secrets(secrets=secrets)
  
  return(secrets[[id]])
}

getCredentials <- function(id) {
  # given and id lookup the username and password
  # id is typically the url of the login, so that users can go there if they don't have an account
  
  secrets <- .load_secrets()
  
  secret <- secrets[[id]]
  if (is.null(secret)) {
    # Secret doesn't exist
    #TODO: open browser if no account
    
    # Offer to create
    username <- readline(paste("Please type your username for", id, ": \n"))
    password <- readline(paste("Please type your password for user", username, " for", id, ": \n"))
    
    secret <- .save_secret(id, username, password)
    # If no account reply with the url to create the account
  }
  
  return(secret)
}