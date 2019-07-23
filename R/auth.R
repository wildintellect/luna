# Store various secrets needed to download Satellite data.
# This method is not encrypted, it stores the credentials in plain text.
# It is only as secure as the location of the file.
#
# 

# The easy way, save to a file - format?
.create_secrets <- function(path="~/.secrets/luna.json", secrets) {
  
  if (!file.exists(path)) {
    dir.create(dirname(path), recursive = TRUE)
    jsonlite::write_json(secrets, path, pretty = TRUE)
    Sys.chmod(path, mode = "0600") # Should we lock to read only when not updating?
  }

  return(TRUE)
}

.load_secrets <- function(path="~/.secrets/luna.json") {
  # Load an existing secrets file
  if (!file.exists(path)) {
    stop("Can't find secret file: '", path, "'")
  }
  
  secrets <- jsonlite::read_json(path, simplifyVector = TRUE)
  return(secrets)
}

.find_secret <- function(id) {
  # given and id lookup the username and password
  
  secrets <- .load_secrets()
  
  if (!secrets[[id]]) {
    # Secret doesn't exist
    # Offer to create
    # If no account reply with the url to create the account
  }
  
  return(secrets[[id]])
}


.save_secret <- function(id, username, password, update=TRUE) {
  # Save a new secret to the secrets file, or update and existing one
  
  # Load the secrets file if not in memory
  # TODO: keep the secrets in memory?
  secrets <- .load_secrets()
  
  # If the secret exists update it otherwise append
  if ( secrets[[id]] ) {
    if ( update==TRUE ) {
      secrets[[id]] <- list(username=username, password=password)
    } else {
      return(print(paste0("Secret for ", id," already exists")))
    }
  } else {
    secrets[[id]] <- list(username=username, password=password)
  }
  
  # Write the secrets to file
  .create_secrets(secrets)
}
  