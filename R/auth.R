# Need to decide how to save authentication for various services

# The easy way, save to a file - format?
create_secrets <- function(path="~/.secrets/luna.json") {
  
  js <- list(site="biogeo.ucdavis.edu",username="me",password="example")
  
  if (!file.exists(path)) {
    dir.create(dirname(path), recursive = TRUE)
    jsonlite::write_json(js, path)
    Sys.chmod(path, mode = "0600") # Should we lock to read only when not updating?
  }

}

load_secrets <- function(path="~/.secrets/luna.json") {
  if (!file.exists(path)) {
    stop("Can't find secret file: '", path, "'")
  }
  
  jsonlite::read_json(path)
}

find_secret <- function(id, keys) {
  # given and id lookup the username and password
  
}
save_secret <- function(id, username, password) {
  # Save a new secret to the secrets file
  
  # Load the secrets file if not in memory
  existing <- load_secrets()
  # If the secret exists update it otherwise append
  
  # Write the secrets to file
}
  
  
