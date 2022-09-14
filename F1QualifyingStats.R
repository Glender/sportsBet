library(rvest)
library(magrittr)
library(stringr)
library(tibble)


# Scrape all urls that link to qualifying statistics.
find_qualifying_urls <- function(main_url, base_url = "https://www.formula1.com") {
  
  html_file <- rvest::read_html(main_url)
  
  # Scrape tags with the href attribute,
  # because those link to a website.
  urls <- rvest::html_nodes(html_file, "a") %>%
    rvest::html_attr("href")
  
  urls <- urls[stringr::str_detect(urls, "qualifying")]
  urls <- urls[!is.na(urls)]
  
  # Combine base and end url to complte full urls.
  qualifying_urls <- stringr::str_c(base_url, urls)
  return(unique(qualifying_urls))
}


# Extract circuit/country from url.
find_circuit_in_url <- function(url) {
  
  # find all  '/' chars in your url
  # it is assumed that the cicruit/country
  # is located between the 4th and 5th '/'.
  lookup <- stringr::str_locate_all(url, "/")[[1]]
  start_end <- lookup[c(8,9), 1]
  
  # extract clubname
  circuit_name <- stringr::str_sub(
    url, 
    start = start_end[1] + 1, 
    end = start_end[2] - 1
  )
  return(circuit_name)
}

# Vectorised version.
find_circuits_in_urls_vec <- function(urls) {
  return(unname(sapply(urls, find_circuit_in_url)))
}

# Extract the table with qualifying stats from the website.
scrape_qualification <- function(url) {
  
  data <- rvest::read_html(url) %>%
    rvest::html_table()
  
  if(length(data) != 0){
    
    # Unlist the data and remove NA cols
    data <- data[[1]]
    data <- data[, colSums(is.na(data)) < nrow(data)]
  }
  
  cat("Scraped the website:", url, "\n")
  return(data)
}


# Vectorised version to scrape multiple urls at once.
scrape_qualifying_stats <- function(qualifying_urls) {
  data <- lapply(qualifying_urls, scrape_qualification)
  names(data) <- find_circuits_in_urls_vec(qualifying_urls)
  return(data)
}

# Examples:
# URL with all relevant qualifying statistics.
url <- 'https://www.formula1.com/en/results.html/2022/races/1114/great-britain/qualifying.html'
quali_urls <- find_qualifying_urls(url)
data <- scrape_qualifying_stats(quali_urls)


# Remove trailing white space in the drivers name.
trim_driver_name <- function(string) {
  return(stringr::str_squish(string))
}


# Get only the first and last name.
# Requires trimmed names; no fancy aesthetics.
get_first_last_name <- function(name){
  return(stringr::word(name, 1, 2))
}


# Get the capital name (e.g. 'Max Verstappen VER' becomes 'VER' ) 
get_capital_name <- function(driver_name){
  return(base::sub(".* ", "", driver_name))
}


# Convert str like '1:09.23' to double 69.23. 
str_to_seconds <- function(string){
  
  # Split the time in two parts, minutes and seconds.
  l <- stringr::str_split(string, ":")
  l_num <- lapply(l, as.numeric)
  
  # Create empty container to store the results.
  time <- vector("double", length = length(l_num))
  idx <- 1
  for(iter in l_num){
    
    min <- iter[1] # Get minutes.
    sec <- iter[2] # And seconds.
    
    # Convert to a total time score in seconds.
    time[idx] <- (min*60) + sec 
    idx <- idx + 1
  }
  return(time)
}