library(rvest)

# URL with all relevant qualifying statistics.
url <-'https://f1i.com/news/453860-2022-italian-grand-prix-qualifying-results.html'

# Extract the matrix with racing stats.
html_file <- rvest::read_html(url)
data <- html_table(html_file)

