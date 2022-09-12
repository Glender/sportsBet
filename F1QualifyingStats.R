library(rvest)

# URL with all relevant qualifying statistics.
url <-'https://f1i.com/news/453860-2022-italian-grand-prix-qualifying-results.html'
#url <- 'https://f1i.com/news/452899-2022-dutch-grand-prix-qualifying-results.html'
# https://f1i.com/news/451890-2022-belgian-grand-prix-qualifying-results.html
# https://f1i.com/news/449871-2022-hungarian-grand-prix-qualifying-results.html
# https://f1i.com/news/448931-2022-french-grand-prix-qualifying-results.html
# https://f1i.com/news/447396-2022-austrian-grand-prix-qualifying-results.html

# https://www.formula1.com/en/results.html/2022/races/1114/great-britain/qualifying.html

# Extract the matrix with racing stats.
html_file <- rvest::read_html(url)
data <- html_table(html_file)

