#******************************************************************************#
#                                                                              #
#                    Lab 3 - Data Acquisition & Analysis                       #
#                                                                              #
#                     Your Name - Data Driven Securty                          #
#                                                                              #
#******************************************************************************#

## Crawling y Scrapping

if(!require("xml2")){
  install.packages("xml2")
  library("xml2")
}

if(!require("httr")){
  install.packages("httr")
  library("httr")
}

if(!require("dplyr")){
  install.packages("dplyr")
  library("dplyr")
}

if(!require("ggplot2")){
  install.packages("ggplot2")
  library("ggplot2")
}




### 1.1 Obtención de la página web
domain <- "https://www.mediawiki.org";
uri <- paste(domain, "/wiki/MediaWiki", sep = "")

load_page <- function(uri)
{
  page <- xml2::read_html(uri)
}

page <- load_page(uri)


### 1.2 Analisis de el contenido de la web
get_title <- function()
{
  xml2::xml_text(xml2::xml_find_first(page, "//title"))
}

get_charset <- function()
{
  xml2::xml_attr(xml2::xml_find_first(page, "//meta[@charset]"),"charset")
}

### 1.3.	Extracción de enlaces
get_links <- function()
{
  names <- xml2::xml_text(xml2::xml_find_all(page, "//a"))
  links <- xml2::xml_attr(xml2::xml_find_all(page, "//a"), "href")
  df <- data.frame(Name = names, Links = links, stringsAsFactors = FALSE)

  #clean links and transform data
  df <- df %>%
    filter(!is.na(Links)) %>%
    filter(!startsWith(Links, "#")) %>%
    mutate(isAbsolute = startsWith(Links, "http")) %>%
    mutate(isInternal = !isAbsolute | startsWith(Links, "https://www.mediawiki.org"))

  return(df)
}

### 1.4 Exploración de enlaces
expl_links <- function(links)
{
  status <- c()

  for (link in links$Links) {
    if (!startsWith(link, "http"))
    {
      link <-  paste(domain, link, sep="")
    }
    Sys.sleep(1)
    req <- httr::HEAD(link, handle = handle(domain))
    status <- c(status, req$status_code);
  }

  links$status_code <- status
  return(links);
}

### Gráficos en R

### 2.1 Histograma
build_histogram <- function(links)
{
  qplot(x = Links, data=links, facets = .~isAbsolute)
}

### 2.2 Un gráfico de barras
build_bar_graph <- function(links)
{
  qplot(x = links$isInternal, data=links, xlab = "Internal link")
}

### 2.3 Pie Chart
build_pie_char <- function(links)
{
  data <- links %>%
    group_by(status_code) %>%
    count() %>%
    ungroup() %>%
    mutate(percentage = `n`/sum(`n`)) %>%
    mutate(label = scales::percent(percentage))

  data$status_code <- as.factor(data$status_code)

  ggplot(data) +
    geom_bar(aes(x="", y=percentage, fill=status_code), stat="identity", width = 1)+
    coord_polar("y", start=0) +
    theme_void() +
    geom_text(aes(x=1, y = cumsum(percentage) - percentage/2, label=label))
}
