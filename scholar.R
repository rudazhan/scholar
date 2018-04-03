# Google Scholar does not provide an API and forbids scrapping.
# Current programmatic access to Google Scholar data all avoids heavy footprint.

# `scholar` by James Keirstead ----------------------------------------------
# Mentioned googleCite.r in help(scholar), but that code was non-functional.
library(scholar)

#' @return An object of class "pubs" derived from data.table
get_publications <- function(user) {
  schema <- data.table::data.table(
    name = c("title", "author", "journal", "number", "cites", "year", "cid", "pubid"),
    type = forcats::fct_recode(factor(c("c", "l", "c", "c", "i", "i", "l", "c")),
                               character = 'c', list = 'l', integer = 'i'))
  set_type <- function(x, cols, as_f) x[, (cols) := lapply(.SD, as_f), .SDcols = cols]

  x <- scholar::get_publications(user)
  data.table::setDT(x)
  set_type(x, schema[type == 'integer', name], as.integer)
  set_type(x, schema[type == 'character', name], as.character)
  set_type(x, schema[type == 'list', name], function(x) strsplit(as.character(x), ',\\s*'))
  x <- x[!(is.na(year) | nchar(journal) == 0L)]
  class(x) <- c('pubs', class(x))
  x
}

#' Method of \code{summary} for class "pubs"
summary.pubs <- function(x) {
  data.table:::print.data.table(
    x[, .(pubid, cites,
          author1 = purrr::map(author, ~ sub(".* ", '', .x[[1]])), year,
          title = stringr::str_trunc(title, 70, "right"),
          journal = stringr::str_trunc(journal, 50, "right"))],
    justify = 'left')
}

#' Method of \code{print} for class "pubs"
print.pubs <- function(x) {
  data.table:::print.data.table(x, justify = 'left', digits = 2)
}

annual_citations <- function(cites, year) {
  year_now <- lubridate::year(Sys.time()) + lubridate::yday(Sys.time()) / 365
  cites / (year_now - year)
}

#' Journal of publications by frequency
journal_groups <- function(pubs) {
  pubs[cites > 0, .(pubid, journal = toupper(journal))
       ][, .N, keyby = .(journal)][order(-N), .(N, journal)]
}

# Old visualization
scatterplot_citation <- function(pubs) {
  today <- zoo::as.yearmon(Sys.Date())
  op <- par(mar = c(2, 1, 2, 4), las = 1)
  pubs[, plot(year, cites, pch = 19, col = rgb(0,0,1,0.8),
              xlab = NA, ylab = NA, yaxt = "n",
              xlim = c(min(year), today), xaxs = 'i', yaxs = 'i',
              main = paste("Citations by Nominal Publication Year"))]
  axis(4)
  pubs[which.max(cites/(today - year)), lines(c(year, today), c(cites, 0), lty = 2)]
  par(op)
}

#' Time concious wrapper of \code[scholar]{get_article_cite_history}
get_article_cite_history <- function(user, pubids) {
  force(pubids)
  message("Query time for article citation histories:")
  print(system.time(
    cits <- purrr::map2_dfr(user, pubids, scholar::get_article_cite_history)
    ))
  data.table::setDT(cits)
  cits
}

#' Bibliometric plot
#'
#' @param drop_top Number of top citing publications removed from plot, ordered by
#' current mean annual citations after removing aged publications.
#' @param max_age Maximum age of publications accepted
#' @return A data.table of visualized publications ordered in the latest mean annual
#' citations, showing pubid, age, and shortened title.
bibliomegram <- function(cits, pubs, name, drop_top = 0, max_age = 50) {
  stopifnot(cits[, .(sorted = all(diff(year) == 1)), keyby = .(pubid)][, all(sorted)])
  year_now <- lubridate::year(Sys.Date())
  DT <- cits[, .(cum_cites = cumsum(cites), years = seq_len(.N)), keyby = .(pubid)
            ][, annual_cites := cum_cites / years]
  aged_pubs <- cits[, .(age = year_now - min(year)), keyby = .(pubid)][age > max_age, pubid]
  DT <- DT[!aged_pubs, on = .(pubid)]
  if (drop_top > 0) {
    top_pubs <- DT[, .(m_cites = annual_cites[[.N]]), keyby = .(pubid)
                   ][order(-m_cites)[seq_len(drop_top)], pubid]
    DT <- DT[!top_pubs, on = .(pubid)]
  }
  jrns <- pubs[unique(DT$pubid), on = .(pubid), .(pubid, journal = toupper(journal))]
  pal <- c('red', 'green', 'blue', 'cyan', 'orange', 'magenta')
  color <- jrns[, .(N_pub = .N), keyby = .(journal)
                ][order(-N_pub), index := .I
                  ][, cummax := max(index), keyby = .(N_pub)
                    ][, .(journal, index = ifelse(cummax > 6, NA, index))]
  color <- color[jrns, on = .(journal), .(pubid, index)]

  op <- par(mar = c(3, 3, 2, 1), mgp = c(2, 0.5, 0), las = 1, xpd = T)
  plot(NA, type = 'n', bty ='n', xaxt = 'n', yaxt = 'n', xlab = NA, ylab = NA,
       xlim = c(1 - 0.1, max(DT$years) + 0.1), ylim = c(0, max(DT$annual_cites) * 1.01),
       xaxs = 'i', yaxs = 'i',
       main = paste0(name, "'s Publications"))
  axis(1, union(1, axTicks(1)))
  grid_levels <- setdiff(axTicks(2), 0)
  segments(1, grid_levels, max(axTicks(1)), grid_levels, col = 'gray', lwd = 1)
  text(0.5, max(axTicks(2)), "Mean annual citations", adj = c(0, -2))
  axis(2, axTicks(2), tick = F)
  mtext("Years since actual publication", side = 1, line = 1.5, at = max(axTicks(1)), adj = 1)
  purrr::map(split(DT, by = "pubid"),
               ~ lines(.$years, .$annual_cites, col = "steelblue", lty = 3))
  DT[, .SD[.N], keyby = .(pubid)
     ][color, on = .(pubid), points(x = years, y = annual_cites, pch = 19,
                                    col = ifelse(is.na(index), 'black', pal[index]))]
  par(op)
  pubs[DT[, .SD[which.max(years)], keyby = .(pubid)], on = .(pubid),
       .(pubid, m_cite = annual_cites, age = years, title = substr(title, 1, 70))
       ][order(-m_cite)]
}

# case study --------------------------------------------------------------

user <- "F_Go4V4AAAAJ"
(profile <- scholar::get_profile(user))
pubs <- get_publications(user)
summary(pubs)
max_pubs <- 50L
select_pubid <- pubs[order(annual_citations(cites, year), decreasing = T)
                     ][cites > 0, head(pubid, n = max_pubs)]
# Typically takes 5s to get citation history of 50 papers
cits <- get_article_cite_history(user, select_pubid)
(journal_groups(pubs[pubid %in% select_pubid]))
# Plot interpretation:
# As with the analysis of average cost curves, mean annual citations curve
# stays level when new citations each year equals the mean annual citations by far;
# the curve rises when marginal citation exceeds the preceding average (healthy);
# the curve drops when marginal citation is less than the preceding average (aging).
(pubs_plot <- bibliomegram(cits, pubs, name = profile$name, drop_top = 0, max_age = 100))

# interactive plot: tooltip for richer text; highlight curve on hover;
