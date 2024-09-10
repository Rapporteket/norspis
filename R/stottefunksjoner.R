#' Beregn konfidensintervall for binomisk data med n suksess av N forsøk
#'
#' Denne funksjonen beregner konfidensintervall for binomisk fordelt data
#'
#' @param n - Antall suksess, skalar eller vektor
#' @param N - Antall forsøk, skalar eller vektor av samme lengde som n
#' @param konfnivaa - Konfidensnivået til test (Default 0.95)
#'
#' @return Nedre og øvre grense til konfidensintervallet
#'
#' @export

binomkonf <- function(n, N, konfnivaa=0.95)
{
  binkonf <- matrix(nrow=2, ncol = length(n))
  for (i in 1:length(n)) {
    if (N[i]>0) {
      binkonf[,i] <- binom.test(n[i],N[i], alternative = 'two.sided', conf.level = konfnivaa)$conf.int[1:2]
    } else {
      binkonf[,i] <- c(0,0)
    }
  }
  return(invisible(binkonf))
}


#' Trekk ut diagnoser fra kolonne med kommaseparerte diagnoser til separate
#' kolonner
#'
#' @param variable - kolonnen som skal splittes i flere
#' @param separator - separator mellom elementer i tekststreng. Overflødig hvis
#'                    unique_varnames gis som input
#' @param unique_varnames - vektor med navnene som skal trekkes ut i nye kolonner, må/bør
#'                    samsvare med navn som eksisterer i kolonnen som skal splittes
#'
#' @return En dataramme med separerte variabler
#'
#' @export
separer <- function(variable, separator = ",", unique_varnames = NULL) {
  if (is.null(unique_varnames)){
    unique_varnames <- stringi::stri_split_fixed(variable[!is.na(variable)], separator) %>%
      unlist() %>% unique()
  }
  df <- lapply(
    unique_varnames,
    function(x, variable){
      stringr::str_detect(variable,x)
    },
    variable
  )

  df <- as.data.frame.list(df)
  names(df) <- unique_varnames
  return(df)
}


#' Transponer output fra dplyr::summarise
#'
#' Denne funksjonen tar som input resultatet av dplyr::summarise og returnerer dens
#' transponerte uten at formatene endres.
#'
#' Her kan detaljer skrives
#'
#' @return tr_frame Den transponerte av inputen
#'
#' @export
#'
tr_summarize_output <- function(x, kolnavn1 = ""){

  rekkefolge <- names(x)[-1]
  y <- x %>% gather(names(x)[-1], key=nokkel, value = verdi) %>%
    spread(key=names(x)[1], value = verdi)
  y <- y[match(rekkefolge, y$nokkel), ]
  names(y)[1] <- kolnavn1

  return(y)
}
