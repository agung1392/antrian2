#' Simple antrians sederhana server tunggal.
#'
#' @param ak A number.
#' @param lm A number.
#' @param rd A number.
#' @param rsc A number.
#' @param rn A number.
#' @param ongoing logical.
#' @return a data frame of \code{get_mon_arrivals}
#' @examples
#' simpel(ak = 0.5, lm = 0.55)
#' simpel(rn = 5)

simpel <- function(ak=0.40, lm=0.45, rd=3, rsc=1, rn=50, ongoing=TRUE){
  ######################################################
  # Program : simpel                                   #
  # Tujuan  : Sistem antrian sederhana dengan server   #
  #           tunggal (antrian tunggal)                #
  # Tanggal : 20 Juli 2022                             #
  # Oleh    : I G.A. Anom Yudistira                    #
  # Versi : v01                                        #
  ######################################################

  # ak laju kedatangan entiti; lm laju lamanya layanan; rd nilai pembulatan
  # rsc jumlah sumberdaya; rn jumlah run simulasi; ongoing = TRUE mencatat datat
  # kedatangan entiti yang belum keluar dari sistem

  env <- simmer("sederhana")

  # Waktu Antar Kedatangan (AK)
  AK <- function() round(rexp(n=1, rate=ak),rd)

  # Membangkitkan lamanya layanan
  Lama <- function() round(rexp(n=1, rate=lm),rd)

  lintas <- trajectory() %>%
    seize("ATM") %>%
    timeout(Lama) %>%
    release("ATM")

  env %>%
    add_resource("ATM", 1) %>%
    add_generator("nasabah", lintas, AK) %>%
    run(rn) %>% invisible

  output.dat <- as_tibble(get_mon_arrivals(env, ongoing = ongoing))
  out <- subset(output.dat, start_time>0)
  return(out)
}






