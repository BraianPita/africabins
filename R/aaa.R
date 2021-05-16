state_coords <- structure(list(abbrev = c("DZ", "AO", "BJ", "BW", "BF", "BI", "CM", "CV", "CF", "TD", "KM", "CG",
                                          "CD", "DJ", "EG", "GQ", "ER", "ET", "GA", "GM", "GH", "GN", "GW", "CI",
                                          "KE", "LS", "LR", "LY", "MG", "MW", "ML", "MR", "MU", "MA", "MZ", "NA",
                                          "NE", "NG", "RW", "ST", "SN", "SC", "SL", "SO", "ZA", "SS", "SD", "SZ",
                                          "TZ", "TG", "TN", "UG", "ZM", "ZW", "EH", "YT", "RE", "SH"),
                               state = c("DZA", "AGO", "BEN", "BWA", "BFA", "BDI", "CMR", "CPV", "CAF", "TCD", "COM", "COG",
                                         "COD", "DJI", "EGY", "GNQ", "ERI", "ETH", "GAB", "GMB", "GHA", "GIN", "GNB", "CIV",
                                         "KEN", "LSO", "LBR", "LBY", "MDG", "MWI", "MLI", "MRT", "MUS", "MAR", "MOZ", "NAM",
                                         "NER", "NGA", "RWA", "STP", "SEN", "SYC", "SLE", "SOM", "ZAF", "SSD", "SDN", "SWZ",
                                         "TZA", "TGO", "TUN", "UGA", "ZMB", "ZWE", "ESH", "MYT", "REU", "SHN"),
                               col = c(6L, 7L, 7L, 8L, 8L, 8L, 7L, 1L, 8L, 8L, 12L, 6L, 7L, 10L, 9L, 6L, 9L,
                                       10L, 7L, 3L, 5L, 5L, 4L, 3L, 10L, 7L, 4L, 8L, 12L, 9L, 6L, 5L, 14L, 5L, 10L,
                                       7L, 7L, 7L, 8L, 4L, 4L, 14L, 6L, 11L, 7L, 9L, 9L, 8L, 9L, 6L, 7L, 9L, 8L, 9L, 4L, 13L, 14L, 3L),
                               row = c(2L, 9L, 4L, 10L, 4L, 7L, 6L, 4L, 6L, 5L, 8L, 7L, 7L, 6L, 3L, 6L,
                                       6L, 7L, 8L, 4L, 5L, 4L, 4L, 5L, 8L, 11L, 5L, 3L, 10L, 9L, 3L, 3L, 10L,
                                       2L, 9L, 10L, 3L, 5L, 8L, 7L, 3L, 7L, 4L, 6L, 12L, 5L, 4L, 11L, 8L, 5L,
                                       2L, 7L, 9L, 10L, 2L, 8L, 11L, 9L)),
                          .Names = c("abbrev", "state", "col", "row"),
                          class = "data.frame", row.names = c(NA, -54L))

state_coords <- state_coords[order(state_coords$state),]

b_state_coords <- state_coords
colnames(b_state_coords) <- c("abbrev", "state", "x", "y")
b_state_coords$y <- -b_state_coords$y

#' "State" abbreviation to name data frame
#'
#' @name state_tbl
#' @docType data
NULL
