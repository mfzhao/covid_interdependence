

state_code_from_fips <- function(x) {
    floor(as.integer(as.character(x))/1000)
}