my_histo <- function(df, x_var, ...){

df |>
    ggplot() +
    aes(x= {{x_var}}) +
    geom_histogram(aes(y=after_stat(density)), ...)

}