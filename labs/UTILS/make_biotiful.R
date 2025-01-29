
pct_format <- scales::percent_format(accuracy = .1)

make_biotifoul <-  function(df, .f=is.factor, .bins=30){

  .scales <- "free"

  if (identical(.f, is.factor)) {
    .scales <- "free_x"
  } 

  p <- df %>%
    select(where(.f)) %>%
    pivot_longer(
      cols = everything(),
      names_to = "var",
      values_to = "val"
    ) %>%
    ggplot() +
    aes(x = val) +
    facet_wrap(~var, scales=.scales) + 
    xlab("")
  
  if(identical(.f, is.factor)){
    p + 
    geom_bar(fill=alpha("black",.9)) + 
    geom_text(
      aes(
        label = sprintf(
          "%d",
          after_stat(count))
      ),
      stat = "count",
      nudge_y = 2,
    )
  } else {
    p + 
    geom_histogram(aes(y=after_stat(density)), bins=.bins) + 
    xlab("")
  }
}
