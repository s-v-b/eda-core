make_p_diag_1 <- function(lm.){
  augment(lm.) %>%
    ggplot() +
    aes(x=.fitted, y=.resid)+
    geom_point(alpha=.5, size=.5) +
    geom_smooth(method="loess",
                formula = y ~ x,
                se=F,
                linetype="dotted",
                linewidth=.5,
                color="black") +
    xlab("Fitted values") +
    ylab("Residuals)") +
    labs(title = "Residuals versus Fitted")
}

make_p_diag_3 <-  function(lm.){
  augment(lm.) %>%
    ggplot() +
    aes(x=.fitted, y=sqrt(abs(.std.resid))) +
    geom_smooth(formula = y ~ x,
                method="loess",
                se=F,
                linetype="dotted",
                linewidth=.5,
                color="black") +
    xlab("Fitted values") +
    ylab("sqrt(standardized residuals)") +
    geom_point(size=.5, alpha=.5) +
    labs(title = "Scale location")
}

make_p_diag_5 <-  function(lm.){
  augment(lm.) %>%
    ggplot() +
    aes(x=.hat, y=((.std.resid))) +
    geom_point(size=.5, alpha=.5) +
    xlab("Leverage") +
    ylab("Standardized residuals") +
    labs(title = "Standardized residulas versus Leverages")
}

make_p_diag_2 <-  function(lm.){
  augment(lm.) %>%
    ggplot() +
    aes(sample=.resid) +
    geom_qq(linewidth=.5, alpha=.5) +
    stat_qq_line(linetype="dotted",
                 linewidth=.5,
                 color="black") +
    labs(title="Residuals qqplot")
}

draw_diag_plots <- function(lm_1) {
  
  (make_p_diag_1(lm_1) +
    make_p_diag_2(lm_1) +
    make_p_diag_3(lm_1) +
    make_p_diag_5(lm_1) ) +
  patchwork::plot_annotation(caption=deparse(formula(lm_1)))

  }
