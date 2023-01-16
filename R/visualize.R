

# Visualize -------------------------------------------------------------


#' list of ggplot objects into one plot
#'
#' @param ...
#' @param plotlist
#' @param file
#' @param cols Number of columns in layout
#' @param layout A matrix specifying the layout. If present, 'cols' is ignored.
#' @details
#' If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
#' then plot 1 will go in the upper left, 2 will go in the upper right, and
#' 3 will go all the way across the bottom
#' @return
#' @export
#'
#' @examples
multiplot <-
  function(...,
           plotlist = NULL,
           file,
           cols = 1,
           layout = NULL) {

    require(grid)

    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)

    numPlots = length(plots)

    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
      # Make the panel
      # ncol: Number of columns of plots
      # nrow: Number of rows needed, calculated from # of cols
      layout <- matrix(seq(1, cols * ceiling(numPlots / cols)),
                       ncol = cols,
                       nrow = ceiling(numPlots / cols))
    }

    if (numPlots == 1) {
      print(plots[[1]])

    } else {
      # Set up the page
      grid.newpage()
      pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

      # Make each plot, in the correct location
      for (i in 1:numPlots) {
        # Get the i,j matrix positions of the regions that contain this subplot
        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

        print(plots[[i]],
              vp = viewport(
                layout.pos.row = matchidx$row,
                layout.pos.col = matchidx$col
              ))
      }
    }
  }


#' Boxplot with confidence intervals and summary statistics such as p-val and risk
#'
#' @param num
#' @param grp
#' @param data
#' @param anova
#' @param alpha
#' @param verbose
#' @param plot_only
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
plot_boxplot <-
  function(num,
           grp = NULL,
           data = NULL,
           anova = TRUE,
           alpha = 0.05,
           verbose = TRUE,
           plot_only = TRUE,
           ...) {

    if (!is.null(grp)) {
      formula = num ~ grp
      if (anova) {
        model = aov(formula, data = data)
        pval = summary(model)[[1]][["Pr(>F)"]][[1]]
      } else{
        grp = as.factor(grp)#characters do not work
        pval = kruskal.test(num, grp, data = data)$p.value
      }

      if (pval < alpha) {
        phrase = glue::glue("significant (alpha = {alpha})")

      } else{
        phrase = glue::glue("NOT significant (alpha = {alpha})")
      }

      test = ifelse(anova,
                    "(ANOVA)",
                    "(Kruskal-Wallis)")

      if (verbose) {
        sub = paste("p-value=", round(pval, 4), " ", phrase, test)
      }
      else{
        sub = ""
      }
    } else {
      #if group == NULL
      sub = ""
      formula = num
      pval = NA
    }

    res = boxplot(formula, sub = sub, ...)

    res$pval = pval

    y0 = c()
    y1 = c()
    y2 = c()

    if (is.null(grp)) {
      grp = 1
      grps = 1
      x0 = 1
    } else{
      grps = sort(unique(grp))

      x0 = seq_len(length(grps))

    }

    for (k in grps) {

      Xloc = num[grp == k]
      y0 = c(y0, mean(Xloc) - qnorm(1 - alpha / 2) * sd(Xloc) / sqrt(length(Xloc)))
      y1 = c(y1, mean(Xloc) + qnorm(1 - alpha / 2) * sd(Xloc) / sqrt(length(Xloc)))
      y2 = c(y2, mean(Xloc))
    }

    arrows(
      x0 = x0,
      y0 = y0,
      x1 = x0,
      y1 = y2,
      angle = 90,
      code = 3,
      lwd = 2,
      col = "blue"
    )

    arrows(
      x0 = x0,
      y0 = y0,
      x1 = x0,
      y1 = y1,
      angle = 90,
      code = 3,
      lwd = 2,
      col = "red"
    )

    res$confint = rbind(y1, y2, y0)
    row.names(res$confint) <- c("upper", "mean", "lower")
    res$risk = alpha

    if (plot_only == TRUE) {
      return(invisible(res))
    } else { return(res) }
  }
