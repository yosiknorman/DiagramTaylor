TaylorDiag = function (mydata, obs = "obs", mod = "mod", group = NULL, type = "default", 
          normalise = FALSE, cols = "brewer1", rms.col = "darkgoldenrod", 
          cor.col = "black", arrow.lwd = 3, annotate = "centred\\nRMS error", 
          key = TRUE, key.title = group, key.columns = 1, key.pos = "right", 
          strip = TRUE, auto.text = TRUE, ...) 
{
  sd.mod <- R <- NULL
  current.strip <- trellis.par.get("strip.background")
  current.font <- trellis.par.get("fontsize")
  on.exit(trellis.par.set(fontsize = current.font))
  if (length(cols) == 1 && cols == "greyscale") {
    trellis.par.set(list(strip.background = list(col = "white")))
    method.col <- "greyscale"
  }
  else {
    method.col <- "default"
  }
  extra.args <- list(...)
  extra.args$xlab <- if ("xlab" %in% names(extra.args)) {
    quickText(extra.args$xlab, auto.text)
  }
  else {
    NULL
  }
  extra.args$ylab <- if ("ylab" %in% names(extra.args)) {
    quickText(extra.args$ylab, auto.text)
  }
  else {
    NULL
  }
  extra.args$main <- if ("main" %in% names(extra.args)) {
    quickText(extra.args$main, auto.text)
  }
  else {
    quickText("", auto.text)
  }
  if ("fontsize" %in% names(extra.args)) {
    trellis.par.set(fontsize = list(text = extra.args$fontsize))
  }
  if (!"layout" %in% names(extra.args)) {
    extra.args$layout <- NULL
  }
  if (!"pch" %in% names(extra.args)) {
    extra.args$pch <- 20
  }
  if (!"cex" %in% names(extra.args)) {
    extra.args$cex <- 2
  }
  combine <- FALSE
  if (length(mod) == 2) 
    combine <- TRUE
  if (any(type %in% dateTypes)) {
    vars <- c("date", obs, mod)
  }
  else {
    vars <- c(obs, mod)
  }
  twoGrp <- FALSE
  if (!missing(group)) 
    if (any(group %in% type)) 
      stop("Can't have 'group' also in 'type'.")
  mydata <- cutData(mydata, type, ...)
  if (missing(group)) {
    if ((!"group" %in% type) & (!"group" %in% c(obs, mod))) {
      mydata$group <- factor("group")
      group <- "group"
      npol <- 1
    }
  }
  else {
    mydata <- cutData(mydata, group, ...)
  }
  if (!missing(group)) {
    npol <- length(unique((mydata[[group[1]]])))
    if (length(group) == 2L) {
      twoGrp <- TRUE
      grp1 <- group[1]
      grp2 <- group[2]
      if (missing(key.title)) 
        key.title <- grp1
      vars <- c(vars, grp1, grp2)
      mydata$newgrp <- paste(mydata[[group[1]]], mydata[[group[2]]], 
                             sep = "-")
      group <- "newgrp"
    }
    if (group %in% dateTypes | any(type %in% dateTypes)) {
      vars <- unique(c(vars, "date", group))
    }
    else {
      vars <- unique(c(vars, group))
    }
  }
  mydata <- checkPrep(mydata, vars, type)
  mydata <- checkNum(mydata, vars = c(obs, mod))
  mydata <- na.omit(mydata)
  legend <- NULL
  calcStats <- function(mydata, obs = obs, mod = mod) {
    R <- cor(mydata[[obs]], mydata[[mod]], use = "pairwise")
    sd.obs <- sd(mydata[[obs]])
    sd.mod <- sd(mydata[[mod]])
    if (normalise) {
      sd.mod <- sd.mod/sd.obs
      sd.obs <- 1
    }
    res <- data.frame(R, sd.obs, sd.mod)
    res
  }
  vars <- c(group, type)
  results <- group_by(mydata, UQS(syms(vars))) %>% do(calcStats(., 
                                                                obs = obs, mod = mod[1]))
  results.new <- NULL
  if (combine) {
    results.new <- group_by(mydata, UQS(syms(vars))) %>% 
      do(calcStats(., obs = obs, mod = mod[2]))
  }
  if (is.null(group)) {
    results$MyGroupVar <- factor("MyGroupVar")
    group <- "MyGroupVar"
  }
  myColors <- openColours(cols, npol)
  pch.orig <- extra.args$pch
  if (twoGrp) {
    myColors <- rep(openColours(cols, length(unique(mydata[[grp1]]))), 
                    each = length(unique(mydata[[grp2]])))
    extra.args$pch <- rep(extra.args$pch, each = length(unique(mydata[[grp2]])))
  }
  temp <- paste(type, collapse = "+")
  myform <- formula(paste("R ~ sd.mod", "|", temp, sep = ""))
  scales <- list(x = list(rot = 0), y = list(rot = 0))
  pol.name <- sapply(levels(mydata[, group]), function(x) quickText(x, 
                                                                    auto.text))
  if (key & npol > 1 & !combine) {
    thecols <- unique(myColors)
    if (twoGrp) {
      pol.name <- levels(factor(mydata[[grp1]]))
    }
    key <- list(points = list(col = thecols), pch = pch.orig, 
                cex = extra.args$cex, text = list(lab = pol.name, 
                                                  cex = 0.8), space = key.pos, columns = key.columns, 
                title = quickText(key.title, auto.text), cex.title = 0.8, 
                lines.title = 3)
  }
  else if (key & npol > 1 & combine) {
    key <- list(lines = list(col = myColors[1:npol]), lwd = arrow.lwd, 
                text = list(lab = pol.name, cex = 0.8), space = key.pos, 
                columns = key.columns, title = quickText(key.title, 
                                                         auto.text), cex.title = 0.8, lines.title = 3)
  }
  else {
    key <- NULL
  }
  if (length(type) == 1 & type[1] == "wd" & is.null(extra.args$layout)) {
    wds <- c("NW", "N", "NE", "W", "E", "SW", "S", "SE")
    mydata$wd <- ordered(mydata$wd, levels = wds)
    wd.ok <- sapply(wds, function(x) {
      if (x %in% unique(mydata$wd)) 
        FALSE
      else TRUE
    })
    skip <- c(wd.ok[1:4], TRUE, wd.ok[5:8])
    mydata$wd <- factor(mydata$wd)
    extra.args$layout <- c(3, 3)
    if (!"skip" %in% names(extra.args)) {
      extra.args$skip <- skip
    }
  }
  if (!"skip" %in% names(extra.args)) {
    extra.args$skip <- FALSE
  }
  stripName <- sapply(levels(mydata[, type[1]]), function(x) quickText(x, 
                                                                       auto.text))
  if (strip) 
    strip <- strip.custom(factor.levels = stripName)
  if (length(type) == 1) {
    strip.left <- FALSE
  }
  else {
    stripName <- sapply(levels(mydata[, type[2]]), function(x) quickText(x, 
                                                                         auto.text))
    strip.left <- strip.custom(factor.levels = stripName)
  }
  if (length(type) == 1 & type[1] == "default") 
    strip <- FALSE
  id <- which(names(results) == group)
  names(results)[id] <- "MyGroupVar"
  maxsd <- 1.2 * max(results$sd.obs, results$sd.mod)
  if (!"ylim" %in% names(extra.args)) {
    extra.args$ylim <- 1.12 * c(0, maxsd)
  }
  if (!"xlim" %in% names(extra.args)) {
    extra.args$xlim <- 1.12 * c(0, maxsd)
  }
  if (is.null(extra.args$ylab)) {
    extra.args$ylab <- if (normalise) 
      "standard deviation (normalised)"
    else "standard deviation"
  }
  if (is.null(extra.args$xlab)) {
    extra.args$xlab <- extra.args$ylab
  }
  xyplot.args <- list(x = myform, data = results, groups = results$MyGroupVar, 
                      aspect = 1, type = "n", as.table = TRUE, scales = scales, 
                      key = key, par.strip.text = list(cex = 0.8), strip = strip, 
                      strip.left = strip.left, panel = function(x, y, ...) {
                        panel.taylor.setup(x, y, results = results, maxsd = maxsd, 
                                           cor.col = cor.col, rms.col = rms.col, annotate = annotate, 
                                           ...)
                        panel.superpose(x, y, panel.groups = panel.taylor, 
                                        ..., results = results, results.new = results.new, 
                                        combine = combine, myColors = myColors, arrow.lwd = arrow.lwd)
                      })
  xyplot.args <- listUpdate(xyplot.args, extra.args)
  plt <- do.call(xyplot, xyplot.args)
  if (length(type) == 1) 
    plot(plt)
  else plot(useOuterStrips(plt, strip = strip, strip.left = strip.left))
  newdata <- results
  output <- list(plot = plt, data = newdata, call = match.call())
  class(output) <- "openair"
  invisible(output)
}