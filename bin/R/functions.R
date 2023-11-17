extract_dye_ch <- function(file, path = "imgxpress_files", use_mask = TRUE) {

  # input: excel file with 3 tabs
  # tab 'red' = red channel
  # tab 'green' = green channel
  # tab 'Segmentation' = masking layer
  # tab 'Samples' = sample meta, concentration data
  filename <- file.path(path, file)
  red_ch <- read.xlsx(filename, "red", colNames = F)
  green_ch <- read.xlsx(filename, "green", colNames = F)
  seg <- read.xlsx(filename, "Segmentation", colNames = FALSE)
  meta <- read.xlsx(filename, "Samples", colNames = F)
  names(meta) <- c("id", "conc")

  seg <- prune_dataset(seg)
  red_ch <- prune_dataset(red_ch)
  green_ch <- prune_dataset(green_ch)
  
  if (use_mask) {
    # excel file contains extra tab now which holds info about masks.
    # expect only 1 mask, any cell (i.e. picture) which contains != 1
    # should be dropped for both red and green channel
    # This value represents how many daphnias are recogized by the program
    rows <- rownames(red_ch)
    seg <- seg[rows, ]
    
    # keep only segmentations cells which == 1
    red_ch[seg != 1] <- NA
    green_ch[seg != 1] <- NA
  }

  # identify individuals
  ids <- rownames(red_ch)
  ids <- unique(sub("[0-9][0-9]", "", ids))
  meta <- meta[meta$id %in% ids, ]

  # Find control, looks for numerics
  rows <- sapply(meta$conc, function(x) grepl("\\d", x))
  conc1 <- meta$conc[!rows]
  conc2 <- meta$conc[rows]

  # conc <- meta$conc[meta$conc != "Control"]
  lev <<- c(conc1, sort(as.numeric(conc2)))
  
  # calc ratios
  ratios <- data.frame(red_ch / green_ch)
  ratios$conc <- rownames(ratios)
  ratios$conc <- sapply(ratios$conc, function(x){
    x <- sub("[0-9]+", "", x)
    meta$conc[grepl(x, meta$id)]
  })
  ratios <- reshape2::melt(ratios, id.vars = "conc")
  ratios$conc <- factor(ratios$conc, levels = lev)
  
  # ratios_mean
  ratios_mean <- red_ch / green_ch
  ratios_mean <- apply(ratios_mean, 1, mean, na.rm = T)

  ratios_mean <- data.frame(ratio = ratios_mean)
  ratios_mean$conc <- id_conc(ratios_mean, meta)
  ratios_mean$conc <- factor(ratios_mean$conc, levels = lev)
  rownames(ratios_mean) <- NULL

  # calc means for channels
  red <- data.frame(
    mean = apply(red_ch, 1, mean, na.rm = T),
    color = "red"
  )
  green <- data.frame(
    mean = apply(green_ch, 1, mean, na.rm = T),
    color = "green"
  )

  red$conc <- id_conc(red, meta)
  green$conc <- id_conc(green, meta)

  data <- rbind(red, green)
  rownames(data) <- NULL

  # adjust the levels of concentrations: control, dmso + increasing conc
  data$conc <- factor(data$conc, levels = lev)
  data$color <- factor(data$color, levels = c("red", "green"))

  return(list("channels" = data, "ratios" = ratios, "ratios_mean" = ratios_mean))
}

prune_dataset <- function(d) {
  # rows 1 : 7 contains metadata
  d <- d[8:nrow(d), ]
  
  # remove rows with no measurments
  keep <- apply(d, 1, function(row) {
    !all(is.na(row[2:length(row)]))
    # print(row)
  })
  d <- d[keep, ]

  # set barcode as rownames
  row.names(d) <- d[, 1]
  d <- d[, 2:ncol(d)]
  d[] <- lapply(d, as.numeric) # make all numeric

  # colnames
  names(d) <- paste0("section", 1:ncol(d))

  return(as.matrix(d))
}
id_conc <- function(d, meta) {
  sapply(rownames(d), function(row_id) {
    meta$conc[sapply(meta$id, grepl, row_id)]
  })
}

# ggplot object
generate_plot <- function(data, concentration = "mg/ml", base_size = 8){
  # test if DMSO and treatment 0 are different

  # sample values
  #DMSO <- data$ratios$value[data$ratios$conc %in% "DMSO"]
  #ctrl <- data$ratios$value[data$ratios$conc %in% 0]
  #t_test <- t.test(DMSO, ctrl)
  #wilcox_test <- wilcox.test(DMSO, ctrl)

  # sample average values
  if ("DMSO" %in% data$ratios_mean){
    DMSO <- data$ratios_mean$ratio[data$ratios_mean$conc %in% "DMSO"]
    ctrl <- data$ratios_mean$ratio[data$ratios_mean$conc %in% 0]
    t_test <- t.test(DMSO, ctrl)
    wilcox_test <- wilcox.test(DMSO, ctrl)
  }else{
    t_test <- list()
    t_test$p.value <- NA
  }
  

  # ---

  # boxplot of individual means, log transformed
  p1 <- ggplot(data$channels, aes(conc, mean, fill = color)) +
    scale_y_log10() +
    stat_boxplot(geom = "errorbar", width = 0.3, position = position_dodge(width = 0.75)) +
    geom_boxplot(outlier.shape = NA) +
    geom_point(shape = 21, size = 2, position = position_jitterdodge(dodge.width = 0.75)) +
    scale_fill_manual(values = c(green = "green", red = "red")) +
    guides(fill = "none") +
    labs(title = "Sample average", 
         x = paste("Treatment", concentration),
         y = "Average log-transformed") +
    theme_cowplot(base_size) +
    theme(plot.title = element_text(hjust = 0.5))

  # ratio plot
  p2 <- ggplot(data$ratios_mean, aes(conc, ratio, fill = conc)) +
    stat_boxplot(geom = "errorbar", width = 0.3, position = position_dodge(width = 0.75)) +
    geom_boxplot(outlier.shape = NA) +
    geom_point(shape = 21, size = 2, position = position_jitterdodge(dodge.width = 0.75)) +
    guides(fill = "none") +
    labs(title = "Average red / green channel ratio", 
         x = paste("Treatment", concentration),
         y = "Ratio",
         
        #caption = paste("T-test DMSO ~ 0, p-value", round(t_test$p.value, 3))) +
        caption = paste("wilcox test DMSO ~ 0, p-value", round(wilcox_test$p.value, 3))) +
    theme_cowplot(base_size) +
    theme(plot.title = element_text(hjust = 0.5))

  # ratio distributions plot
  p3 <- ggplot(data$ratios, aes(value, conc, fill = conc)) +
    geom_density_ridges(scale = 2, alpha = 0.8) +
    scale_x_log10() +
    labs(title = "Distrubution", x = "Ratios log-transformed", 
         y = paste("Treatment", concentration)) +
    theme_cowplot(base_size) +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5))

  # result
  left_plt <- plot_grid(p1, p2, nrow = 2, align = "v", labels = "AUTO")
  p <- plot_grid(left_plt, NULL, p3, labels = c("", "", "C"), ncol = 3, rel_widths = c(1, 0.05, 0.8))
  return(p)
}

save_pdf <- function(p, pdfname = NULL) {
  # save pdf
  if (is.null(pdfname)) {
    pdfname <- sub(".xlsx", ".pdf", file)
    #pdfname <- sub("data/", "", pdfname)
  }
  show(pdfname)

  pdfname <- file.path("img", pdfname)

  pdf(pdfname, width = 9)
  print(p)
  dev.off()
}