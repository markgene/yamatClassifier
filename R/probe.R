# Get probes of interest


#' Update probes of interest.
#'
#' If probe set is not set for \code{YamatClassifierTrainer}, check if a Rda
#' file exists for probe set. If exists, load Rda file. Otherwise, get probe IDs
#' from scratch. The probe set has:
#'   \itemize{
#'     \item Drop SNP.
#'     \item Drop loci on chr X or Y..
#'     \item Common between the array types of trainer and EPIC v2
#'   }
#'
#' If probe set is set, returns it and meanwhile save it in Rda file if the
#' file does not exist.
#'
#' @param trainer A S3 object of \code{YamatClassifierTrainer} class.
#' @param chip_type_name column contains the chip type name.
#' @param present_by_epic_v2 bool if only include probes present in EPIC v2.
#' @return a list of probe CG IDs.
#' @export
get_probes <- function(trainer,
                       chip_type_name = NULL,
                       present_by_epic_v2 = TRUE) {
  probes_rda <- get_probes_rda(trainer)
  if (is.null(trainer$probes)) {
    if (file.exists(probes_rda)) {
      logger::log_info("Reading existing probes Rda file")
      load(probes_rda)
    } else {
      logger::log_info("Getting probes from scratch")
      probes <- get_probes_from_scratch(
        trainer = trainer,
        chip_type_name = chip_type_name,
        present_by_epic_v2 = present_by_epic_v2
      )
      save(probes, file = probes_rda)
    }
  } else {
    probes <- trainer$probes
    if (!file.exists(probes_rda)) {
      save(probes, file = probes_rda)
    }
  }
  return(probes)
}


#' Get probes of interest
#'
#' The probe set has:
#'   \itemize{
#'     \item Drop SNP.
#'     \item Drop loci on chr X or Y..
#'     \item Common between the array types of trainer and EPIC v2
#'   }
#'
#' @param trainer A S3 object of \code{YamatClassifierTrainer} class.
#' @param chip_type_name column contains the chip type name.
#' @param present_by_epic_v2 bool if only include probes present in EPIC v2.
#' @return a list of probe CG IDs.
#' @export
get_probes_from_scratch <- function(trainer,
                                    chip_type_name = NULL,
                                    present_by_epic_v2 = TRUE) {
  targets <- get_targets(trainer)
  preprocessed_dir <- get_preprocessed_dir(trainer)
  if (!is.null(chip_type_name) &&
      chip_type_name %in% colnames(targets)) {
    logger::log_debug("Get probe set by chip type")
    chip_types <- unique(targets[, chip_type_name])
    probe_sets <- lapply(chip_types, function(chip_type) {
      sentrix_id <- targets$Sentrix_ID[targets[, chip_type_name] == chip_type][1]
      rda_file_name <- paste0(sentrix_id, ".Rda")
      mset_rda <- file.path(preprocessed_dir, rda_file_name)
      logger::log_debug(glue::glue(
        "Load preprocessed MethySet {sentrix_id} of type {chip_type}"
      ))
      load(mset_rda)
      get_and_filter_probes(mset = mset)
    })
    probes <- Reduce(intersect, probe_sets)
  } else {
    logger::log_debug("Get probe set from first sample")
    sentrix_id <- targets$Sentrix_ID[1]
    rda_file_name <- paste0(sentrix_id, ".Rda")
    mset_rda <- file.path(preprocessed_dir, rda_file_name)
    logger::log_debug(glue::glue("Load preprocessed MethySet {sentrix_id} of first sample"))
    load(mset_rda)
    probes <- get_and_filter_probes(mset = mset)
  }
  if (present_by_epic_v2) {
    logger::log_debug("Load EPIC v2 and filter the probes present in EPIC v2")
    require(IlluminaHumanMethylationEPICv2anno.20a1.hg38)
    anno_epic_v2 <- minfi::getAnnotation(IlluminaHumanMethylationEPICv2anno.20a1.hg38)
    probes_900k <- unique(anno_epic_v2$EPICv1_Loci)
    probes <- intersect(probes, probes_900k)
  }
  return(probes)
}


#' Get and filter probes.
#'
#' @param mset \code{\link[minfi]{MethySet}} object.
#' @return a list of probe IDs.
get_and_filter_probes <- function(mset) {
  gmset <- minfi::mapToGenome(mset)
  gmset_flt <- minfi::dropLociWithSnps(gmset, snps = c("CpG", "SBE"))
  # tag sex chromosome probes for removal
  anno <- minfi::getAnnotation(gmset_flt)
  keep <- !(featureNames(gmset_flt) %in% anno$Name[anno$chr %in%
                                                     c("chrX", "chrY")])
  gmset_flt <- gmset_flt[keep, ]
  probes <- rownames(gmset_flt)
  return(probes)
}
