#' @include annotation_class.R
#' @export beamspy_annotation
beamspy_annotation = function(input_file, tag = 'BEAMSpy',...) {
    # new object
    out = new_struct('beamspy_annotation',
                     input_file=input_file,
                     tag=tag,
                     ...
    )
    return(out)
}


.beamspy_annotation<-setClass(
    "beamspy_annotation",
    contains = c('lcms_table'),
    prototype=
        list(
            rt_column = 'rt',
            mz_column = 'mz',
            id_column = 'id'
        )
)


#' @export
setMethod(f = "import_source",
          signature = c("beamspy_annotation"),
          definition = function(M,...) {
              beams_output <- read.csv(file=obj$input_file, sep="\t")
              rownames(beams_output)=beams_output$name
              obj$annotations=beams_output
              return(obj)
          }
)

