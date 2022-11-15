#' @eval get_description('BEAMSpy_source')
#' @include import_annotation_class.R
#' @export BEAMSpy_source
BEAMSpy_source = function(
        input_file,
        add_cols=list(),
        tag = 'BEAMSpy',
        ...) {
    # new object
    out = new_struct(
        'BEAMSpy_source',
        input_file = input_file,
        add_cols=add_cols,
        tag=tag,
        ...
    )
    return(out)
}


.BEAMSpy_source<-setClass(
    "BEAMSpy_source",
    contains = c('lcms_source')
)

#' @export
setMethod(f = "model_apply",
    signature = c("BEAMSpy_source","lcms_table"),
    definition = function(M,D) {
        
        # read in the file
        MF = read.table(
            file = M$input_file,sep = '\t',
            header=TRUE)
        
        # add ids
        MF$id = as.character(1:nrow(MF))
        
        # add extra columns if requested
        if (length(M$add_cols)>0){
            for (g in 1:length(M$add_cols)) {
                MF[[names(M$add_cols)[g]]]=M$add_cols[[g]]
            }
        }
        
        D$annotations=MF
        
        D$mz_column='mz'
        D$tag = M$tag
        D$rt_column='rt'
        D$id_column='id'
        
        M$imported=D
        
        return(M)
    }
)


