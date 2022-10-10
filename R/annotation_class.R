#' @import dplyr
#' @export annotation_source
#' @import struct
#' @importFrom methods is new slot slotNames
#' @importFrom stats p.adjust
#' @importFrom utils URLencode read.csv read.table write.table
#' @include zzz.R
annotation_source = function(input_file,tag,add_cols=list(),...) {
    # new object
    out = new_struct('annotation_source',
        input_file=input_file,
        tag=tag,
        add_cols=add_cols,
        ...)
    return(out)
}

.annotation_source<-setClass(
    "annotation_source",
    contains = c('struct_class'),
    slots = c(
        input_file = 'entity',
        annotations = 'entity',
        tag = 'entity',
        id_column = 'character',
        add_cols = 'entity'
    ),
    prototype = list(
        input_file = entity(
            name = 'Input file',
            description = "The file annotations are imported from.",
            value=character(0),
            type='character',
            max_length = 1
        ),
        annotations = entity(
            name = 'Annotations',
            description = 'Data frame of imported annotations.',
            type = 'data.frame'
        ),
        tag = entity(
            name = 'Annotation source id tag.',
            description=paste0('A (short) character string that is appended to ',
                               'columns names when merging this annotation ',
                               'source with other sources. Used to identify ',
                               'columns that are from this source in the merged ',
                               'output.'),
            type='character',
            max_length=1
        ),
        add_cols = entity(
            name='additonal columns',
            description='A list of additional columns to be added to the table and populated with the provided value.',
            type='list',
            max_length=Inf
        ),
        .params=c('input_file','tag','id_column','add_cols'),
        .outputs=c('annotations')
    )
)

# autocompletion, return sample_meta column names
#' @export
#' @method .DollarNames annotation_source
.DollarNames.annotation_source <- function(x, pattern = "") {
    struct:::.DollarNames.struct_class(x,pattern)
}

#' @export 
setMethod('.DollarNames','annotation_source',.DollarNames.annotation_source)

#' @export lcms_source
lcms_source = function(input_file,...) {
    # new object
    out = new_struct('lcms_source',input_file=input_file,...)
    return(out)
}

.lcms_source<-setClass(
    "lcms_source",
    contains = c('annotation_source'),
    slots = c(
        input_file = 'entity',
        annotations = 'entity',
        mz_column = 'character',
        rt_column = 'character',
        id_column='character'
    ),
    prototype = list(
        input_file = entity(
            name = 'Input file',
            description = "The file annotations are imported from.",
            value=character(0),
            type='character',
            max_length = 1
        ),
        annotations = entity(
            name = 'Annotations',
            description = 'Data frame of imported annotations.',
            type = 'data.frame'
        ),
        .params=c('mz_column','rt_column'),
        .outputs=c('annotations')
    )
)

# autocompletion, return sample_meta column names
# @export
# @method .DollarNames lcms_source
#.DollarNames.lcms_source <- function(x, pattern = "") {
#    struct:::.DollarNames.struct_class(x,pattern)
#}

# @export 
#setMethod('.DollarNames','lcms_source',.DollarNames.lcms_source)

#' @export
setMethod(f = 'show',
    signature = c('annotation_source'),
    definition = function(object) {
        
        # print struct generic info
        callNextMethod()
        
        cat('annotations:   ',nrow(object$annotations),' rows x ', ncol(object$annotations),' columns\n',sep='')

    }
)

##### generics
#' @export
setGeneric("import_annotations",function(obj)standardGeneric("import_annotations"))
setGeneric("combine_annotations",function(A,B,...)standardGeneric("combine_annotations"))
#####


make_source = function(A) {
    if (is(A,'lcms_source')) {
        OUT=struct::new_struct('lcms_source')
    } else {
        OUT=struct::new_struct('annotation_source')
    }
    return(OUT)
}

rename_matching = function(a,matching_columns) {
    
    for (k in 1:length(matching_columns)) {
        
        w=which(colnames(a) %in% matching_columns[[k]])
        if (length(w) >= 1) {
            colnames(a)[w[1]] = names(matching_columns)[k]
        }
        if (length(w)>1) {
            warning('combine_annotations: more than one matching column. The first one will be used.')
        }
    }
    return(a)
    
}

do_combine = function(A,B,matching_columns,keep_cols,tag_ids,source_col) {
    
    a = A$annotations
    b = B$annotations
    
    a = lapply(a,as.character)
    a = as.data.frame(a)
    
    b = lapply(b,as.character)
    b = as.data.frame(b)
    
    
    if (tag_ids) {
        if (nrow(a)>0){
            a[[A$id_column]]=paste(A$tag,a[[A$id_column]],sep='_')
        }
        if (nrow(b)>0) {
            b[[B$id_column]]=paste(B$tag,b[[B$id_column]],sep='_')
        }
    }
    
    if (!is.null(matching_columns)) {
        a = rename_matching(a,matching_columns)
        b = rename_matching(b,matching_columns)
    }
    
    
    G=list()
    if (nrow(a) + nrow(b)>0) {
        if (nrow(a)>0) {
            G[[A$tag]]=a
        }
        if (nrow(b)>0) {
            G[[B$tag]]=b
        }
    } else {
        G[[A$tag]]=a
        G[[B$tag]]=b
    }

    


    
    g=dplyr::bind_rows(G,.id=source_col)
    
    w=which((colnames(g) %in% colnames(a) & colnames(g) %in% colnames(b)) | colnames(g) %in% c(keep_cols,source_col))
    if (length(w)<1) {
        stop('must have some matching columns between annotation sources')
    }
    g=g[,w]
    
    return(g)
}

#' @export
setMethod(f = "combine_annotations",
    signature = c("annotation_source","annotation_source"),
    definition = function(A,B,matching_columns=NULL,keep_cols=NULL,tag_ids=FALSE,source_col='annotation_source') {
        
        # force matching for known columns defined by object
        all_matching = list(
            id = c(A$id_column,B$id_column),
        )
        # include the user defined ones
        all_matching = c(all_matching,matching_columns)
        
        g = do_combine(A,B,all_matching,keep_cols,tag_ids,source_col)
        
        OUT = struct::new_struct('annotation_source')
        OUT$annotations = g
        OUT$tag = 'combined'
        OUT$name = 'Combined annotation source'
        OUT$description = 'This annotation_source object was generated by combining two other sources.'
        OUT$id_column = 'id'
        
        return(OUT)
    }
)

#' @export
setMethod(f = "combine_annotations",
    signature = c("list","missing"),
    definition = function(A,B,matching_columns=NULL,keep_cols=NULL,tag_ids=FALSE,source_col='annotation_source') {
        
        J = A[[1]]
        annotation_source = rep(J$tag,nrow(J$annotations))
        for (k in 2:length(A)) {
            B = A[[k]]
            C = combine_annotations(J,B,matching_columns=matching_columns,keep_cols=keep_cols,tag_ids=tag_ids,source_col=source_col)
            
            annotation_source_new = C$annotations[[source_col]]
            annotation_source_new[1:length(annotation_source)] = annotation_source
            annotation_source = annotation_source_new
            
            J = C
        }
        J$annotations[[source_col]] = annotation_source
        return(J)
    })


#' @export
setMethod(f = "combine_annotations",
    signature = c("lcms_source","lcms_source"),
    definition = function(A,B,matching_columns=NULL,keep_cols=NULL,tag_ids=FALSE,source_col='annotation_source') {
        
        # force matching for known columns defined by object
        all_matching=list(
            id = c(A$id_column,B$id_column),
            rt = c(A$rt_column,B$rt_column),
            mz = c(A$mz_column,B$mz_column)
        )
        
        # include the user defined ones
        all_matching = c(all_matching,matching_columns)
        
        g = do_combine(A,B,all_matching,keep_cols,tag_ids,source_col)
        
        OUT = struct::new_struct('lcms_source')
        OUT$annotations = g
        OUT$tag = 'combined'
        OUT$name = 'Combined annotation source'
        OUT$description = 'This annotation_source object was generated by combining two other sources.'
        OUT$id_column = 'id'
        OUT$mz_column = 'mz'
        OUT$rt_column = 'rt'
        
        return(OUT)
    }
)


#' @export
setMethod(f = "import_annotations",
          signature = c("annotation_source"),
          definition = function(obj) {
              stop('no import defined for this annotation source.')
          }
)



#' @export
setMethod(f = "import_annotations",
          signature = c("list"),
          definition = function(obj) {
              
              check = all(unlist(lapply(obj,is,class2='annotation_source')))
              
              if (!check) {
                  stop('All list items must be annotation sources.')
              }
              
              for (k in seq_len(length(obj))) {
                  obj[[k]] = import_annotations(obj[[k]])
              }
              return(obj)
          }
)




#' @export
setMethod(f = "model_apply",
    signature = c("model_seq","annotation_source"),
    definition = function(M,D) {
        # for each method in the list
        S = D # for first in list the input D is the data object
        
        for (i in seq_len(length(M))) {
            if (M[i]@seq_in != 'data') {
                # apply transformation
                S=M[i]@seq_fcn(S)
                # set
                param_value(M[i],M[i]@seq_in)=S
            }
            # use current data
            M[i] = model_apply(M[i],D)
            
            # set the output of this method as the input for the next method
            S = predicted(M[i])
            if (is(S,'annotation_source')) {
                # if its a dataset then update current D
                D = predicted(M[i])
            }
        }
        return(M)
    }
)

#' @export
setMethod(f = "model_apply",
    signature = c("list","list"),
    definition = function(M,D) {
        for (k in 1:length(M)){
            M[[k]] = model_apply(M[[k]],D[[k]])
        }
        return(M)
    }
)




