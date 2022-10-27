#' @eval get_description('pubchem_lookup')
#' @export
#' @include annotation_class.R
pubchem_lookup = function(
    annotation_column,
    out_column = 'cid',
    search_by = 'name',
    return_value = 'cids',
    property = 'InChIKey',
    ...) {
    
    allowed=c('MolecularFormula','MolecularWeight','CanonicalSMILES',
        'IsomericSMILES','InChI','InChIKey','IUPACName','Title','XLogP',
        'ExactMass','MonoisotopicMass','TPSA','Complexity','Charge','CID')
    
    if (!all(property %in% allowed)) {
        stop('One of the properties is not a valid choice. Allowed values are: ', paste0(allowed,collapse=', '))
    }
    
    if (length(out_column) != length(property)){
        stop('An output column must be specified for each property.')
    }
    
    out=struct::new_struct('pubchem_lookup',
        annotation_column = annotation_column,
        out_column = out_column,
        search_by=search_by,
        return_value=return_value,
        property=property,
        ...)
    
    return(out)
}



.pubchem_lookup<-setClass(
    "pubchem_lookup",
    contains = c('model'),
    slots=c(
        annotation_column = 'entity',
        out_column = 'entity',
        updated='entity',
        search_by='enum',
        return_value='enum',
        property='entity'
    ),
    
    prototype=list(
        name = 'Compound ID lookup via pubchem',
        description = paste0('Uses the PubChem API to search for CID based on the input annotation column.'),
        type = 'univariate',
        predicted = 'updated',
        .params=c('annotation_column','out_column','search_by','return_value','property'),
        .outputs=c('updated'),
        annotation_column = entity(
            name = 'Annotation column name',
            description ='The column name to use as the reference for searching the database e.g. "HMBD_ID". If length > 1, then priority is given to columns left-to-right.',
            type=c('character'),
            value='V1',
            max_length=Inf
        ),
        out_column = entity(
            name = 'Output column name',
            description ='The name of the column to store the returned value for the annotations.',
            type=c('character'),
            value='',
            max_length=Inf
        ),
        updated=entity(
            name='Updated annotations',
            description = 'The updated annotation source.',
            type='annotation_table'
        ),
        search_by=enum(
            name='Search by term',
            description='The PubChem domain to search for matches to the annotation_column',
            allowed=c('cid','name','smiles','inchi','sdf','inchikey','formula'),
            value='cid'
        ),
        return_value = enum(
            name = 'Return value',
            description = 'The values to return if a match is found.',
            allowed=c('cids','property'),
            value='cids'
        ),
        property = entity(
            name = 'Property value',
            description = 'Property value to return if a match is found, and return_value = "property"',
            value='InChI',
            max_length=Inf
        )
    )
)


#' @export
setMethod(f="model_apply",
    signature=c("pubchem_lookup","annotation_table"),
    definition=function(M,D) {
        X = D$annotations
        
        # use a cache to stop multiple requests for the same search term
        pubchem_cache=list()
        
        # get ready
        httr::set_config(httr::config(http_version = 2))
        
        # for each annotation
        OUT=list()
        for (k in 1:nrow(X)) {
            x=X[k,]
            
            # if NA, try other columns
            for (j in M$annotation_column) {
                if (is.na(x[[j]])) {
                    # if NA, try next identifier
                    cid = NA
                    next
                }
                
                # check the cache
                w = which(names(pubchem_cache) == x[[j]])
                # if in the cache, use the previous results
                if (length(w)>0) {
                    cid = pubchem_cache[[w]]
                } else {
                    cid = structReportsPCB:::.query_pubchem(x = x[[j]],search_by=M$search_by,return_value=M$return_value,property=M$property)  
                    }
                
                if (is.null(cid)){ # no hits :(
                    cid = rep(NA,length(M$out_column))
                }
                
                # update cache
                pubchem_cache[[x[[j]]]]=cid
                
                # if we got a cid, next annotation, otherwise try next id
                if (!all(is.na(cid))) {
                    break
                }
                
            }
            
            # store the cid
            OUT[[k]]=cid
        }
        
        out=do.call(rbind,OUT)
        out[out=='NA']=NA
        colnames(out)=M$out_column
        X=cbind(X,out)
        
        D$annotations=X
        
        M$updated=D
        
        return(M)
    }
)


.query_pubchem = function(x,search_by,return_value,property){
    
    # remove CID from url
    property2=property
    if ('CID' %in% property){
        w=which(property=='CID')
        property=property[-w]
        if(length(property)==0){
            property='InChIKey' # dummy request to get CID
        }
    }
    
    # build the url
    if (return_value=='property'){
        url=paste0('https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/',URLencode(search_by),'/',URLencode(x),'/',URLencode(return_value),'/',URLencode(paste0(property,collapse=',')),'/JSON')
    } else {
        url=paste0('https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/',URLencode(search_by),'/',URLencode(x),'/',URLencode(return_value),'/JSON')
    }

    # try 3 times in case of time out
    failed=TRUE # flag to detect api failure
    for (attempt in 1:3) {
        response = httr::GET(url)
        
        Sys.sleep(0.5) # short delay so we dont overwhelm the api
        
        if (response$status_code<405) { # 404 if compound not found
            failed=FALSE
            break  # if no error then dont try again
        }
    }
    
    if (failed) {
        stop('pubchem api failed after 3 attempts')
    }
    
    # parse the results
    if (response$status_code!=200) { # no match, so NA
        return(NULL)
    } 
    
    response = httr::content(response,as = 'text',encoding = 'UTF-8')
    J = jsonlite::fromJSON(response)
    
    
    if (return_value=='cids') {
        # get cid
        cid=data.frame(cids=J$IdentifierList$CID[1]) # take first (best) match
    } else if (return_value=='property') {
        # check columns
        w=which(!(property2 %in% colnames(J$PropertyTable$Properties)))
        if (length(w)>0) {
            J$PropertyTable$Properties[,property2[w]]=NA
        }
        cid=J$PropertyTable$Properties[1,property2,drop=FALSE]
    }
    
    
    return(cid)
}

