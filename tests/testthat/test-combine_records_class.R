test_that("combine records collapses ok", {
    db =data.frame(
        dbid = c('A','B','C'),
        rt = c(10,100,200),
        mz = c(499.99,500,500.01),
        combine_by = c(1,1,1)
    )
    
    AN = lcms_table(
        annotations = db,
        id_column='dbid',
        rt_column='rt',
        mz_column='mz'
    )
    
    M = combine_records(group_by = 'combine_by',
        default_fcn = .collapse(separator = ' || ',na_string = NA),
        fcns = list()
    )
    
    M = model_apply(M,AN)
    
    out=predicted(M)$annotations
    
    expect_equal(nrow(out),1) # all combined into 1 row
    expect_equal(out$dbid[1],'A || B || C')
    
})

test_that("combine records does nothings ok", {
    db =data.frame(
        dbid = c('A','B','C'),
        rt = c(10,100,200),
        mz = c(499.99,500,500.01),
        combine_by = c(1,1,1)
    )
    
    AN = lcms_table(
        annotations = db,
        id_column='dbid',
        rt_column='rt',
        mz_column='mz'
    )
    
    M = combine_records(group_by = 'combine_by',
        default_fcn = .nothing(),
        fcns = list()
    )
    
    M = model_apply(M,AN)
    
    out=predicted(M)$annotations
    
    expect_equal(nrow(out),3) # all combined into 1 row
    expect_equal(out$combine_by,db$combine_by) # all combined into 1 row
})

test_that("combine unique ok", {
    db =data.frame(
        dbid = c('A','B','C'),
        rt = c(10,100,200),
        mz = c(499.99,500,500.01),
        combine_by = c(1,1,1),
        levels = c(1,1,2)
    )
    
    AN = lcms_table(
        annotations = db,
        id_column='dbid',
        rt_column='rt',
        mz_column='mz'
    )
    
    M = combine_records(group_by = 'combine_by',
        default_fcn = .unique(separator = ' || '),
        fcns = list()
    )
    
    M = model_apply(M,AN)
    
    out=predicted(M)$annotations
    
    expect_equal(nrow(out),1) # all combined into 1 row
    expect_equal(out$dbid[1],'A || B || C')
    expect_equal(out$levels,'1 || 2')
})

test_that("combine select_exact ok", {
    db =data.frame(
        dbid = c('A','B','C'),
        rt = c(10,100,200),
        mz = c(499.99,500,500.01),
        combine_by = c(1,1,1),
        levels = c(1,1,2)
    )
    
    AN = lcms_table(
        annotations = db,
        id_column='dbid',
        rt_column='rt',
        mz_column='mz'
    )
    
    M = combine_records(group_by = 'combine_by',
        default_fcn = .select_exact(match_col='levels',match = 1,separator = ' || '),
        fcns = list()
    )
    
    M = model_apply(M,AN)
    
    out=predicted(M)$annotations
    
    expect_equal(nrow(out),1) # all combined into 1 row
    expect_equal(out$dbid[1],'A || B') # only A and B have leve = 1
    expect_equal(out$levels,'1')
})

test_that("combine select_match ok", {
    db =data.frame(
        dbid = c('A','B','C'),
        rt = c(10,100,200),
        mz = c(499.99,500,500.01),
        combine_by = c('1','1','1'),
        levels = c('1','1','2'),
        levels2=c('2','1','1')
    )
    
    AN = lcms_table(
        annotations = db,
        id_column='dbid',
        rt_column='rt',
        mz_column='mz'
    )
    
    M = combine_records(group_by = 'combine_by',
        default_fcn = .select_match(match_col='levels',search_col = "levels2",separator = ' || '),
        fcns = list()
    )
    
    M = model_apply(M,AN)
    
    out=predicted(M)$annotations
    
    expect_equal(nrow(out),1) # all combined into 1 row
    expect_equal(out$dbid[1],'B') # only B has level and level2 equal 1
    expect_equal(out$levels,'1')
})


test_that("combine select_min ok", {
    db =data.frame(
        dbid = c('A','B','C'),
        rt = c(10,100,200),
        mz = c(499.99,500,500.01),
        combine_by = c('1','1','1'),
        levels = c('1','1','2'),
        levels2=c('2','1','1')
    )
    
    AN = lcms_table(
        annotations = db,
        id_column='dbid',
        rt_column='rt',
        mz_column='mz'
    )
    
    M = combine_records(group_by = 'combine_by',
        default_fcn = .select_min(min_col='rt'),
        fcns = list()
    )
    
    M = model_apply(M,AN)
    
    out=predicted(M)$annotations
    
    expect_equal(nrow(out),1) # all combined into 1 row
    expect_equal(out$dbid[1],'A') # A has min rt
})

test_that("combine select_max ok", {
    db =data.frame(
        dbid = c('A','B','C'),
        rt = c(10,100,200),
        mz = c(499.99,500,500.01),
        combine_by = c('1','1','1'),
        levels = c('1','1','2'),
        levels2=c('2','1','1')
    )
    
    AN = lcms_table(
        annotations = db,
        id_column='dbid',
        rt_column='rt',
        mz_column='mz'
    )
    
    M = combine_records(group_by = 'combine_by',
        default_fcn = .select_max(max_col='rt'),
        fcns = list()
    )
    
    M = model_apply(M,AN)
    
    out=predicted(M)$annotations
    
    expect_equal(nrow(out),1) # all combined into 1 row
    expect_equal(out$dbid[1],'C') # C has max rt
})

test_that("combine mean median mode ok", {
    db =data.frame(
        dbid = c('A','B','C'),
        rt = c(10,100,200),
        mz = c(499.99,500,500.01),
        combine_by = c('1','1','1'),
        levels = c(1,1,2),
        levels2=c('2','1','1')
    )
    
    AN = lcms_table(
        annotations = db,
        id_column='dbid',
        rt_column='rt',
        mz_column='mz'
    )
    
    M = combine_records(group_by = 'combine_by',
        default_fcn = .collapse(separator = ' || '),
        fcns = list('rt' = .median(),
                    'mz' = .mean(),
                    'levels' = .mode()
        )
    )
    
    M = model_apply(M,AN)
    
    out=predicted(M)$annotations
    
    expect_equal(out$dbid[1],'A || B || C')
    expect_equal(out$rt,median(out$rt))
    expect_equal(out$mz,mean(out$mz))
    expect_equal(out$levels,1)
})

test_that("combine records prioritise ok", {
    db =data.frame(
        dbid = c('A','B','C'),
        rt = c(10,100,200),
        mz = c(499.99,500,500.01),
        combine_by = c('1','1','2'),
        levels = c("1","2","3"),
        levels2=c('2','1','3')
    )
    
    AN = lcms_table(
        annotations = db,
        id_column='dbid',
        rt_column='rt',
        mz_column='mz'
    )
    
    M = combine_records(group_by = 'combine_by',
        default_fcn = .prioritise(
            separator = NULL,
            match_col = 'levels',
            priority = c('2','1'),
            no_match = NA,
            na_string = NA),
    )
    
    M = model_apply(M,AN)
    
    out=predicted(M)$annotations
    
    # level 2 prioritised over 1 for combine_by == 1
    expect_equal(out$dbid[1],'B')
    expect_equal(out$levels[1],"2") 
    # NA if no matches to priority list
    expect_setequal(unlist(out[2,]),c('2',rep(NA,5)))
    
   
    
})

test_that("combine prioritise collapse ok", {
    db =data.frame(
        dbid = c('A','B','C'),
        rt = c(10,100,200),
        mz = c(499.99,500,500.01),
        combine_by = c('1','1','1'),
        levels = c(1,1,2),
        levels2=c('2',NA,'1')
    )
    
    AN = lcms_table(
        annotations = db,
        id_column='dbid',
        rt_column='rt',
        mz_column='mz'
    )
    
    M = combine_records(group_by = 'combine_by',
        default_fcn = .prioritise(
            separator = ' || ',
            match_col = 'levels',
            priority = c('1','2'),
            no_match = NA,
            na_string = "NA"),
    )
    
    M = model_apply(M,AN)
    
    out=predicted(M)$annotations
    
    expect_equal(out$dbid[1],'A || B') # check collapsed expected rows
    expect_equal(out$levels,"1") # check priority 1
    expect_equal(out$levels2,c("2 || NA")) # check NA string replacement
})

