( function () {

    En <- new.env ( parent = baseenv() )

    En $ fromSorted <- function ( X ) {
        `ORIGINAL?` <- ! duplicated ( X )
        nextNumber <- function ( previousNumber , `original?` )
                if ( `original?` ) {
                     1L
                } else {
                    1L + previousNumber }
        Reduce (                              
            accumulate = TRUE ,                                          
            x = `ORIGINAL?` ,
            f = nextNumber ) }

    En $ fromUnsorted <- function ( X ) {
        INFO <- data.frame (
            REORDER = seq_along ( X ) ,
            ORDER   = order     ( X ) )
        INFO <- INFO [ INFO $ ORDER , ]
        INFO <- cbind ( INFO , ENUM = fromSorted ( X [ INFO $ REORDER ] ) )
        INFO [ order ( INFO $ REORDER ) , ] $ ENUM }

    environment ( En $ fromSorted   ) <- En
    environment ( En $ fromUnsorted ) <- En

    Ex <- new.env ( parent = En )

    f <- function ( X ) {
        FUN <- if ( is.unsorted ( X ) )
                fromUnsorted else fromSorted
        FUN ( X ) }

    environment ( f ) <- Ex

    f } ) ()
