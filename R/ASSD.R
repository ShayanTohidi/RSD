assd.test = function(sd.obj, type){

  if(!is(sd.obj, 'StochasticDominance')){
    stop("Input must be of class 'StochasticDominance'.")
  }

  if(!type %in% c('ll', 'ths')) stop("The type must be 'll' or 'ths'.")

  if(type == 'll') return(assd.ll.test(sd.obj))
  if(type == 'ths') return(assd.ths.test(sd.obj))
}
