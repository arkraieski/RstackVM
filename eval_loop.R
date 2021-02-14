run.program <- function(instructions){
  vm <- initVM(program_length = length(instructions))
  
  vm <- load.program(vm, instructions)
  
  while(vm$ip <= length(instructions)){
    vm2 <- eval(vm$instructions[[vm$ip]])
    vm <- increment.ip(vm2)
    }
  
  vm$stack[[1]]
}



