
initVM <- function(){
  stack <- vector(mode = "list", length = 5)
  instructions <- list()
  
  
  as.environment(list(stack = stack, instructions = instructions, ip = 1L))
}

push.stack <- function(vm, x){
  vm$stack <- c(x, vm$stack)
  vm
}


pop.stack <- function(vm){
  vm$stack <- vm$stack[2: length(vm$stack)]
  vm
}


add.stack <- function(vm){
  # x <- as.list(vm)$stack[1]
  # vm1 <- pop.stack(vm)
  # 
  # y <- as.list(vm1)$stack[1]
  # vm1 <- pop.stack(vm1)
  
  
  x <- vm$stack[[1]]
  y <- vm$stack[[2]]
  
  vm1 <- pop.stack(vm)
  vm1 <- pop.stack(vm1)
  
  push.stack(vm1, x + y)
}

increment.ip <- function(vm){
  new_ip <- vm$ip + 1L
  
  vm1 <- vm
  vm1$ip <- new_ip
  vm1
}