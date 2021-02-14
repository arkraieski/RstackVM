library(stringr)
asm <- 
  "push 1;
   pop;
   push 5;
   push 4;
   add;"


asm1 <- str_replace_all(asm, "\n", "")

asm2 <- str_trim(unlist(str_split(asm1, ";")))

as.instructions <- function(asm){
  if(str_detect(asm, "push")){
    
    val <- str_remove(asm, "push ")
    
    return(paste0("push.stack(vm, ", val, ")"))
    
  } else if(str_detect(asm, "pop")) return("pop.stack(vm)")
    else if(str_detect(asm, "add")) return("add.stack(vm)")
    else stop("Invalid instructions")
}

as.instructions("push 1")

assemble <- function(asm){
  asm1 <- str_replace_all(asm, "\n", "")
  
  asm2 <- str_trim(unlist(str_split(asm1, ";")))
  
  asm3 <- asm2[asm2 != ""]
  
  r_instructions <- lapply(asm3, as.instructions)
  
  lapply(r_instructions, function(x){
    compiler::compile(parse(s = substitute(x)))
  })
  
}

test <- assemble(asm)

