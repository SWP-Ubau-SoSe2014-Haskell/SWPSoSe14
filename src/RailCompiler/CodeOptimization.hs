module CodeOptimization (
                         process   -- main function of the module "CodeOptimization"
					    )
 where
 
 -- imports --
 import InterfaceDT as IDT
 import ErrorHandling as EH
 
 -- functions --
 process :: IDT.InterCode2CodeOpt -> IDT.CodeOpt2Backend
 process (IDT.IIC input) = IDT.ICB output
  where
   output = input
