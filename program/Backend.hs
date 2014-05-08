module Backend (
                process   -- main function of the module "Backend"
               )
 where
 
 -- imports --
 import InterfaceDT as IDT
 
 -- functions --
 process :: IDT.CodeOpt2Backend -> IDT.Backend2Output
 process input = output
  where
   output = IDT.IBO $ show input
