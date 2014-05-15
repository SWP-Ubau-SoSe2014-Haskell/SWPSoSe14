module SemanticalAnalysis (
                            process   -- main function of the module "SemanticalAnalysis"
					       )
 where
 
 -- imports --
 import InterfaceDT as IDT
 
 -- functions --
 process :: IDT.SynAna2SemAna -> IDT.SemAna2InterCode
 process (IDT.ISS input) = IDT.ISI output
  where
   output = input
