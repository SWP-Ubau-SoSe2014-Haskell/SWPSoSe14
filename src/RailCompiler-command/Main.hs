{- |
Module      : Main.hs
Description : .
Maintainer  : (c) Christopher Pockrandt, Nicolas Lehmann, Tobias Kranz
License     : MIT

Stability   : stable

Entrypoint of the rail2llvm-compiler. Contains main-function.

See also:
--https://www.haskell.org/ghc/docs/7.8.2/html/libraries/base-4.7.0.0/System-Console-GetOpt.html
--http://leiffrenzel.de/papers/commandline-options-in-haskell.html (Outdated!)

-}
module Main( main ) where

-- imports --
import System.Environment
import System.Exit
import System.Console.GetOpt
import Data.Maybe( fromMaybe )
import InterfaceDT                   as IDT
import qualified Preprocessor        as PreProc
import qualified Lexer
import qualified SyntacticalAnalysis as SynAna
import qualified SemanticalAnalysis  as SemAna
import qualified IntermediateCode    as InterCode
import qualified Backend

-- defining the option settings for the main-function
data Options = Options  {
    optInput     :: IO String,
    optOutput    :: String -> IO (),
    optASTOutput :: String -> IO (),
    compile      :: Bool,
    impAST       :: Bool,
    expAST       :: Bool
  }

-- default Options
defaultOptions :: Options
defaultOptions   = Options {
    optInput     = getContents,
    optOutput    = putStr,
    optASTOutput = putStr,
    compile      = False,
    impAST       = False,
    expAST       = False
    
  }

-- usageInfo
options :: [OptDescr (Options -> IO Options)]
options = [
    --Option ['V'] ["version"] (NoArg  showVersion)      "show version number",
    Option "h" ["help"     ] (NoArg  showHelp         ) "display Help Text",
    Option "i" ["input"    ] (ReqArg setInput  "FILE" ) "input file (don't set to use stdin')",
    Option "o" ["output"   ] (ReqArg setOutput "FILE" ) "output file (don't set to use stdout')",
    Option "c" ["compile"  ] (NoArg  setCompile       ) "compile 'rail' to 'llvm'",
    Option [ ] ["exportAST"] (OptArg setExpAST "FILE" ) "export frontend AST (don't offer a File to use stdout) \n(dont set with --importAST)",
    Option [ ] ["importAST"] (NoArg setImpAST         ) "import frontend AST and compile to llvm\nautosets -c \n(set input via -i; (dont set with --exportAST)\n\nset -c with --exportAST and get both: the AST and the llvm code"    
  ]

-- output for the help-function
showHelp _ = do
  putStrLn "rail2llvm--haskell-compiler (development version)"
  putStr "https://github.com/SWP-Ubau-SoSe2014-Haskell/SWPSoSe14\n\n"
  putStr $ usageInfo "Usage: main [OPTION...]" options  
  exitSuccess

-- options-getter for optional output for exprotAST
getOut (Just arg) = writeFile arg
getOut Nothing = putStr

-- options-setters
setInput  arg opt = return opt { optInput     = readFile arg }
setOutput arg opt = return opt { optOutput    = writeFile arg }
setExpAST arg opt = return opt { optASTOutput = getOut arg, expAST = True}
setImpAST     opt = return opt { impAST       = True, compile = True }
setCompile    opt = return opt { compile      = True }

-- main-function --
main = do args <- getArgs
          let (actions,nonOpts,msgs) = getOpt RequireOrder options args
          -- intercept errors
          if msgs /= []
          then error $ concat msgs ++ usageInfo "Usage: main [OPTION...]" options
          -- unrecognized arguments error
          else if nonOpts /= [] 
            then error $ "unrecognized arguments: " ++ unwords nonOpts ++ "\nUsage: For basic information, try the `--help' option."
            else do opts <- foldl (>>=) (return defaultOptions) actions
                    -- option aliases
                    let Options { optInput = input, optOutput = output,  optASTOutput = outputAST, compile = cmpl, impAST = imp, expAST = exp} = opts
                    inputWithoutIO <- input
                    -- importAST and exportAST can't be set together (error)
                    if imp && exp 
                    then do error "No export of the imported AST (Usage: For basic information, try the `--help' option.)'"
                            exitSuccess
                    -- compile a file
                    else if cmpl 
                         then do let transform (IBO x) = x
                                 -- compile an imported AST to a LLVM-file
                                 if imp
                                 then transform (Backend.process . InterCode.process . SemAna.process . SynAna.process . Lexer.toAST $ inputWithoutIO) >>= output
                                 -- compile a RAIL-file to an AST as an export-file
                                 else if exp
                                      then do outputAST (Lexer.fromAST . Lexer.process . PreProc.process $ IIP inputWithoutIO)
                                              transform (Backend.process . InterCode.process . SemAna.process . SynAna.process . Lexer.process . PreProc.process $ IIP inputWithoutIO) >>= output
                                      -- compile a RAIL-file to a LLVM-file
                                      else transform (Backend.process . InterCode.process . SemAna.process . SynAna.process . Lexer.process . PreProc.process $ IIP inputWithoutIO) >>= output
                         -- exportAST (without compiling it to llvm)
                         else if exp
                              then outputAST (Lexer.fromAST . Lexer.process . PreProc.process $ IIP inputWithoutIO)
                              -- missing argument error
                              else error "Error. Set atleast -c or --importAST or --exportAST."
