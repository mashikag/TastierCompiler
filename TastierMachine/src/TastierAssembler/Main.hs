{-# LANGUAGE ScopedTypeVariables, DoAndIfThenElse, OverloadedStrings #-}
module Main where
import qualified TastierMachine.Machine as Machine
import qualified TastierMachine.Instructions as I
import qualified TastierMachine.Bytecode as Bytecode
import TastierAssembler.Parser
import Data.Array (Array, listArray)
import Data.Int (Int16)
import Data.Char (isSpace, isAlphaNum)
import qualified Data.Binary.Get as G
import qualified Data.Binary.Put as P
import qualified Data.ByteString.Lazy.Char8 as B
import Control.Monad.RWS.Lazy (execRWS)
import System.Environment (getArgs)
import Data.Maybe (fromJust)
import qualified Data.Map as M
import qualified Control.Monad.RWS.Lazy as RWS

commentOpenString = ";;"
eformatMarker = "."

ignoreLinePredicate l = (not $ B.null l) &&
                        (not $ B.isPrefixOf commentOpenString l) &&
                        (not $ B.isPrefixOf eformatMarker l)

flatten x =
  case x of
    (i, Right a) -> a
    (i, Left b) -> error $ "Line " ++ show i ++
                           ": cannot assemble unresolved instruction " ++ show b

main = do
  args <- getArgs
  if length args == 2 then do
    assemblerFile' <- B.readFile (args !! 0)

    let assemblerFile = "Call 0 Main\nJmp $END\n" `B.append` assemblerFile'
                        `B.append` "\n$END: Halt\n"

    let chunks = filter ignoreLinePredicate $
                 map (B.dropWhile isSpace) $
                 B.lines assemblerFile

    let ((lines, insts, symtab), instructions) = RWS.execRWS
                                                 parse
                                                 chunks
                                                 (1, 0, M.empty)

    let instructions' = patchLabelAddresses symtab
                        (zip [1..length instructions] instructions)

    let instructions'' = map flatten instructions'

    B.writeFile (args !! 1) $ P.runPut $ Bytecode.save instructions''
  else
    error $ "Usage: tasm <input assembler file> <output bytecode file>"
