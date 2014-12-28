{-# LANGUAGE ScopedTypeVariables, DoAndIfThenElse #-}
module Main where
import qualified TastierMachine.Machine as Machine
import qualified TastierMachine.Instructions as Instructions
import qualified TastierMachine.Bytecode as Bytecode
import Data.Array (Array, listArray)
import Data.Int (Int16)
import qualified Data.Binary.Get as G
import qualified Data.Binary.Put as P
import qualified Data.ByteString.Lazy.Char8 as B
import Control.Monad.RWS.Lazy (execRWS)
import System.Environment (getArgs)
import Data.Maybe (fromJust)

machine = (Machine.Machine 0 0 0
          (listArray (0,0) [Instructions.Nullary Instructions.Halt])
          (listArray (0,4091) (take 4092 $ cycle [0]))
          (listArray (0,4095) (take 4096 $ cycle [0])))

main = do
  args <- getArgs
  if length args == 2 then do
    bytecodeFile <- B.readFile (args !! 0)
    dataFile <- B.readFile (args !! 1)
    let insns = G.runGet Bytecode.load bytecodeFile
    let inputLines = filter (not . B.null) $ B.lines dataFile
    let inputData = map getFirstIntegerFromString inputLines
    let program = (listArray (0, fromIntegral $ (length insns)-1) insns)
    let machine' = machine { Machine.imem = program }
    let (machine'', output) = execRWS Machine.run inputData machine'
    machine''' <- Machine.debug' machine''
    putStrLn $ show output
  else
    error $ "Usage: tvm <input bytecode file> <input data file>"
  where
    getFirstIntegerFromString =
      (fromIntegral . fst . fromJust . B.readInteger . head . B.words)
