{-# LANGUAGE ScopedTypeVariables, DoAndIfThenElse #-}
module TastierMachine.Bytecode where
import qualified TastierMachine.Instructions as Instructions
import qualified Data.Binary.Get as G
import qualified Data.Binary.Put as P

load :: G.Get [Instructions.InstructionWord]
load = do
  empty <- G.isEmpty
  if empty then
    return []
  else do
    opcode <- G.getWord8
    let inst = toEnum $ fromIntegral $ fromEnum $ opcode
    let arity = Instructions.arguments inst
    case arity of
      0 -> do
        rest <- load
        return ((Instructions.Nullary inst) : rest)

      1 -> do
        arg <- G.getWord16be
        rest <- load
        return ((Instructions.Unary inst
                (toEnum $ fromIntegral $ fromEnum $ arg)) : rest)

      2 -> do
        arg0 <- G.getWord16be
        arg1 <- G.getWord16be
        rest <- load
        return ((Instructions.Binary inst
                (toEnum $ fromIntegral $ fromEnum $ arg0)
                (toEnum $ fromIntegral $ fromEnum $ arg1)) : rest)

save :: [Instructions.InstructionWord] -> P.Put
save [] = return ()
save (i:rest) = do
  case i of
    (Instructions.Nullary inst) -> do
      P.putWord8 $ fromInteger $ fromIntegral $ fromEnum inst
      save rest

    (Instructions.Unary inst a) -> do
      P.putWord8 $ fromInteger $ fromIntegral $ fromEnum inst
      P.putWord16be $ fromInteger $ fromIntegral $ fromEnum a
      save rest

    (Instructions.Binary inst a b) -> do
      P.putWord8 $ fromInteger $ fromIntegral $ fromEnum inst
      P.putWord16be $ fromInteger $ fromIntegral $ fromEnum a
      P.putWord16be $ fromInteger $ fromIntegral $ fromEnum b
      save rest
