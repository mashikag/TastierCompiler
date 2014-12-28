{-# LANGUAGE ScopedTypeVariables, DoAndIfThenElse, OverloadedStrings #-}
module TastierAssembler.Parser where
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Map as M
import qualified Control.Monad.RWS.Lazy as RWS
import qualified TastierMachine.Instructions as I
import Data.Maybe (fromJust)
import Data.Char (isSpace, isAlphaNum)

{-
  parseInstruction takes one single line of text from the assembler file and
  tries to parse the instruction that it contains. If parsing succeeded, the
  result is a (Right I.InstructionWord), and if it failed, the result is a
  (Left [B.ByteString]) representing the tokens on that line. Often parse
  failures are due to legitimate things like jumps to a label, which have to
  be resolved by looking at the symbol table.
-}

parseInstruction :: Int -> B.ByteString ->
                    (Either [B.ByteString] I.InstructionWord)

parseInstruction lineNumber text =
  case B.words text of
    ["Add"]         -> Right $ I.Nullary I.Add
    ["Sub"]         -> Right $ I.Nullary I.Sub
    ["Mul"]         -> Right $ I.Nullary I.Mul
    ["Div"]         -> Right $ I.Nullary I.Div
    ["Equ"]         -> Right $ I.Nullary I.Equ
    ["Lss"]         -> Right $ I.Nullary I.Lss
    ["Gtr"]         -> Right $ I.Nullary I.Gtr
    ["Neg"]         -> Right $ I.Nullary I.Neg
    ["Load", a, b]  ->
      let a' = B.readInteger a
          b' = B.readInteger b
      in
        case (a', b') of
          (Just x, Just y) ->
            Right $ I.Binary I.Load
                    (fromIntegral $ fst x)
                    (fromIntegral $ fst y)
          _ -> Left $ ["Load", a, b]

    ["Sto", a, b]   ->
      let a' = B.readInteger a
          b' = B.readInteger b
      in
        case (a', b') of
          (Just x, Just y) ->
            Right $ I.Binary I.Sto
                    (fromIntegral $ fst x)
                    (fromIntegral $ fst y)
          _ -> Left $ ["Sto", a, b]

    ["LoadG", a]    ->
      let a' = B.readInteger a
      in
        case a' of
          Just x -> Right $ I.Unary I.LoadG (fromIntegral $ fst x)
          _ -> Left $ ["LoadG", a]

    ["StoG", a]     ->
      let a' = B.readInteger a
      in
        case a' of
          Just x -> Right $ I.Unary I.StoG (fromIntegral $ fst x)
          _ -> Left $ ["StoG", a]

    ["Const", a]    ->
      let a' = B.readInteger a
      in
        case a' of
          Just x -> Right $ I.Unary I.Const (fromIntegral $ fst x)
          _ -> error $ "Line " ++ show lineNumber ++ ": Const instruction " ++
                       "requires an immediate integer argument"

    ["Enter", a]    ->
      let a' = B.readInteger a
      in
        case a' of
          Just x -> Right $ I.Unary I.Enter (fromIntegral $ fst x)
          _ -> error $ "Line " ++ show lineNumber ++ ": Enter instruction " ++
                       "requires an immediate integer argument"

    ["Jmp", a]      ->
      case B.readInteger a of
        Just i -> Right $ I.Unary
                          I.Jmp
                          (fromIntegral $ fst i)
        _ -> Left $ ["Jmp", a]

    ["FJmp", a]     ->
      case B.readInteger a of
        Just i -> Right $ I.Unary
                          I.FJmp
                          (fromIntegral $ fst i)
        _ -> Left $ ["FJmp", a]

    ["Call", a, b]  ->
      case B.readInteger b of
        Just i -> Right $ I.Binary I.Call
                          (convert $ B.readInteger a)
                          (convert $ B.readInteger b)
        _ -> Left $ ["Call", a, b]

    ["Ret"]         -> Right $ I.Nullary I.Ret
    ["Leave"]       -> Right $ I.Nullary I.Leave
    ["Read"]        -> Right $ I.Nullary I.Read
    ["Write"]       -> Right $ I.Nullary I.Write
    ["Halt"]        -> Right $ I.Nullary I.Halt
    ["Dup"]         -> Right $ I.Nullary I.Dup
    ["Nop"]         -> Right $ I.Nullary I.Nop
    _               -> error $ "Line " ++ show lineNumber ++ ": " ++
                               "unknown instruction " ++ show text
  where
    convert = fromIntegral . fst . fromJust

{-
  parse processes the file line by line, looking for labels and putting them
  into the symbol table when it finds them. It builds up a list of
  instructions. When an instruction cannot be properly parsed (usually due
  to either jumps to labels that haven't been defined yet (forward jumps) or
  uses of external variables), it just emits the tokens that it tried to
  parse so that someone else can resolve those references. The forward jumps
  are resolved by the function patchLabelAddresses in this file, while uses
  of external variables or calls to external functions have to be resolved
  by the linker.
-}
parse :: RWS.RWS [B.ByteString]
                 [(Either [B.ByteString] I.InstructionWord)]
                 (Int, Int, M.Map B.ByteString Int)
                 ()
parse = do
  (lineNumber, instNumber, symbolTable) <- RWS.get
  sourceCode <- RWS.ask
  if lineNumber > length sourceCode then return ()
  else do
    let currentLine = sourceCode !! (lineNumber-1)
    let mightBeLabelText = B.takeWhile isAlphaNumOrDollar currentLine
    let restOfLine = B.drop (B.length mightBeLabelText) currentLine

    if ((B.length mightBeLabelText) > 0) then --could be a label
      if B.null restOfLine then do --can only be an instruction
        RWS.put (lineNumber+1, instNumber+1, symbolTable)
        RWS.tell $ [parseInstruction instNumber currentLine]
        parse
      else if (B.head restOfLine) == ':' then --definitely a label
        if M.member mightBeLabelText symbolTable then
          error $ "Multiple definitions of the label " ++
                  (show mightBeLabelText) ++
                  " (line " ++ (show $ symbolTable M.! mightBeLabelText) ++
                  ", line " ++ (show lineNumber) ++ ")"
        else do
          RWS.put (lineNumber+1, instNumber+1,
                   M.insert mightBeLabelText instNumber symbolTable)
          RWS.tell $ [parseInstruction instNumber $ B.tail restOfLine]
          parse
      else do
        RWS.put (lineNumber+1, instNumber+1, symbolTable)
        RWS.tell $ [parseInstruction instNumber currentLine]
        parse
    else do
      RWS.put (lineNumber+1, instNumber+1, symbolTable)
      RWS.tell $ [parseInstruction instNumber currentLine]
      parse
  where
    isAlphaNumOrDollar a = (a == '$' || isAlphaNum a)

{-
  patchLabelAddresses takes the symbol table containing all labels defined in
  the file, and tries to resolve all the forward jumps. If there is a jump
  or a call to a label which is not defined *anywhere* in the file, an error
  will be reported with the line number containing the jump or call to the
  nonexistent label.
-}
patchLabelAddresses :: M.Map B.ByteString Int ->
                       [(Int, (Either [B.ByteString] I.InstructionWord))] ->
                       [(Int, (Either [B.ByteString] I.InstructionWord))]

patchLabelAddresses symtab instructions =
  map (patchLabel symtab) instructions
  where
    patchLabel symtab (lineNumber, (Right x)) = (lineNumber, (Right x))
    patchLabel symtab (lineNumber, (Left x)) =
      case x of
        ["Jmp", a]  ->
          if M.member a symtab then
            (lineNumber, (Right $ I.Unary I.Jmp (fromIntegral $ symtab M.! a)))
          else badLabel lineNumber a

        ["FJmp", a] ->
          if M.member a symtab then
            (lineNumber, (Right $ I.Unary I.FJmp (fromIntegral $ symtab M.! a)))
          else badLabel lineNumber a

        ["Call", a, b] ->
          if M.member b symtab then
            (lineNumber, (Right $
                            I.Binary I.Call
                            (fromIntegral $ fst $ fromJust $ B.readInteger a)
                            (fromIntegral $ symtab M.! b)))
          else badLabel lineNumber b
        _ -> error $ "Line " ++ show lineNumber ++ ": Unresolved argument " ++
                     "to instruction " ++ show x

    badLabel lineNumber labelText =
      error $ "Reference to undefined label " ++ (show labelText) ++
              " on line " ++ (show lineNumber)
