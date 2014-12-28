{-# LANGUAGE ScopedTypeVariables, DoAndIfThenElse, OverloadedStrings #-}
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Maybe (fromJust)
import qualified Data.Map as M
import qualified Data.List as L

data Symbol = Procedure Bool B.ByteString
            | Variable Bool Int B.ByteString
            deriving (Show, Eq)

type SymbolTable = M.Map B.ByteString (Symbol, Int)

type Instruction = [B.ByteString]

--Helper functions
isHeaderLine = B.isPrefixOf "."

isVariable (Variable _ _ _) = True
isVariable _ = False

isExternal (Variable a _ _) = a
isExternal (Procedure a _) = a

isInternalVariable v = (not $ isExternal v) && (isVariable v)

getName (Variable _ _ a) = a
getName (Procedure _ a) = a

numGlobalsExported :: SymbolTable -> Int
numGlobalsExported t = length $ M.elems $ M.filter (isInternalVariable . fst) t

dumpAssembler :: [[Instruction]] -> B.ByteString
dumpAssembler files =
  let smushedFiles = map smush files
  in B.intercalate "\n" smushedFiles
  where
    smush file = B.intercalate "\n" $ map (B.intercalate " ") file

parseSymbol :: B.ByteString -> Symbol
parseSymbol s =
  let chunks = B.words s
  in
    if (length chunks > 0) then
      case (head chunks) of
        ".var" -> Variable False
                  (convert $ B.readInteger $ (chunks !! 1))
                  (chunks !! 2)
        ".proc" -> Procedure False (chunks !! 1)
        ".external" ->
          case (chunks !! 1) of
            "var" -> Variable True
                     (convert $ B.readInteger $ (chunks !! 2)) (chunks !! 3)
            "proc" -> Procedure True (chunks !! 2)
            _ -> error $ "Could not parse " ++ (show s) ++ " as a symbol"
        _ -> error $ "Could not parse " ++ (show s) ++ " as a symbol"
    else error $ "Could not parse " ++ (show s) ++ " as a symbol"
  where
    convert = fromIntegral . fst . fromJust

buildTableForFile :: String -> IO SymbolTable
buildTableForFile fname = do
  assemblerFile <- B.readFile fname
  let assemblerFileLines = filter (not . B.null) $ B.lines assemblerFile
  let numberOfSymbols = head assemblerFileLines
  let headerLines = filter isHeaderLine (tail assemblerFileLines)
  let symbols = map parseSymbol headerLines
  let internalVariableSymbols = filter isInternalVariable symbols
  let addresses = [3..((length internalVariableSymbols)+3)]
  let addressedSymbols = zip (reverse internalVariableSymbols) addresses
  let otherSymbols = (symbols L.\\ internalVariableSymbols)
  let dummySymbols = zip otherSymbols (replicate (length otherSymbols) (-1))
  let totalSymbols = addressedSymbols ++ dummySymbols
  let names = map (getName . fst) totalSymbols
  return $ M.fromList $ zip names totalSymbols

buildProgramForFile :: String -> IO [Instruction]
buildProgramForFile fname = do
  assemblerFile <- B.readFile fname
  let assemblerFileLines = filter (not . B.null) $ B.lines assemblerFile
  let codeLines = filter (not . isHeaderLine) assemblerFileLines
  return $ map B.words codeLines

buildGlobalIncrements :: [SymbolTable] -> [Int]
buildGlobalIncrements tables =
  let numbers = map numGlobalsExported tables
      takeAmounts = [0..((length numbers)-1)]
      sums = map ($numbers) (map take takeAmounts)
  in map sum sums

replaceLoadsAndStores :: [Instruction] -> B.ByteString -> B.ByteString -> [Instruction]
replaceLoadsAndStores program oldValue newValue =
  map (update oldValue newValue) program
  where
    update old new instruction =
      case (instruction !! 0) of
        "LoadG" ->
          if (instruction !! 1) == oldValue then
            ["LoadG", newValue]
          else instruction
        "StoG" ->
          if (instruction !! 1) == oldValue then
            ["StoG", newValue]
          else instruction
        _ -> instruction


{-
  Uniqify: increment the address of every global load and store
-}

uniqify :: ([Instruction], SymbolTable) -> Int -> ([Instruction], SymbolTable)
uniqify (code, table) increment =
  let exportedGlobals = M.elems $ M.filter (isInternalVariable . fst) table
      addresses = map snd exportedGlobals
      newAddresses = map (+increment) addresses
      fixedProgram = doRecurse code addresses newAddresses
      newExportedGlobals = map (updateEntry (M.fromList $ zip addresses newAddresses)) exportedGlobals
      names = map (getName . fst) newExportedGlobals
      newTableEntries = M.fromList $ zip names newExportedGlobals
      fixedTable = M.union newTableEntries table
  in (fixedProgram, fixedTable)
  where
    doRecurse code [] [] = code
    doRecurse code old new =
      doRecurse (replaceLoadsAndStores code
                  (B.pack $ show $ head old)
                  (B.pack $ show $ head new))
                (tail old)
                (tail new)

    updateEntry mapping (a, i) = (a, (mapping M.! i))


{-
  The reason we do (tail linkFiles) in here is that we don't want to uniqify
  the first linkFile, because we assume that is the point of origin of the
  increments, so we can't change any addresses in there. We also discard the
  first increment, which is always going to be zero for the same reason.
-}

uniqifyLoadsAndStores :: [([Instruction], SymbolTable)] -> [Int] -> [([Instruction], SymbolTable)]
uniqifyLoadsAndStores linkFiles increments = zipWith uniqify (tail linkFiles) (tail increments)

{-
  Steps required:
    1. Uniqify all load and store addresses for non-external globals by incrementing everything
      1. build for each file the increment for variable locations for that file

    2. For each external variable in each file
      1. Find the file which declares it (or error)
      2. Get the (now unique) address of that global
      3. Replace all loads and stores to the external variable name in the current file with that address

    3. Uniquify all loop labels by incrementing everything

    functions required:

    numloops(file)
    numGlobalsExported(file)
    replaceLoadsStores(file, ext_name/address, newaddress)
-}

main = do
  args <- getArgs
  if length args > 1 then do
    tables <- mapM buildTableForFile args
    files <- mapM buildProgramForFile args
    let filesAndTables = zip files tables
    let globalIncrements = buildGlobalIncrements tables
    let uniqified = uniqifyLoadsAndStores filesAndTables globalIncrements
    --putStrLn $ concatMap show tables
    --putStrLn $ concatMap show $ map numGlobalsExported tables
    --mapM_ (putStrLn . show) files
    --mapM_ (putStrLn . show) globalIncrements
    --mapM_ (putStrLn . show) uniqified
    let outputCode = [(head files)] ++ (map fst uniqified)
    putStrLn $ B.unpack $ dumpAssembler outputCode
  else do
    error $ "Usage: tld <1.asm> <2.asm> ... <n.asm> <output.asm>"
