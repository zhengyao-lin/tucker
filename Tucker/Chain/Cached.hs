module Tucker.Chain.Cached where

import Data.List
import qualified Data.ByteString as BSR

import Control.Monad
import Control.Monad.Loops

import Control.Exception

import Text.Regex.Posix

import Debug.Trace

import System.IO
import System.FilePath
import System.Directory

import Tucker.Enc
import Tucker.Msg
import Tucker.Conf
import Tucker.Atom
import Tucker.Error

import Tucker.Chain.Object

type ChunkNumber = Word

data BlockTreeChunk = BlockTreeChunk {
        chunk_no    :: ChunkNumber,
        -- file_handle :: Handle,
        file_path   :: FilePath,

        altered     :: Atom Bool, -- whether the part in mem differs from the disk
        cache_part  :: Atom (Either BlockTreePartInfo BlockTreePart)
                             -- Left if not cached in mem
    }

instance Show BlockTreeChunk where
    show (BlockTreeChunk {
        chunk_no = num,
        file_path = path
    }) = "BlockTreeChunk " ++
         "{ chunk_no = " ++ show num ++
         ", path = " ++ show path ++
         " }"

data BlockTreeCached = BlockTreeCached {
        -- cache_count :: Int, -- cache hottest n chunks
        base_dir    :: FilePath,
        chunks      :: [BlockTreeChunk]
    } deriving (Show)

data SearchOption = SearchOption {
        max_depth :: Int -- rounded to the nearest chunk
    } deriving (Show)

isValidTreeCached :: BlockTreeCached -> Bool
isValidTreeCached (BlockTreeCached {
    chunks = chunks
}) =
    not $ null chunks

treeCachedHeight :: BlockTreeCached -> IO TreeHeight
treeCachedHeight (BlockTreeCached {
    chunks = chunks
}) = mapM treeChunkHeight chunks >>= (return . sum)

decodeTreePart :: ByteString -> IO BlockTreePart
decodeTreePart bs =
    case decodeLE bs of
        (Left err, _) ->
            throw $ wrapError err "fail to parse btp file"
   
        (Right part, _) -> return part

treeChunkHeight :: BlockTreeChunk -> IO TreeHeight
treeChunkHeight (BlockTreeChunk {
    cache_part = cache_part
}) = do
    cached <- getA cache_part

    case cached of
        Left info -> return $ tree_height info
        Right part -> return $ treePartHeight part

treeChunkNumber :: BlockTreeChunk -> ChunkNumber
treeChunkNumber (BlockTreeChunk { chunk_no = chunk_no }) = chunk_no

emptyTreeChunk :: ChunkNumber -> FilePath -> IO BlockTreeChunk
emptyTreeChunk chunk_no file_path = do
    let init_part = emptyTreePart

    handle <- openBinaryFile file_path ReadWriteMode
    BSR.hPutStr handle (encodeLE init_part)
    hClose handle

    altered <- newA False
    cache_part <- newA (Right init_part)

    return $ BlockTreeChunk {
        chunk_no = chunk_no,
        -- file_handle = handle,
        file_path = file_path,

        altered = altered,
        cache_part = cache_part
        -- an empty part is small, no problem to cache in mem
    }

treeChunkFromFile :: ChunkNumber -> FilePath -> IO BlockTreeChunk
treeChunkFromFile chunk_no file_path = do
    part <- BSR.readFile file_path >>= decodeTreePart

    let info = treePartInfo part

    altered <- newA False
    cache_part <- newA (Left info)

    putStrLn $ "finished parsing " ++ file_path ++ ": " ++ show info

    return $ BlockTreeChunk {
        chunk_no = chunk_no,
        -- file_handle = handle,
        file_path = file_path,

        altered = altered,
        cache_part = cache_part
    }

-- get tree part from the tree chunk
getTreeChunk :: BlockTreeChunk -> IO BlockTreePart
getTreeChunk (BlockTreeChunk {
    file_path = path,
    altered = altered,
    cache_part = cache_part
}) = do
    mpart <- getA cache_part

    case mpart of
        Right part -> return part
        Left _ -> do
            part <- BSR.readFile path >>= decodeTreePart
            
            setA cache_part $ Right part
            setA altered $ False
            
            return part

setTreeChunk :: BlockTreeChunk -> BlockTreePart -> IO ()
setTreeChunk (BlockTreeChunk {
    altered = altered,
    cache_part = cache_part
}) part = do
    setA cache_part $ Right part
    setA altered $ True

-- flush the tree part back to file and clear the cache
flushTreeChunk :: BlockTreeChunk -> IO ()
flushTreeChunk (BlockTreeChunk {
    file_path = path,
    altered = altered,
    cache_part = cache_part
}) = do
    mpart <- getA cache_part
    altered <- getA altered

    case mpart of
        Left _ -> return () -- not cached, do nothing
        Right part -> do
            if altered then do -- tree part altered
                BSR.writeFile path (encodeLE part)
            else
                return ()

            setA cache_part (Left $ treePartInfo part)

-- chunks are named as "[btp base name].[number]"
treeCachedFromDirectory :: FilePath -> IO BlockTreeCached
treeCachedFromDirectory dir = do
    files <- listDirectory dir

    let res = (flip map) files $ \file_name ->
            case file_name =~ tucker_btp_name_regex of
                [] -> Nothing
                ([_, nums]:_) ->
                    Just (read nums :: Word, dir </> file_name)
                
        sorted =
            sortBy (\(a, _) (b, _) -> compare a b)
                   [ v | Just v <- res ]

        -- next_num = if null sorted then 0 else fst $ last sorted

    -- read chunks
    chunks <-
        if null sorted then
            emptyTreeChunk 0 (dir </> tucker_btp_name_gen 0) >>= (return . (:[]))
        else
            forM sorted $ uncurry treeChunkFromFile

    return $ BlockTreeCached dir chunks

flushTreeCached :: BlockTreeCached -> IO ()
flushTreeCached (BlockTreeCached {
    chunks = chunks
}) = do
    mapM flushTreeChunk chunks
    return ()

-- take part of the top chunk and seal it
sealTreeCached :: Int -> BlockTreeCached -> IO BlockTreeCached
sealTreeCached seal_count orig@(BlockTreeCached {
    base_dir = dir,
    chunks = chunks
}) = do
    let last_chunk = last chunks
        next_no = treeChunkNumber last_chunk + 1

    last_part <- getTreeChunk last_chunk
    new_chunk <- emptyTreeChunk next_no (dir </> tucker_btp_name_gen next_no)

    let (oldp, newp) = splitTreePart seal_count last_part

    setTreeChunk last_chunk oldp
    setTreeChunk new_chunk newp

    let new_tree = orig {
        chunks = chunks ++ [new_chunk]
    }

    flushTreeCached new_tree

    return new_tree

-- max_depth == 0 means no limit
insertToTreeCached :: Int -> BlockTreeCached -> BlockPayloadHashed -> IO (Either TCKRError ())
insertToTreeCached max_depth (BlockTreeCached {
    chunks = chunks
}) bph = do
    let stop_pred (suc, depth, chunks) =
            (max_depth /= 0 && depth >= max_depth) ||
            null chunks ||
            suc

        iterate_proc (suc, depth, chunks) = do
            let (i, chunk) = head chunks

            part <- getTreeChunk chunk
            let height = treePartHeight part

            res <- case insertToTreePart part bph of
                Left err -> do
                    part <- getTreeChunk chunk
                    return False

                Right new_part -> do
                    setTreeChunk chunk new_part

                    if i == 0 then
                        -- don't flush the top chunk
                        return ()
                    else
                        flushTreeChunk chunk
                        
                    return True

            return (res, height + depth, tail chunks)

    let search = do
        (suc, depth, chunks) <-
            iterateUntilM
                stop_pred iterate_proc
                (False, 0, zip [ 0 .. ] $ reverse chunks)

        return $
            if suc then
                Right ()
            else
                Left $ TCKRError $ "parent not found above depth " ++ show depth

    catch search $ \exc -> do
        return $ Left exc

treeCachedLatestHash :: BlockTreeCached -> IO [Hash256]
treeCachedLatestHash (BlockTreeCached {
    chunks = chunks
}) = do
    let last_chunk = last chunks
    part <- getTreeChunk last_chunk
    return $ treePartLatestHash part

treeCachedTopChunkHeight :: BlockTreeCached -> IO Int
treeCachedTopChunkHeight (BlockTreeCached {
    chunks = chunks
}) = treeChunkHeight $ last chunks
