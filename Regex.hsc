{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Regex where

import Foreign
import Foreign.C.Types
import Foreign.C.String

import Data.ByteString.Char8
import System.IO.Unsafe

#include <pcre.h>

newtype PCREOption = PCREOption { unPCREOption :: CInt }
    deriving (Eq, Show)

#{enum PCREOption, PCREOption,
    caseless = PCRE_CASELESS,
    dollar_endonly = PCRE_DOLLAR_ENDONLY,
    dotall = PCRE_DOTALL
}

combineOptions :: [PCREOption] -> PCREOption
combineOptions = PCREOption . foldr ((.|.) . unPCREOption) 0

type PCRE = Ptr ()

foreign import ccall unsafe "pcre.h pcre_compile"
    c_pcre_compile :: CString
                   -> PCREOption
                   -> Ptr CString
                   -> Ptr CInt
                   -> Ptr Word8
                   -> IO (Ptr PCRE)

data Regex = Regex !(ForeignPtr PCRE) !(ByteString)
    deriving (Eq, Ord, Show)

compile :: ByteString -> [PCREOption] -> Either String Regex
compile str flags = unsafePerformIO $
    useAsCString str $ \pattern -> do
        alloca $ \errptr -> do
        alloca $ \erroroffset -> do
            pcre_ptr <- c_pcre_compile pattern (combineOptions flags) errptr erroroffset nullPtr
            if pcre_ptr == nullPtr
                then do
                    err <-peekCString =<< peek errptr
                    return (Left err)
                else do
                    reg <- newForeignPtr finalizerFree pcre_ptr
                    return (Right (Regex reg str))
