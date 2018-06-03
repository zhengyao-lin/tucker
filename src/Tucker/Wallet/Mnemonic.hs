{-# LANGUAGE CPP #-}

-- implementation for BIP 39

module Tucker.Wallet.Mnemonic where

import Data.Bits
import Data.List
import qualified Data.Text as TXT
import qualified Data.ByteString as BSR
import qualified Data.Text.Normalize as NORM
import qualified Data.ByteString.Char8 as BS

import Tucker.Enc
import Tucker.Util
import Tucker.Error
import Tucker.Crypto

import qualified Tucker.Container.Map as MAP

import qualified Tucker.Wallet.WordList.English as ENG

{-

NOTES for BIP 39 Mnemonic code for generating deterministic keys

basically there are two functions:

entropyToMnemonic
mnemonicToEntropy

parameters:

(predefined)
1. N - number of bits in one word index(in BIP 39 it's 11(0-2047))

(predefined)
2. C - checksum length = len_in_bits(entropy) / C

3. E - length of entropy(in bits)
4. W - number of words

N should divide (E + E / C)

W * N = E + E / C = (CE + E) / C
(C + 1)E = W * N * C
E = W * N * C / (C + 1)

-}

type WordList = (MAP.TMap Int String, MAP.TMap String Int)

alistToWordList list =
    (MAP.fromList ENG.word_list,
     MAP.fromList (map (\(a, b) -> (b, a)) ENG.word_list))

-- normalize utf-8 nfkd
normalize :: String -> String
normalize = TXT.unpack . NORM.normalize NORM.NFKD . TXT.pack

data MnemonicConf =
    MnemonicConf {
        mne_list     :: WordList, -- the word list should be sorted
        mne_kdf_iter :: Int,
        mne_n        :: Int,
        mne_c        :: Int
    }

instance Default MnemonicConf where
    def =
        MnemonicConf {
            mne_list = alistToWordList ENG.word_list,
            mne_kdf_iter = 2048,
            mne_n = 11,
            mne_c = 32
        }

-- the serialization of data(to bits) is all in big endianness
entropyToMnemonic :: MnemonicConf -> ByteString -> Either TCKRError [String]
entropyToMnemonic conf ent = do
    let n = mne_n conf
        c = mne_c conf
        e = BSR.length ent * 8
        check_bits = e `div` c
        (w, rem) = (e + check_bits) `quotRem` n
        checksum = decodeVWord BigEndian (sha256 ent) :: Integer
        entint = decodeVWord BigEndian ent :: Integer
        dat = shift entint check_bits .|. shiftR checksum (256 - check_bits)
        (wmap, _) = mne_list conf

    assertMT "the entropy has an invalid length" (rem == 0)
    -- assertMT "total length not aligned to bytes" ((w * n) `mod` 8 == 0)

    return $ reverse $
        for [ 0 .. w - 1 ] $ \i ->
            let idx = shiftR dat (i * n) .&. ones n in
            case MAP.lookup (fi idx) wmap of
                Just w -> w
                Nothing -> error "invalid word list"

mnemonicToEntropy :: MnemonicConf -> [String] -> Either TCKRError ByteString
mnemonicToEntropy conf words = do
    let (_, wmap) = mne_list conf
        n = mne_n conf
        c = mne_c conf

        (e, rem) = (length words * n * c) `quotRem` (c + 1)
        check_bits = e `div` c

        find word =
            maybe (Left word) (Right . fi) $
            MAP.lookup word wmap

        idxs = map find words
        not_found = [ i | Left i <- idxs ]
        found = [ i | Right i <- idxs ]

        fold_proc dat idx = shift dat n .|. (idx .&. ones n)
        dat = foldl fold_proc 0 found

        entint = shiftR dat check_bits
        ent = encodeInt (e `div` 8) BigEndian entint

        obs_checksum = dat .&. ones check_bits
        exp_checksum =
            shiftR (decodeVWord BigEndian (sha256 ent) :: Integer)
                   (256 - check_bits)

    assertMT "entropy length not aligned to 8" (e `mod` 8 == 0)
    assertMT ("words not found: " ++ show not_found) (null not_found)
    assertMT "unable to solve for the entropy length(illegal sentence length)" (rem == 0)
    assertMT "checksum not met" (obs_checksum == exp_checksum)

    return ent

mnemonicToSeed :: MnemonicConf -> [String] -> Maybe String -> Either TCKRError ByteString
mnemonicToSeed conf words mpass = do
    mnemonicToEntropy conf words
    return (pbkdf2 mne pass (mne_kdf_iter conf) 64)
    where
        mne = BS.pack (normalize (unwords words))
        pass = BS.pack (normalize ("mnemonic" ++ maybe "" id mpass))
