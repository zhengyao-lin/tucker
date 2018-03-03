module Tucker.ASN1 where

import Data.Word
import Data.Bits
import qualified Data.ByteString as BSR

import Control.Applicative

import Tucker.Enc
import Tucker.Util

data ASN1Length = ASN1Length Integer deriving (Eq, Show)

data ASN1Object
    = ASN1Integer Integer
    | ASN1Sequence [ASN1Object] deriving (Eq, Show)

instance Encodable ASN1Length where
    encode _ (ASN1Length len)
        | len < 0x7f = bchar (fi len)
        | otherwise = -- long form, X.690-0207, section 8.1.3.5
            first <> i -- TODO: if len of i >= 127
            where i = vword2bsBE len
                  first = bchar (BSR.length i .|. 0x80)

instance Decodable ASN1Length where
    decoder = forceEndian BigEndian $ do
        first <- byteD

        if first < 0x80 then -- short form
            return (ASN1Length (fi first))
        else do -- long form
            len <- bs2vwordBE <$> bsD (fi (first .&. 0x7f))
            return (ASN1Length len)

-- (tag, size, content) triple
encodeTriple :: Encodable t => Word8 -> t -> ByteString
encodeTriple tag cont = encodeBE (tag, ASN1Length len, enc)
    where enc = encodeBE cont
          len = fi (BSR.length enc)

instance Encodable ASN1Object where
    encode _ (ASN1Integer num) = encodeTriple 0x02 num
    encode _ (ASN1Sequence lst) = encodeTriple 0x30 lst

tripleD :: (t -> ASN1Object) -> Word8 -> Decoder t -> Decoder ASN1Object
tripleD cons tag d = forceEndian BigEndian $ do
    beginWithByteD tag

    -- read the length
    ASN1Length len <- decoder

    cons <$> quota (fi len) d

asn1IntegerD :: Decoder ASN1Object
asn1IntegerD = tripleD ASN1Integer 0x02 decoder

asn1SequenceD :: Decoder ASN1Object
asn1SequenceD = tripleD ASN1Sequence 0x30 (many decoder)

instance Decodable ASN1Object where
    decoder = asn1IntegerD <|> asn1SequenceD
