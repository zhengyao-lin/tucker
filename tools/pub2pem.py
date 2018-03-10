#! /bin/usr/python3

# convert ECDSA public key(secp256k1, in hex) to pem file

from pyasn1.type.namedtype import *
from pyasn1.type.char import *
from pyasn1.type.univ import *

from pyasn1.codec.der.encoder import encode

import binascii

class Param(Sequence):
    componentType = NamedTypes(
        NamedType("p1", ObjectIdentifier()),
        NamedType("p2", ObjectIdentifier())
    )

class PublicKey(Sequence):
    componentType = NamedTypes(
        NamedType("param", Param()),
        NamedType("raw", BitString())
    )

pk = PublicKey()
param = Param()

hex = input()

param["p1"] = ObjectIdentifier("1.2.840.10045.2.1")
param["p2"] = ObjectIdentifier("1.3.132.0.10")

pk["param"] = param
pk["raw"] = BitString.fromHexString(hex)

print(binascii.hexlify(encode(pk)).decode())
