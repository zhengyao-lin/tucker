#! /bin/bash

function hex2bin() {
	xxd -r -p
}

function bin2hex() {
	hexdump -e '/1 "%02X"'
}

# echo "-----BEGIN PUBLIC KEY-----" && echo "04085C6600657566ACC2D6382A47BC3F324008D2AA10940DD7705A48AA2A5A5E33A726DD7E88D4C03086E19122A8350F82CF7FFF16D8111B48FD82D9DBD07B374C" | python3 ../tucker/tools/pub2pem.py | hex2bin | base64 && echo "-----END PUBLIC KEY-----"

# base64 --decode

# $1 public key in hex
# $2 signature in hex
# $3 message in hex

base=$(dirname $0)

# echo $1 | python3 $base/pub2pem.py
# echo $1 | python3 $base/pub2pem.py | hex2bin | base64

echo $3 | hex2bin | \
openssl pkeyutl -verify -pubin \
-inkey <( \
	echo "-----BEGIN PUBLIC KEY-----" && \
	echo $1 | python3 $base/pub2pem.py | hex2bin | base64 && \
	echo "-----END PUBLIC KEY-----" \
) \
-sigfile <(echo $2 | hex2bin)

