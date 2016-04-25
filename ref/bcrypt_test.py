import bcrypt
from bcrypt import _bcrypt

key = b"CHARLIER0"
#key = b'\x6d\x79\x6b\x65\x79'
#salt = b'\xb9\xe4\x03\x30\xd2\xc1\x0b\xbd\x8b\xd3\x0c\xbd\x02\x20\xce\xea'
salt = b'\xe4\xe4\xe4\xe4\xe4\xe4\xe4\xe4\xe4\xe4\xe4\xe4\xe4\xe4\xe4\xe4'

rounds = 4

saltout = _bcrypt.ffi.new("unsigned char[]", 30)
gensalt = _bcrypt.lib.crypt_gensalt_rn(b"$" + b"2b" + b"$", rounds, salt, len(salt), saltout, len(saltout))

hashed = _bcrypt.ffi.new("unsigned char[]", 128)
_bcrypt.lib.crypt_rn(key, gensalt, hashed, len(hashed))

print(_bcrypt.ffi.string(hashed))
