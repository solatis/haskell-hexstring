{ mkDerivation, aeson, base, base16-bytestring, binary, bytestring
, hspec, stdenv, text
}:
mkDerivation {
  pname = "hexstring";
  version = "0.11.1";
  src = ./.;
  buildDepends = [
    aeson base base16-bytestring binary bytestring text
  ];
  testDepends = [ base binary bytestring hspec text ];
  homepage = "http://www.leonmergen.com/opensource.html";
  description = "Fast and safe representation of a hex string";
  license = stdenv.lib.licenses.mit;
}
