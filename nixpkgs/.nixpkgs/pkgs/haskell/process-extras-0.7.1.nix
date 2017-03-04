{ mkDerivation, base, bytestring, data-default, deepseq
, generic-deriving, HUnit, ListLike, mtl, process, stdenv, text
}:
mkDerivation {
  pname = "process-extras";
  version = "0.7.1";
  sha256 = "03ykpjk2li6392vwqi1vi4pwvqbzyady98xbhq7vjq2rh8l64pyj";
  libraryHaskellDepends = [
    base bytestring data-default deepseq generic-deriving ListLike mtl
    process text
  ];
  testHaskellDepends = [ base HUnit ];
  doCheck = false;
  homepage = "https://github.com/seereason/process-extras";
  description = "Process extras";
  license = stdenv.lib.licenses.mit;
}
