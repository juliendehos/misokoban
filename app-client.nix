
{ pkgs ? import ./nixpkgs.nix }:

pkgs.stdenv.mkDerivation {
  name = "app";
  src = ./.;
  dontBuild = true;
  installPhase = ''
    mkdir $out
    cp -r $src/public $out/
  '';
}

