# nix develop .#wasm --command bash -c "make"
# nix-build docker.nix
# docker load < result
# docker run --rm -it -p 3000:3000 miso-games:latest

{ pkgs ? import ./nixpkgs.nix }:

let

  app-client = pkgs.callPackage ./app-client.nix {};

  server = pkgs.static-web-server;

  entrypoint = pkgs.writeScript "entrypoint.sh" ''
    #!${pkgs.stdenv.shell}
    $@
  '';


in pkgs.dockerTools.buildLayeredImage {
  name = "misokoban";
  tag = "latest";
  created = "now";
  contents = [ "${app-client}" ];
  config = {
    Entrypoint = [ entrypoint ];
    Cmd = [ "${server}/bin/static-web-server -p 3000 -d ${app-client}/public" ];
  };

}

