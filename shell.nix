let
  pkgs = import <nixpkgs> {};
  project = (import ./release.nix).project;
in
pkgs.mkShell {
  name="scripts";
  inputsFrom=[
    project.env
  ];
  buildInputs=[
    (pkgs.writeWatchScript {
      name = "watch-all";
      src = "**/*.hs"; 
      exclude = "dist-newstyle";
      command = "cabal new-build all";
    })
    (pkgs.writeShellScriptBin "update" "hpack && cabal2nix . > project.nix")
    (pkgs.localCabalRun "projects")
    (pkgs.localCabalRun "copy")
    (pkgs.localCabalRun "create-interactive-slideshow")
    (pkgs.localCabalRun "nix-github")
    (pkgs.localCabalRun "nix-npm")
    (pkgs.localCabalRun "ssh-init")
  ];
}

