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
    (pkgs.localCabalRun "projects" "projects")
    (pkgs.localCabalRun "copy" "copy")
    (pkgs.localCabalRun "create-interactive-slideshow" "create-interactive-slideshow")
    (pkgs.localCabalRun "nix-github" "nix-github")
    (pkgs.localCabalRun "nix-npm" "nix-npm")
    (pkgs.localCabalRun "ssh-init" "ssh-init")
    (pkgs.localCabalRun "simple-watch" "simple-watch")
    (pkgs.localCabalRun "secrets" "secrets")
  ];
}

