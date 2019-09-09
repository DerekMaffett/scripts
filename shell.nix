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
      src = "."; 
      exclude = "dist-newstyle";
      command = "cabal new-build all";
    })
    (pkgs.localCabalRun "projects")
    (pkgs.localCabalRun "copy")
    (pkgs.localCabalRun "create-interactive-slideshow")
    (pkgs.localCabalRun "nix-github")
    (pkgs.localCabalRun "nix-npm")
    (pkgs.localCabalRun "ssh-init")
  ];
}

