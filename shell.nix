let
  pkgs = import <nixpkgs> {};
  project = (import ./release.nix).project;

  # Had to be copied over to here since it's no longer added to the local packages. Not sure how
  # to do that with home-manager now. And don't care until this no longer works since it's the only
  # place I use this...
  writeWatchScript = { name, src ? ".", exclude ? "//", command }: 
    pkgs.writeShellScriptBin name "${pkgs.fswatch}/bin/fswatch -0 --event=Updated -r -o -l 0.2 -e ${exclude} ${src} | xargs -0 -I {} -n 1 ${command}";

  localCabalRun = name: executable: pkgs.writeShellScriptBin name "cabal new-run ${executable} -- $@";
in
pkgs.mkShell {
  name="scripts";
  inputsFrom=[
    project.env
  ];
  buildInputs=[
    (writeWatchScript {
      name = "watch-all";
      src = "**/*.hs"; 
      exclude = "dist-newstyle";
      command = "cabal new-build all";
    })
    (pkgs.writeShellScriptBin "update" "hpack && cabal2nix . > project.nix")
    (localCabalRun "projects" "projects")
    (localCabalRun "copy" "copy")
    (localCabalRun "nix-github" "nix-github")
    (localCabalRun "nix-npm" "nix-npm")
    (localCabalRun "ssh-init" "ssh-init")
    (localCabalRun "simple-watch" "simple-watch")
    (localCabalRun "secrets" "secrets")
  ];
}

