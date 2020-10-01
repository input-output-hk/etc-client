{ sources ? import nix/sources.nix, pkgs ? import ./nix { } }:

if __getEnv "BUILDKITE" == "true" then
  import .buildkite/shell.nix { inherit sources pkgs; }
else
  with pkgs;

  mkShell {
    inputsFrom = [ mantis ];
    buildInputs = [ (callPackage sources.Sbtix { }) ];
    SBTIX_GEN = "true";
  }
