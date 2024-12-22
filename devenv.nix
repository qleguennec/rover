{
  pkgs,
  lib,
  config,
  inputs,
  ...
}:

{
  env.GREET = "devenv";

  packages = with pkgs; [
    nixfmt-rfc-style
    ghcid
    ormolu
  ];

  # https://devenv.sh/languages/
  languages.haskell.enable = true;
  languages.haskell.package = (
    pkgs.ghc.withPackages (
      p: with p; [
        parsec
        text
        linear
        lens
        hspec
      ]
    )
  );

  processes = {
    "rover:dev".exec = "ghcid -c 'ghci -Wall' -T mainWithArgs src/Main.hs";
    "rover:test".exec = "ghcid -c 'ghci -Wall' -T main test/Spec.hs";
  };

  enterShell = ''
    git --version
    ghc --version
  '';

  tasks = {
    "rover:compile".exec = "ghc -isrc/ src/Main.hs -o ./rover";
  };
}
