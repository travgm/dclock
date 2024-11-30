let
  pkgs = import <nixpkgs> { };
  
  customHaskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: {
      mkDerivation = args: super.mkDerivation (args // {
        jailbreak = true;  # Ignore version bounds
        doCheck = false;   # Skip tests to avoid dependency issues
      });
    };
  };
in
customHaskellPackages.mkDerivation {
  pname = "dclock";
  version = "0.1.0";

  src = ./.;

  isLibrary = false;
  isExecutable = true;

  executableHaskellDepends = with customHaskellPackages; [
    base
    QuickCheck
    ansi-terminal
    hspec
    hspec-discover
    lens
    machines
    optparse-applicative
    text
    time
    process
  ];

  description = "A decimal clock that breaks your day into a 1000 decimal minutes";
  license = pkgs.lib.licenses.mit;
  maintainers = with pkgs.lib.maintainers; [ travgm ];
  mainProgram = "dclock";
}
