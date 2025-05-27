{
  inputs = rec {
    common.url = "github:YuMingLiao/common";
    nixpkgs.follows = "common/nixpkgs";
 
   replica = {
     type = "github";
     owner = "pkamenarsky";
     repo = "replica";
     rev = "46d2fd3b2c236d573f1d6002eb58cc95b2ba03b9";
     flake = false;
   };
    concur = {
      type = "github";
      owner = "pkamenarsky";
      repo = "concur";
      rev = "dc5347b35c79654d58fa95716f425ee7248504fb";
      flake = false;
    };
  };
  outputs =
    inputs@{ self, common, ... }:
    common.lib.mkFlake { inherit inputs; } {

      perSystem =
        {
          self',
          pkgs,
          config,
          ...
        }:
        {
          haskellProjects.default = {
            basePackages = config.haskellProjects.ghc9101.outputs.finalPackages;
            projectRoot = builtins.toString (
              pkgs.lib.fileset.toSource {
                root = ./.;
                fileset = pkgs.lib.fileset.difference ./. ./flake.nix;
              }
            );
            settings = {
              websockets.jailbreak = true;
            };
            packages = {
              replica.source = inputs.replica;
              concur-core.source = inputs.concur + "/concur-core";
            };
            devShell = {
              mkShellArgs = {
                packages = hp: with hp; [ ];
              };
            };
          };

          packages.default = self'.packages.concur-replica;
        };
    };
}
