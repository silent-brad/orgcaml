{
  description = "OrgCaml – Org-mode parser and HTML renderer for OCaml";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    {
      overlays.default = final: prev: {
        ocamlPackages = prev.ocamlPackages.overrideScope (
          ofinal: oprev: {
            orgcaml = ofinal.buildDunePackage {
              pname = "orgcaml";
              version = "0.1.0";
              src = self;
              duneVersion = "3";
              nativeBuildInputs = [ prev.git ];
              propagatedBuildInputs = [ ofinal.angstrom ];
              meta = {
                description = "OrgCaml – Org-mode parser and HTML renderer for OCaml";
                license = prev.lib.licenses.mit;
                homepage = "https://github.com/silent-brad/orgcaml";
              };
            };
          }
        );
      };
    }
    // flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ self.overlays.default ];
        };
        ocamlPackages = pkgs.ocamlPackages;

        orgcaml-bin = ocamlPackages.buildDunePackage {
          pname = "orgcaml-bin";
          version = "0.1.0";
          src = ./.;
          duneVersion = "3";
          buildInputs = [ ocamlPackages.orgcaml ];
        };
      in
      {
        packages = {
          default = orgcaml-bin;
          orgcaml = ocamlPackages.orgcaml;
          orgcaml-bin = orgcaml-bin;
        };

        devShells.default = pkgs.mkShell {
          inputsFrom = [ ocamlPackages.orgcaml ];
          packages = with ocamlPackages; [
            ocaml
            dune_3
            ocaml-lsp
            ocamlformat
            utop
            odoc
          ];
        };

        checks.default = ocamlPackages.orgcaml;
      }
    );
}
