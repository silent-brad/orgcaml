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
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        ocamlPackages = pkgs.ocaml-ng.ocamlPackages_5_3;

        orgcaml = ocamlPackages.buildDunePackage {
          pname = "orgcaml";
          version = "0.1.0";
          src = ./.;
          duneVersion = "3";
          propagatedBuildInputs = [ ocamlPackages.angstrom ];
          doCheck = true;
          meta = {
            description = "OrgCaml – Org-mode parser and HTML renderer for OCaml";
            license = pkgs.lib.licenses.mit;
            homepage = "https://github.com/silent-brad/orgcaml";
          };
        };

        orgcaml-bin = ocamlPackages.buildDunePackage {
          pname = "orgcaml-bin";
          version = "0.1.0";
          src = ./.;
          duneVersion = "3";
          buildInputs = [ orgcaml ];
        };
      in
      {
        packages = {
          default = orgcaml-bin;
          orgcaml = orgcaml;
          orgcaml-bin = orgcaml-bin;
        };

        devShells.default = pkgs.mkShell {
          inputsFrom = [ orgcaml ];
          packages = with ocamlPackages; [
            ocaml
            dune_3
            ocaml-lsp
            ocamlformat
            utop
            odoc
          ];
        };

        checks.default = orgcaml;
      }
    );
}
