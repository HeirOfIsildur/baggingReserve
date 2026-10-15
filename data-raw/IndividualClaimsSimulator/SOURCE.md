# Vendored: Individual Claims Simulator (Wang & Wüthrich, 2022)

`functions simulation.r` (upstream path `Tools/functions simulation.r`) and the helpers it sources,
`Tools/reopening.r`, `Tools/functions plotting.r` and `Tools/README.md`, are copied verbatim from

- Repository: https://github.com/JSchelldorfer/IndividualClaimsSimulator
- Commit: `4991e882929a0e3f0bc10cc9dc00d7cedbf2a5cf` (main, 2022-08-19)
- Authors: Melantha Wang and Mario V. Wüthrich
- Paper: *Individual Claims Generator for Claims Reserving Studies: data simulation.R*,
  SSRN 4127073, https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4127073
- Licence: GPL-3.0 (see `LICENSE` in this directory)

The generator sources `./Tools/reopening.r` relative to the working directory, so the
scripts call `data.generation()` from inside this folder. It is used only by
`data-raw/make_synthetic_claims.R`, which regenerates the package datasets (and, in the
manuscript's replication material, by the robustness study); it is not part of the installed package (`data-raw/` is in `.Rbuildignore`). The generator
builds on the CRAN package SynthETIC (Avanzi, Taylor, Wang & Wong).
