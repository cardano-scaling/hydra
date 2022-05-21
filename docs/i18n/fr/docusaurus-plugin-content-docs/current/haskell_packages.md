---
sidebar_position: 99
---

# Bibliothèques Haskell

Le projet est organisé en différente bibliothèques de code. Parmi celles-ci, certaines remplissent des fonctions spécifiques au projet et sont donc difficilement réutilisables dans un autre contexte; mais d'autres sont suffisamment génériques ou détachées du projet pour être utilisées telles quelles au sein d'autres projets Haskell. Dans un cas comme dans l'autre toutefois, nous mettons à disposition une [documentation Haddock](https://www.haskell.org/haddock/). Les listes ci-après fournissent un résumé sommaire des bibliothèques disponibles et de leur contenu.

## Bibliothèques publiques / réutilisables

| Nom                                                                                            | Description                                                                         |
| ---                                                                                            | ---                                                                                 |
| [plutus-merkle-tree](https://hydra.family/head-protocol/haddock/plutus-merkle-tree/index.html) | Implémentation des arbres de Merkle, compatible avec les scripts Plutus             |
| [plutus-cbor](https://hydra.family/head-protocol/haddock/plutus-cbor/index.html)               | Implementation du format d'encodage CBOR, compatible avec les scripts Plutus        |
| [hydra-prelude](https://hydra.family/head-protocol/haddock/hydra-prelude/index.html)           | Un remplacement de la Prelude de base Haskell, basée sur relude                     |
| [hydra-cardano-api](https://hydra.family/head-protocol/haddock/hydra-cardano-api/index.html)   | Une sur-couche de la `cardano-api`, avec de nouveaux utilitaires et specialisée à une seule ère (Alonzo) | 

## Bibliothèques internes

| Nom                                                                                        | Description                                                             |
| ---                                                                                        | ---                                                                     |
| [hydra-node](https://hydra.family/head-protocol/haddock/hydra-node/index.html)             | Le coeur du noeud Hydra                                                 |
| [hydra-node tests](https://hydra.family/head-protocol/haddock/hydra-node/tests/index.html) | Des utilitaires pour tester le noeud Hydra                              |
| [hydra-tui](https://hydra.family/head-protocol/haddock/hydra-tui/index.html)               | L'interface utilisateur dans le terminal pour la démo des noeuds Hydra  |
| [hydra-plutus](https://hydra.family/head-protocol/haddock/hydra-plutus/index.html)         | Le code Plutus des contrats / scripts Hydra                             |
| [hydra-cluster](https://hydra.family/head-protocol/haddock/hydra-cluster/index.html)       | Tests d'intégration et utilitaires pour démarrer un noeud Cardano local |
