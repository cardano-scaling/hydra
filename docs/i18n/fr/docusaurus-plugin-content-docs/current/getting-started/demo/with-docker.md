---
sidebar_position: 2
---

# Via Docker

```mdx-code-block
import TerminalWindow from '@site/src/components/TerminalWindow';
```

> Ce document détaille les étapes nécessaires pour faire tourner une Hydra Head dans un environment de démo, avec Docker.

La démo comprend:

- un cluster de trois noeuds Hydra, connectés point-à-point, et ayant chacun une identité (`alice`, `bob`, `carol`) associée à une paire de clés publique/privée;
- Un noeud Cardano, en mode BFT, produisant des blocs et formant a lui seul un réseau Cardano pour le développement local&nbsp;;
- un serveur Prometheus pour des métriques&nbsp;;
- une interface utilisateur dans le terminal pour se connecter et opérer chaque noeud Hydra. 

:::caution Avertissement!
Le noeud Cardano créé un "réseau" privé et local qui démarre depuis un bloc _genesis_. Pour démarrer, la configuration du noeud doit être à jour (comprendre, récente). Si au démarrage, le noeud Cardano retourne `LedgerNoView`, alors c'est que l'heure de démarrage indiquée dans la configuration génésis est trop loin dans le passé: il faut en ce cas générer la configuration à nouveau via `prepare-devnet.sh`.
:::

## Mise en place du réseau

Nous utiliserons [Docker](https://www.docker.com/get-started) et [compose](https://www.docker.com/get-started) pour cette démo, assurez-vous de les avoir disponibles dans votre terminal de commandes. Si vous ne souhaitez pas utiliser ou installer Docker, vous pouvez directement vous rendre sur [Démo: Sans Docker](/docs/getting-started/demo/without-docker) et mettre les mains dans le cambouis.

:::info Contexte
Toutes les commandes ci-dessous supposent qu'elles sont éxecutées depuis le dossier `demo/`, à la racine du dépôt. Vous aurez donc besoin de cloner le dépôt et de `cd demo` avant d'aller plus loin.
:::

:::warning (in)compatibilité du système d'exploitation
Les instructions fournies ci-après ont été testées pour des environnements Linux récents (Ubuntu, ArchLinux, NixOS...). Si vous êtes sur Windows ou MacOS vous aurez sans doute besoin d'ajuster certaines commandes liées aux [volumes](https://docs.docker.com/storage/volumes/).
:::

Pour commencer, récupérez les images des différents services définis dans le fichier compose:

```mdx-code-block
<TerminalWindow>
docker-compose --profile tui pull
</TerminalWindow>
```

Ensuite, vous pouvez exécuter le script `./prepare-devnet.sh` pour créer les fichiers nécessaires à la configuration du réseau local (a.k.a devnet). Le script génère une configuration _genesis_ Cardano (qui fixe les paramètres du protocole). Notez que dans le cadre de cette démo, la configuration du réseau n'utilise pas de _stake pool_ et ne requiert qu'un unique noeud pour fonctionner.

```mdx-code-block
<TerminalWindow>
./prepare-devnet.sh
</TerminalWindow>
```

C'est tout. Il est maintenant possible de démarrer le _cluster_ via:

```mdx-code-block
<TerminalWindow>
docker-compose up -d
</TerminalWindow>
```

Toutes ces étapes sont combinées dans un unique script `./run-docker.sh`. Ce script contient aussi quelques petites vérifications pour éviter de vous tirer une balle dans le pied avec certaines commandes. 

## Générer des fonds

En l'état actuel, les noeuds Hydra requièrent des UTxO dans une forme assez spécifique afin de pouvoir payer les transactions nécessaires au protocole (on appelle ces UTxO: "carburant"), ainsi que des fonds à consigner dans le contrat pour chaque participant. Parmi les fichiers, vous trouverez un script `./seed-devnet.sh` qui s'occupe de générer ces UTxOs pour chaque participant en utilisant la `cardano-cli`. 

:::info
Les transactions générées par seed-devnet.sh n'ont rien de particulier, ce sont des transactions Cardano. Toutefois si vous deviez les générer vous-même, assurez vous qu'à chaque addresse générée à partir de la paire publique/privée associée à l'option `--cardano-signing-key` de chaque participant soient envoyés deux UTxOs :

- Un UTxO simple, contenant des Ada ou actifs natifs;
- Un UTxO ne contenant que des Ada et avec le datum hash suivant: `a654fb60d21c1fed48db2c320aa6df9737ec0204c0ba53b9b94a09fb40e757f3` (ce datum identifie les UTxOs réservés au "carburant").
:::

## Utiliser l'interface client

À l'aide de compose, vous pouvez démarrer les interfaces clientes (a.k.a `hydra-tui`) pour interagir avec chaque noeud Hydra. Le fichier compose contient déjà trois services pré-configurés: `hydra-tui-1`, `hydra-tui-2`, and `hydra-tui-3`. Pour démarrer le premier client, utilisez simplement:

```mdx-code-block
<TerminalWindow>
docker-compose --profile tui run hydra-tui-1
</TerminalWindow>
```

Cette commande lance une interface utilisateur dans le terminal, configurée avec les clés du premier noeud (i.e. Alice). Dans d'autres terminaux, vous pouvez lancer de la même façon `hydra-tui-2` et `hydra-tui-3`.
