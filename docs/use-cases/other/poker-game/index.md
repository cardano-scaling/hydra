# Poker game

The poker game is a prototypical example of a multi-party state channel.

We often use the analogy of a _poker game_ to explain how Hydra heads function, as it closely aligns with the fundamentals of the Hydra Head protocol. A poker game, like any game, has a clear start and end, and progresses according to a set of agreed-upon rules. In poker, the monetary component is central; players place bids and exchange money throughout the game. Moreover, the game involves a fixed number of players who have conflicting goals (ie, to win the game), may not fully trust each other, but are willing to cooperate within the established rules.

:::tip Decentralized randomness and multi-party computation
For this use case, we consider the possibility of implementing a decentralized poker game with pseudo-randomness or multi-party computation (refer to [ROYALE by David & al](https://eprint.iacr.org/2018/157)). We focus here on the state channel aspect, for which Hydra heads provide a robust solution.
:::

In this scenario, each player represents a member of a Hydra head, operating their own Hydra node. The game begins with each participant committing funds to the head, which serve as their chips. Once the head is active, participants can start the game, utilizing on-head Plutus contracts to facilitate gameplay. Players can instantly process fund transfers within the head, with the script acting as the game dealer — ensuring adherence to rules and smooth game progression.

![](./poker.webp)

Ultimately, the game concludes with a well-defined distribution of funds. Participants can choose to play another game or close the head and record the final outcomes on layer 1. The entire gameplay remains opaque to layer 1; only the ultimate UTXO distribution is known.

While it is technically feasible to conduct this game entirely on layer 1, using a Hydra head offers significant advantages. It enables fast-paced transactions throughout the game and minimizes transaction fees beyond the initial costs needed to establish the Hydra head.
