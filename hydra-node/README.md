## Hydra Node Guide

Hydra node is a piece of software that contains the implementation of the Head
protocol. This guide will try to go through each component and describe what it
does as well as put together a complete mental image of connected parts.

#### Starting the Hydra Node

When starting the Hydra Node we first need to provide some parameters as
command line arguments. The list of all possible flags that Hydra Node accepts
can be seen easily using the `--help` command.

Two main commands you can use are `run` and `publish`. `run` is reserved for
actually running your hydra-node and `publish` serves the purpose of publishing
plutus scrips needed to drive the Head protocol before actually running the
software (see "Publishing scripts").

Let's take a look at what happens when you run the hydra-node. After providing
needed arguments to your node (see "Hydra Node arguments") we first set up
logging which enables us to get insights about what is happening inside of a
running node. Then we proceede with creating the event queue. This queue is
used to capture different incoming _events_ which will then be handled by the
hydra-node sequentially (see "Event Queue").

#### Event Queue

Event Queue is fundamental in the hydra-node design. It is implemented using a handle pattern
like a lot of things in the Hydra codebase. For this event we are able to:

- Put events into it (`putEvent`)
- Put some event after some other (`putEventAfter`)
- Get the next event (`nextEvent`)
- Check if queue is empty (`isEmpty`)

This queue is designed to work in the multithreaded environment.

TODO: write about event queue

#### Hydra Node arguments

TODO: Write about the hydra-node arguments here

#### Publishing scripts

Hydra scripts are part of the hydra-plutus package. They need to be
pre-published because they are used as the reference inputs in the Hydra
transactions. All three play crucial part of Head protocol and play important
role since without them modelling of the protocol would be impossible.

There are three scripts that we want to pre-publish:

- Initial
- Commit
- Head

TODO: continue describing here

