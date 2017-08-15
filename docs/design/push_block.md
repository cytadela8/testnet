# Block Pushing algorithm

## Summery

To minimize hard forks it's important nodes get new mined blocks as fast as possible, so everyone mines on one common longest chain. To achieve that a variant of [Gossip protocol](https://en.wikipedia.org/wiki/Gossip_protocol) is used. Whenever a node mines a block it starts sending to known nodes the block in special message. Those nodes in turn start sending the block to other nodes utilizing the same special message.

## Description

- `push_block` supervisor spawns `push_block_worker`
- `push_block_worker` routing:
  1. get all peers in a list and randomize the order
  2. spawn `push_block_gossip_process_count` processes that send the block to peers in order of apperaring in the randomize list using the `give_new_block` message.
  3. when a process finishes spawn and:
    a. the response is `unknown`: spawn block sending processes till there are `push_block_gossip_process_count` processes.
    b. the response is `known` and last `push_block_gossip_stop_count - 1` processes finished with `known` response: terminate `push_block_worker`
    c. if else: do nothing
- `push_block` supervisor is called to spawn the worker when:
  - we mined a block
  - we got a block with `give_new_block` message and the block is valid
- `give_new_block` gives `known` when the block hash is in our database and `unknown` otherwise

## Maths behind this

Gossip algorithm is very fast. Given one send process is used at a time and every send takes the same amount of time and all nodes are live the number of nodes that know a block increases about x2 every iteration. The simulation given infinite time and episilon fraction of nodes (1 node) spreading the block in the beginning gives following:

| value of `stop_count` | log\_2 (fraction of nodes that don't know the block in the end) | messages send / number of nodes |
| --- | --- | --- |
| 1 | -2.029324 | 1.736401 |
| 2 | -4.584444 | 3.122338 |
| 3 | -7.558602 | 4.193323 |
| 4 | -11.405129 | 5.202064 |
| 5 | -16.582562 | 6.203117 |
| 6 | -23.041973 | 7.203193 |
| 7 | -32.123610 | 8.203196 |
| 8 | -42.978441 | 9.203196 |
| 9 | -56.186940 | 10.203196 |

If `stop_count` >=3 more then 99% of nodes know the transaction after 23 iterations. Given 1 000 000 nodes.

