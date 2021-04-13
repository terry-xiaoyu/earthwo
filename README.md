# Earthwo

Earthwo is a simple and high available KV database.

## Why "Earthwo"

"Earthwo" is actually ["Earthworm"](https://en.wikipedia.org/wiki/Earthworm) but only part of it. Earthworms have the ability to regenerate lost segments. So an "earthwo" would still be alive even if it lost the "rm" part!

## Basic Design Requirements

- Use mnesia as the underlying database, keep the implementation as simple as possible.

- Simple and clean architecture, easy of understand and management.

- High availablilty over consistency. Some data inconsistency should be solved by the application level (e.g. update the key again)

- Be able to work even if some of the nodes lost. (there should be some replicas of a key, and the replicas can be added/removed at runtime)

- The replicas work in the master-master mode, that is, writes can go to any of the relica node.

- It solves the inconsistency by itslef without the interference from the user.

- Persistent data to disk, support rolling restart/upgrade without losing data

- Support key expiry after some period of time (like the TTL of a key in Redis)

- Support hooks upon key creation or deletion (e.g. notify someone that the data has changed)

- Support multiple secondary indexes.

- Support a channel for clients to send messages to other clients in the same cluster (useful when send messages across data center)

## Recover from inconsistency

- In case of a network partition, the clients should help earthwo to determine a master node to use as the ultimate segment. Some of the data could be loss after one replica copies the whole table from the master node.

- Writting to the same key on different replica nodes at the same time may lead to an inconsistent data, but that should be rare. And we can force the operations on the same key go into the same replica.


## Segments

A segment is a logical data container for a shard of data. A segments stores the keys fall into a range of hash value:

```
             segment_1   [0..9999]
          /
hash(Key)  - segment_2   [10000..19999]
          \
             segment_3   [20000..29999]
             ...
```

### Distribution of Segments and Replicas


### Segments shuffling

Given following deployment:

```
```

Let's discuss the scenarios when node lost.

#### Shuffling when node lost

#### Shuffling when new node joins the cluster
