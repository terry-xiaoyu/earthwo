# Earthwo

Earthwo is a simple and high available KV database.

## Why "Earthwo"

"Earthwo" is actually ["Earthworm"](https://en.wikipedia.org/wiki/Earthworm) but only part of it. Earthworms have the ability to regenerate lost segments. So an "earthwo" would still be alive even if it lost the "rm" part!

## Build

    $ make

## Basic Design Requirements

- Cluster across data centers (public network)

- Be able to work even if some of the nodes lost, or even one of the data centers lost

- Solves the conflicting itself without the interference from the user

- Simple and easy to deploy using k8s

- Persistent data to disk, support rolling restart/upgrade without losing data

- Support key expiry after some period of time

- Support hooks upon key creation or deletion (e.g. notify someone that the data has changed)

- Quick and simple API using text based protocol

- Support multiple secondary indexes

- Support a channel for clients to send messages to other clients in the same cluster (useful when send messages across data center)

## Terminologies

- DC: data center

- Segments: data partitions in which data can be stored

- N: how many replicas a data should have, equal to the number of data centers

- Shard: the shard number of the whole data set

## Segments

A segment is a logical data container for a piece of data. Data is stored to a specific segment by calculating the hash of the key.

```
             s1
          /
hash(Key)  - s2
          \
             s3
```

### Segments distribution

Let's say there's 3 data centers, and the shard number is set to 2, and replica count (N) is 3 (  always equals to the data centers number). Then we could know that there should be 2 segments on each data center:

```
    DC1      DC2       DC3
         |         |
    s1,  |   s2,   |   s3,
    s4   |   s5    |   s6
         |         |
```

The data written on a specific segment will then be replicated to other DCs:

When writing data to s1 on DC1, it would be replicated to s2 on DC2 and s3 DC3.

When writing data to s4 on DC1, it would be replicated to s5 on DC2 and s6 DC3.

Then nodes in each DC will be assigned some segments, say we have 4 nodes for above example:

```
    DC1        DC2          DC3
          |            |
node1(s1) | node3(s2,  |  node4(s3,
node2(s4) |       s5)  |        s6)
          |            |
```

The total node number must be greater than or equal to N, and cannot be greater than segment number.

`N =< NodeNum =< SegmentNum = (N * ShardNum)`

### Segments shuffling

Given following deployment:

```
    DC1        DC2          DC3
          |            |
node1(s1) | node3(s2,  |  node4(s3,
node2(s4) |       s5)  |        s6)
          |            |
```

Let's discuss the scenarios when node lost or DC lost.

#### Shuffling when node lost

- If some of the nodes on a DC lost

  Then re-distribute the segments on those nodes to the remaining nodes on the same DC. Let's say the node2 failed, then s2 will be moved to node1:

  ```
        DC1        DC2          DC3
              |            |
    node1(s1, | node3(s2,  |  node4(s3,
          s4) |       s5)  |        s6)
              |            |
  ```

- If all of the nodes on a DC lost

  Then remove the DC entirely:

  ```
       DC2          DC3
               |
    node3(s2,  |  node4(s3,
          s5)  |        s6)
               |
  ```

  Then all the data will be handled by only 2 DCs.

#### Shuffling when new node joins the cluster

- When node5 and node6 join the cluster on DC2 and DC3

  ```
        DC1        DC2          DC3
            |            |
  node1(s1) | node3(s2)  |  node4(s3)
  node2(s4) | node5(s5)  |  node6(s6)
            |            |
  ```

  But node number cannot be greater than segment number.

  A default initial deployment have `64 * N` segments, that means it can have maximum to `64` nodes in each DC:

  ```
        DC1         DC2          DC3
               |            |
    node1(s1,  | node2(s1,  |  node3(s1,
          s2,  |       s2,  |        s2,
          ..,  |       ..,  |        ..,
          s64) |       s64) |        s64)
  ```

### Conflicts Resolution

- Data Types: Convergent the siblings

  - map

  - set

- DVVs: Track changes

## How does node join work

- New node will register itself node to the leader nodes after connected. The leader node may reject the registration (due to a running segment change).

- If the leader detects the node-list change, and if segment change is necessary, it will demotes itself to follower and restart the leader election.

- The leader sends (`TOPOLOGY_SYNC`) to all of the followers, and the follower replies back with their local topology. If the leader got a topology from any follower that differs, then it would start a segment change procedure.

- The leader calculates the new topology by evaluating all the topologies got from followers, and then locks the node-list registry and send the new topology to followers (`SEG_CHANGE_START`)

- The followers compare the new topology against the local topology. If they are the same, the follower replies `SEG_CHANGE_COMPLETE` directly. Otherwise the follower logs the topology received and starts to apply the segment change, this procedure includes pulling segments from remote nodes and merging the data, and then replies the leader with `SEG_CHANGE_COMPLETE`.

- After all of the followers replies `SEG_CHANGE_COMPLETE`, the leader notifies the followers to apply the new topology logged locally (by sending `SEG_CHANGE_RELEASE`), and then unlocks the node-list registry.

During the segment change:

- Writes to the segments are distributed to nodes according to both the new and old topology.

- Reads to the segments are distributed to nodes according to both the new and old topology, that is, if the key cannot found from the node defined by the old topology, then try to read the key again using the new topology.

- If any error occurs, the leader stops the procedure and send `SEG_CHANGE_ABORT` to all the followers. The followers will delete the logged topology and the tmp segment copies pulled from remote nodes.

- If the leader is re-elected before `SEG_CHANGE_RELEASE` is received, then the follower will drop the topology change.

- Any node change is ignored by the current leader. After the segment change finished, the leader detects the node change and demotes itself to follower to start a new

