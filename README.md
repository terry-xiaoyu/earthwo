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
