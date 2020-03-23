-module(earthwo_cluster).

-behaviour(gen_server).

-export([ pick_seed/1
        , seeds/0
        , init/0
        , local_topology/0
        , init_topology/0
        , join/1
        ]).

-type(seg_id() :: binary()).
-type(dc_name() :: binary()).
-type(node_id() :: binary()).

-record(cluster_info, {
    node_id :: node_id()
}).

%% topology: #{seg_id() => node()}
-record(topology, {
    segment :: seg_id(),
    dc_name :: dc_name(),
    node_name :: node()
}).

join() -> 

init() ->
    true = lists:all(fun(R) -> R end, [net_kernel:connect_node(SeedNode) || SeedNode <- earthwo_persist:seeds()]),
    StoreProps = [{ets, [{read_concurrency, true}]}],
    ok = mnesia:create_table(cluster_info, [
                {disc_copies, [node()]},
                {attributes, record_info(fields, cluster_info)},
                {storage_properties, StoreProps}]),
    ok = mnesia:create_table(topology, [
                {disc_copies, [node()]},
                {index, [#topology.dc_name, #topology.node_name]},
                {attributes, record_info(fields, topology)},
                {storage_properties, StoreProps}]).

local_topology() ->
    case ets:tab2list(cluster_info) of
        [] -> {undefined, []};
        [#cluster_info{node_id = NodeId}] ->
            {NodeId, ets:tab2list(topology)}
    end.

init_topology() ->
    case ets:tab2list(cluster_info) of
        [] -> new_topology();
        [#cluster_info{}] -> ok
    end.

new_topology() ->
    Seeds = seeds(),
    case Seeds -- [nodes()] of
        Seeds -> ok;
        OtherSeeds ->
            calc_topology(earthwo_conf:get(dc_name, default_dc),
                          earthwo_conf:get(shard, 64))
    end.

pick_seed(random) ->
    Seeds =  (),
    lists:nth(rand:uniform(length(Seeds)), Seeds).

seeds() ->
    {ok, Seeds} = earthwo_conf:get(seed_nodes),
    Seeds.

dcs(Topology) ->
    
pick_dc(Topology, random) ->
    DCs = dcs(Topology),
    lists:nth(rand:uniform(length(DCs)), DCs).

nodes(Topology, DC) ->
    [].

