### Precondition

#### Fully Connected Cluster 

Because it depends on `global`'s name registration service. 
The name server must maintains a fully connected network. 

For example, if node N1 connects to node N2 (which is already connected to N3), 
the global name servers on the nodes N1 and N3 ensure that also N1 and N3 are connected. 
In other words, command-line flag `-connect_all false` can not be used.

#### Same Global Configuration 
All node's `global_quorum_size` and `global_jobs` must keep the same value. 
This ensures that the global task manager can transfer between nodes when the network splits.

### Configuration

#### global_jobs
the same format as `local_jobs`, default is `[]`. 
This means only run local jobs without running global task manager and monitor processes.

#### global_quorum_size
 
ecron application live on at least `global_quorum_size` nodes in the same cluster, can be regarded as a healthy cluster. 

Global task manager only running on a healthy cluster.

If you want to guarantee always no more than **one** global task manager even when the cluster has network split,
you should set it to **"half plus one"**.

For example:

### Run on majority
1. `ABC` 3 nodes in one cluster.
2. `global_quorum_size=2`.
3. (`ABC`) cluster split into 2 part(`AB`  =|=  `C`).
4. the global task manager would run on `AB` cluster(`AB` is the healthy cluster now).
5. `C` node only running local jobs without global jobs.

### Run on majority 
1. `ABC` 3 nodes in one cluster.
2. `global_quorum_size=2`.
3. (`ABC`) cluster split into 3 part(`A` =|= `B`  =|=  `C`).
4. every node only running local jobs without global jobs(all nodes is unhealthy).

### Run on every node if brain split.
1. `ABC` nodes in one cluster.    
1. `global_quorum_size=1`.
2. (`ABC`) cluster split into 3 part(`A` =|= `B`  =|=  `C`).
3. the global task manager would run on every nodes(we have three healthy cluster now).
4. But the global task manager only running one in the same cluster.

### Implementation
1. checking if `global_jobs` is empty when ecron application starts.
2. The process of global is finished when `global_jobs` is empty.
3. `ecron_sup` would start `ecron_monitor`(gen_server), when `global_jobs` is not empty.
4. `ecron_monitor` monitors node's status by [net_kernel:monitor_nodes(true)](http://erlang.org/doc/man/net_kernel.html#monitor_nodes-1), when it initializes.
5. Checking if there is enough `ecron` process in the cluster(`global_quorum_size`).
6. Trying to terminate global manager job process when `ecron` process's number less than `global_quorum_size`.
7. Otherwise, trying to start a global manager job gen_server, This process register by [global:register_name/2](http://erlang.org/doc/man/global.html#register_name-2).
8. All nodes competitively register this global jobs manager process, only one node will success, other node's `ecron_monitor` only link this process.
9. The `ecron_monitor` will receive notification, when node down/up or global jobs manager process die.
10. Enter step 5 again, when receiving notification.

```
     NodeA             NodeB              NodeC
      sup               sup                sup
       |                |  \                |
    monitor             | monitor         monitor
       |                |      |            |
       |                |     link          |
       |____link____GlobalJob__|____link____|
```
  
