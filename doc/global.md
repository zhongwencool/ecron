### Precondition
Because it depends on `global`'s name registration service. 
The name server also maintains a fully connected network. 
For example, if node N1 connects to node N2 (which is already connected to N3), 
the global name servers on the nodes N1 and N3 ensure that also N1 and N3 are connected. 
In other words, command-line flag -connect_all false can not be used 

### Configuration
`cluster_quorum_size` - A majority of the ecron must respond, default is 1.

If you want to make sure always one global task manager run in cluster even at brain split.

You should set it to “half plus one”.

for example:

### Run on majority
1. Set up 3 nodes in on cluster.  
2. `cluster_quorum_size=2`.
3. (`ABC`) nodes cluster split into 2 part(`AB`  =/=  `C`).
4. the global task manager will run on `AB` cluster.

### Don't run
1. Set up 3 nodes in on cluster.  
2. `cluster_quorum_size=2`.
3. (`ABC`) nodes cluster split into 3 part(`A` =/= `B`  =/=  `C`).
4. the global task manager doesn't run.

### Run on every node if brain split.
1. Set up 3 nodes in on cluster.  
1. `cluster_quorum_size=1`.
2. (`ABC`) nodes cluster split into 3 part(`A` =/= `B`  =/=  `C`).
3. the global task manager will run on every nodes.
