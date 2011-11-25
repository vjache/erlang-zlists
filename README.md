Erlang Zlists: a lazy sequences library.
----------------------------------------------------

There are many possibilities to represent and handle lazy sequences.  
For disambiguation reasons the approach implemented here will be 
referred to as Z-Lists, or simply zlists.


## Design ##

The core module is 'zlists', similar to module 'lists' in Erlang's STDLIB. 
It  contains base operations supported over Z-Lists. The others, called 'zlists_X', 
are utility modules that bridge Z-Lists with standard Erlang modules 
which may be considered as data sources e.g. 'ets', 'dets', 'mnesia', 'disk_log', 'file' etc.  

## Usage ##

### Integrate to your project ###

This is a rebar'ized project, so, if you are already using rebar, just insert a reference 
to this git repo at your rebar.config.
Otherwise clone this repo, and run ``erl -make``.

### Examples ###

```erlang
        1> Naturals=zlists:recurrent(1, fun(N)-> N+1 end).
        [1|#Fun<zlists.2.24701927>]

        2> zlists:scroll(10,Naturals).
        {[1,2,3,4,5,6,7,8,9,10],[11|#Fun<zlists.2.24701927>]}

        3> zlists:scroll(10,zlists:drop(5,Naturals)).
        {[6,7,8,9,10,11,12,13,14,15],[16|#Fun<zlists.2.24701927>]}

        4> zlists:scroll(10,zlists:drop(500000,Naturals)).   
        {[500001,500002,500003,500004,500005,500006,500007,500008,500009,500010],
         [500011|#Fun<zlists.2.24701927>]}

        5> Fibonacci=zlists:recurrent(1, 0, fun(X0,S0) -> {X0+S0, X0} end).
        [1|#Fun<zlists.3.26975111>]

        6> zlists:expand(10, Fibonacci).
        [1,1,2,3,5,8,13,21,34,55|#Fun<zlists.3.26975111>]

        7> zlists:expand(10, zlists:drop(10,Fibonacci)).
        [89,144,233,377,610,987,1597,2584,4181,6765|
         #Fun<zlists.3.26975111>]

        8> zlists:scroll(10, zlists:drop(10,Fibonacci)).
        {[89,144,233,377,610,987,1597,2584,4181,6765],
         [10946|#Fun<zlists.3.26975111>]}
```    
    
### Create your own Z-List ###

Creating your own custom zlist is quite easy, you can directly construct 
a special improper list like this:
        
```erlang
[1|fun()-> L=deferred_work(),true=is_list(L),L end]
```

Note the | symbol between head element and tail function. You can use 
a proper list and tail function to construct a lazy list:

```erlang        
zlists:new([1,2,3], fun()-> [4,5] end)
```

If the lazy list you want to create has explicit recurrent nature, use ```zlists:recurrent/2/3``` 
to easily create a lazy list.