%% -------------------------------------------------------------------
%%
%% Copyright (c) 2013 Basho Technologies, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------
-module(riak_ensemble_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    riak_ensemble_sup:start_link().

stop(_State) ->
    ok.
%%% 
%%% riak_ensemble is a consensus library that supports creating multiple consensus groups (ensembles). 
%%% Each ensemble is a separate Multi-Paxos instance with its own leader, set of members, and state.
%%% 
%%% 集群一致性的library，提供了多个共存一致性的组
%%% 每个一致性组都是使用Paxos协议的节点，都会拥有自己的leader
%%% kafka也使用了类似的技术，不过选主的过程需要依赖Zookeeper
%%% 大家选择这么做，都是因为防止数据丢失
%%% 由于Paxos选主的影响，不可能无限的添加节点
%%% 所以将数据进行分片，每一个分片都有自己的Leader和Follower
%%% 这样就可以做到每个局部区域都是in-sync replicas
%%% 就是用分区来防止全局不可用
