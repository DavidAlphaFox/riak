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

%% @doc
%% Provide a safe method of saving data to disk along with a checksum
%% that is verified on read. Additionally, four replicas of the data
%% are stored across two files for greater redundancy/durability.
%% 安全存储
%% 通过增加校验和额外的文件冗余，提供数据安全
-module(riak_ensemble_save).
-export([write/2, read/1]).

%%===================================================================

-spec write(file:filename(), binary()) -> ok | {error, term()}.
write(File, Data) ->
	%% 先算出数据的的CRC
    CRC = erlang:crc32(Data),
    Size = byte_size(Data),
	%% 创建Meta
    Meta = <<CRC:32/integer, Size:32/integer>>,
	%% 写入双份数据，并进行了CRC双端校验
	%% 考虑的非常周全
	%% 如果第一份Meta和Data成功刷盘，即便后面写入失败，
	%% 也可以保证一定的数据完整性
    Out = [Meta, Data,  %% copy 1
           Data, Meta], %% copy 2
    ok = filelib:ensure_dir(File),
    try
        _ = Out,
		%% 刷盘一次主文件
		%% 刷盘一次备份文件
		%% 里面包含4份数据，分别分配在两个文件当中
        ok = riak_ensemble_util:replace_file(File, Out),
        ok = riak_ensemble_util:replace_file(File ++ ".backup", Out),
        ok
    catch
        _:Err ->
            {error, Err}
    end.

-spec read(file:filename()) -> {ok, binary()} | not_found.
read(File) ->
    case do_read(File) of
        not_found ->
            do_read(File ++ ".backup");
        Result ->
            Result
    end.

%%===================================================================

-spec do_read(file:filename()) -> {ok, binary()} | not_found.
do_read(File) ->
    case riak_ensemble_util:read_file(File) of
        {ok, Binary} ->
            safe_read(Binary);
        {error, _} ->
            not_found
    end.

-spec safe_read(binary()) -> {ok, binary()} | not_found.
safe_read(<<CRC:32/integer, Size:32/integer, Data:Size/binary, Rest/binary>>) ->
	%% 验证CRC
    case erlang:crc32(Data) of
        CRC ->
            {ok, Data};
        _ ->
			%% 尝试从第二份数据中读取
            safe_read_backup(Rest)
    end;
safe_read(Binary) ->
    safe_read_backup(Binary).

-spec safe_read_backup(binary()) -> {ok, binary()} | not_found.
safe_read_backup(Binary) when byte_size(Binary) =< 8 ->
    not_found;
safe_read_backup(Binary) ->
    BinSize = byte_size(Binary),
    Skip = BinSize - 8,
	%% 从尾部读出Meta信息
    <<_:Skip/binary, CRC:32/integer, Size:32/integer>> = Binary,
    Skip2 = Skip - Size,
    case Binary of
        <<_:Skip2/binary, Data:Size/binary, _:8/binary>> ->
            case erlang:crc32(Data) of
                CRC ->
                    {ok, Data};
                _ ->
                    not_found
            end;
        _ ->
            not_found
    end.
