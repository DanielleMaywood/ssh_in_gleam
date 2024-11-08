-module(quiet_ffi).
-export([
    ssh_connect/4,
    ssh_close/1,
    ssh_session_channel/2,
    ssh_channel_close/2,
    ssh_channel_exec/4,
    ssh_channel_recv/2,
    ssh_sftp_start_channel/1,
    ssh_sftp_stop_channel/1,
    ssh_sftp_write_file/4
]).

ssh_connect(Host, Port, Options, NegotiationTimeout) ->
    case ssh:connect(binary_to_list(Host), Port, Options, NegotiationTimeout) of
        {ok, Pid} -> {ok, Pid};
        {error, econnrefused} -> {error, connection_refused};
        {error, Reason} when is_list(Reason) -> {error, {reason, Reason}};
        {error, _} -> {error, unknown}
    end.

ssh_close(ConnectionRef) ->
    case ssh:close(ConnectionRef) of
        ok -> {ok, nil};
        {error, _} -> {error, unknown}
    end.

ssh_session_channel(ConnectionRef, Timeout) ->
    ssh_connection:session_channel(ConnectionRef, Timeout).

ssh_channel_close(ConnectionRef, ChannelId) ->
    case ssh_connection:close(ConnectionRef, ChannelId) of
        ok -> nil
    end.

ssh_channel_exec(ConnectionRef, ChannelId, Command, Timeout) ->
    case ssh_connection:exec(ConnectionRef, ChannelId, binary_to_list(Command), Timeout) of
        {error, Reason} -> {error, Reason};
        Reason -> {ok, Reason}
    end.

ssh_channel_recv(Channel, Timeout) ->
    ssh_channel_recv(Channel, [], [], Timeout).

ssh_channel_recv(Channel, Stdout, Stderr, Timeout) ->
    receive
        {ssh_cm, _ConnRef, {data, Channel, 0, Data}} ->
            ssh_channel_recv(Channel, [Stdout, Data], Stderr, Timeout);

        {ssh_cm, _ConnRef, {data, Channel, 1, Data}} ->
            ssh_channel_recv(Channel, Stdout, [Stderr, Data], Timeout);

        {ssh_cm, ConnRef, {eof, Channel}} ->
            ssh_connection:send_eof(ConnRef, Channel),
            ssh_channel_recv(Channel, Stdout, Stderr, Timeout);

        {ssh_cm, _ConnRef, {exit_status, Channel, Status}} ->
            {lists:flatten(Stdout), lists:flatten(Stderr), Status};

        {ssh_cm, _ConnRef, {closed, Channel}} ->
            {lists:flatten(Stdout), lists:flatten(Stderr), 0};
        _ ->
            ssh_channel_recv(Channel, Stdout, Stderr, Timeout)
    after Timeout ->
        {list_to_binary(lists:flatten(Stdout)), list_to_binary(lists:flatten(Stderr)), 0}
    end.

ssh_sftp_start_channel(Connection) ->
    case ssh_sftp:start_channel(Connection) of
        {ok, Pid} -> {ok, Pid};
        {ok, Pid, _} -> {ok, Pid};
        {error, _} -> {error, unknown}
    end.

ssh_sftp_stop_channel(Channel) ->
    ssh_sftp:stop_channel(Channel).

ssh_sftp_write_file(Channel, File, Data, Timeout) ->
    case ssh_sftp:write_file(Channel, binary_to_list(File), Data, Timeout) of
        ok -> {ok, nil};
        {error, _} -> {error, unknown}
    end.
