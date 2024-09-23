import gleam/erlang/charlist.{type Charlist}
import gleam/result

pub type SshError {
  ConnRefused
  Reason(String)
  Unknown
}

@external(erlang, "quiet_ffi", "ssh_start")
fn ssh_start() -> Result(Nil, SshError)

pub type ConnectionRef

type ConnectOption {
  User(name: Charlist)
}

@external(erlang, "quiet_ffi", "ssh_connect")
fn ssh_connect(
  host: String,
  port: Int,
  options: List(ConnectOption),
  timeout: Int,
) -> Result(ConnectionRef, SshError)

@external(erlang, "quiet_ffi", "ssh_close")
fn ssh_close(conn: ConnectionRef) -> Result(Nil, SshError)

pub type ChannelId

pub type SessionError {
  Closed
  Timeout
}

@external(erlang, "quiet_ffi", "ssh_session_channel")
fn ssh_session_channel(
  conn: ConnectionRef,
  timeout: Int,
) -> Result(ChannelId, SessionError)

@external(erlang, "quiet_ffi", "ssh_channel_close")
fn ssh_channel_close(conn: ConnectionRef, channel: ChannelId) -> Nil

pub type ExecStatus {
  Success
  Failure
}

@external(erlang, "quiet_ffi", "ssh_channel_exec")
fn ssh_channel_exec(
  conn: ConnectionRef,
  channel: ChannelId,
  cmd: String,
  timeout: Int,
) -> Result(ExecStatus, SessionError)

@external(erlang, "quiet_ffi", "ssh_channel_recv")
pub fn ssh_channel_recv(
  channel: ChannelId,
  timeout: Int,
) -> #(Charlist, Charlist, Int)

pub type ConnectOptions {
  ConnectOptions(username: String)
}

fn convert_connect_options(options: ConnectOptions) -> List(ConnectOption) {
  [User(name: charlist.from_string(options.username))]
}

pub opaque type Connection {
  Connection(ref: ConnectionRef)
}

pub fn open(
  host: String,
  port: Int,
  options: ConnectOptions,
  timeout: Int,
) -> Result(Connection, SshError) {
  use _ <- result.try(ssh_start())

  let options = convert_connect_options(options)

  use conn <- result.map(ssh_connect(host, port, options, timeout))

  Connection(ref: conn)
}

pub fn close(conn: Connection) -> Result(Nil, SshError) {
  ssh_close(conn.ref)
}

pub fn with_conn(
  host: String,
  port: Int,
  options: ConnectOptions,
  timeout: Int,
  next next: fn(Connection) -> a,
) -> Result(a, SshError) {
  use conn <- result.try(open(host, port, options, timeout))
  let out = next(conn)
  use _ <- result.try(close(conn))
  Ok(out)
}

pub opaque type Channel {
  Channel(conn: Connection, id: ChannelId)
}

pub fn open_channel(
  conn: Connection,
  timeout timeout: Int,
) -> Result(Channel, SessionError) {
  use id <- result.map(ssh_session_channel(conn.ref, timeout))

  Channel(conn:, id:)
}

pub fn close_channel(channel: Channel) {
  ssh_channel_close(channel.conn.ref, channel.id)
}

pub fn with_channel(
  conn: Connection,
  timeout timeout: Int,
  next next: fn(Channel) -> a,
) -> Result(a, SessionError) {
  use channel <- result.map(open_channel(conn, timeout))
  let out = next(channel)
  close_channel(channel)
  out
}

pub type Recv {
  Recv(stdout: String, stderr: String, status: Int)
}

pub fn exec(
  channel: Channel,
  cmd: String,
  timeout: Int,
) -> Result(ExecStatus, SessionError) {
  ssh_channel_exec(channel.conn.ref, channel.id, cmd, timeout)
}

pub fn recv(channel: Channel, timeout: Int) -> Result(Recv, Nil) {
  let #(stdout, stderr, status) = ssh_channel_recv(channel.id, timeout)

  Ok(Recv(
    stdout: charlist.to_string(stdout),
    stderr: charlist.to_string(stderr),
    status:,
  ))
}
