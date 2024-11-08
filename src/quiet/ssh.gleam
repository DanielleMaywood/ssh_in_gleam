import gleam/erlang.{type EnsureAllStartedError}
import gleam/erlang/atom.{type Atom}
import gleam/erlang/charlist.{type Charlist}
import gleam/result

// /////////////// //
// SSH Application //
// /////////////// //

pub fn ensure_all_started() -> Result(List(Atom), EnsureAllStartedError) {
  erlang.ensure_all_started(atom.create_from_string("ssh"))
}

// ////////////// //
// SSH Connection //
// ////////////// //

pub opaque type Connection {
  Connection(ref: ConnectionRef)
}

pub type ConnectionError {
  ConnectionRefused
  Reason(String)
  Unknown
}

pub type ConnectOptions {
  ConnectOptions(username: String)
}

fn convert_connect_options(options: ConnectOptions) -> List(ConnectOption) {
  [User(name: charlist.from_string(options.username))]
}

type ConnectionRef

type ConnectOption {
  User(name: Charlist)
}

pub fn with_connection(
  host host: String,
  port port: Int,
  options options: ConnectOptions,
  timeout timeout: Int,
  perform perform: fn(Connection) -> a,
) -> Result(a, ConnectionError) {
  use connection <- result.try(connect(host, port, options, timeout))
  let out = perform(connection)
  use _ <- result.try(close(connection))
  Ok(out)
}

pub fn connect(
  host: String,
  port: Int,
  options: ConnectOptions,
  timeout: Int,
) -> Result(Connection, ConnectionError) {
  let options = convert_connect_options(options)
  use ref <- result.try(do_ssh_connect(host, port, options, timeout))

  Ok(Connection(ref:))
}

@external(erlang, "quiet_ffi", "ssh_connect")
fn do_ssh_connect(
  host: String,
  port: Int,
  options: List(ConnectOption),
  timeout: Int,
) -> Result(ConnectionRef, ConnectionError)

pub fn close(connection: Connection) -> Result(Nil, ConnectionError) {
  do_ssh_close(connection.ref)
}

@external(erlang, "quiet_ffi", "ssh_close")
fn do_ssh_close(conn: ConnectionRef) -> Result(Nil, ConnectionError)

// /////////////////// //
// SSH Session Channel //
// /////////////////// //

pub opaque type SessionChannel {
  SessionChannel(connection: Connection, id: ChannelId)
}

pub type SessionChannelError {
  Closed
  Timeout
}

type ChannelId

pub fn with_session(
  connection: Connection,
  timeout timeout: Int,
  perform perform: fn(SessionChannel) -> a,
) -> Result(a, SessionChannelError) {
  use channel <- result.try(session_open(connection, timeout))
  let out = perform(channel)
  session_close(channel)
  Ok(out)
}

pub fn session_open(
  connection: Connection,
  timeout timeout: Int,
) -> Result(SessionChannel, SessionChannelError) {
  use id <- result.try(do_session_channel(connection.ref, timeout))

  Ok(SessionChannel(connection:, id:))
}

@external(erlang, "quiet_ffi", "ssh_session_channel")
fn do_session_channel(
  conn: ConnectionRef,
  timeout: Int,
) -> Result(ChannelId, SessionChannelError)

pub fn session_close(channel: SessionChannel) -> Nil {
  do_close_session_channel(channel.connection.ref, channel.id)
}

@external(erlang, "quiet_ffi", "ssh_channel_close")
fn do_close_session_channel(conn: ConnectionRef, channel: ChannelId) -> Nil

pub type ExecStatus {
  Success
  Failure
}

pub fn exec(
  channel: SessionChannel,
  command: String,
  timeout: Int,
) -> Result(ExecStatus, SessionChannelError) {
  do_exec(channel.connection.ref, channel.id, command, timeout)
}

@external(erlang, "quiet_ffi", "ssh_channel_exec")
fn do_exec(
  conn: ConnectionRef,
  channel: ChannelId,
  cmd: String,
  timeout: Int,
) -> Result(ExecStatus, SessionChannelError)

pub fn recv(channel: SessionChannel, timeout: Int) -> #(String, String, Int) {
  do_recv(channel.id, timeout)
}

@external(erlang, "quiet_ffi", "ssh_channel_recv")
fn do_recv(channel: ChannelId, timeout: Int) -> #(String, String, Int)

// //////////////// //
// SSH SFTP Channel //
// //////////////// //

pub opaque type SftpChannel {
  SftpChannel(pid: SftpChannelPid)
}

pub type SftpChannelError

type SftpChannelPid

pub fn with_sftp(
  connection: Connection,
  perform: fn(SftpChannel) -> a,
) -> Result(a, SftpChannelError) {
  use channel <- result.try(sftp_open(connection))
  let out = perform(channel)
  sftp_close(channel)
  Ok(out)
}

pub fn sftp_open(
  connection: Connection,
) -> Result(SftpChannel, SftpChannelError) {
  use pid <- result.try(do_ssh_start_channel(connection.ref))

  Ok(SftpChannel(pid:))
}

@external(erlang, "quiet_ffi", "ssh_sftp_start_channel")
fn do_ssh_start_channel(
  connection: ConnectionRef,
) -> Result(SftpChannelPid, SftpChannelError)

pub fn sftp_close(channel: SftpChannel) -> Nil {
  do_ssh_stop_channel(channel.pid)
}

@external(erlang, "quiet_ffi", "ssh_sftp_stop_channel")
fn do_ssh_stop_channel(channel: SftpChannelPid) -> Nil

pub fn write_file(
  channel: SftpChannel,
  file: String,
  data: BitArray,
  timeout: Int,
) -> Result(Nil, SftpChannelError) {
  do_ssh_channel_write_file(channel.pid, file, data, timeout)
}

@external(erlang, "quiet_ffi", "ssh_sftp_write_file")
fn do_ssh_channel_write_file(
  channel: SftpChannelPid,
  file: String,
  data: BitArray,
  timeout: Int,
) -> Result(Nil, SftpChannelError)
