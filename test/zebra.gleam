import gleam/int
import gleam/io
import quiet/ssh

pub fn main() {
  let assert Ok(_) = ssh.ensure_all_started()

  let host = "piglet"
  let port = 22
  let options = ssh.ConnectOptions(username: "danielle")
  let timeout = 5000

  use connection <- ssh.with_connection(host, port, options, timeout)

  let assert Ok(_) =
    ssh.with_sftp(connection, fn(channel) {
      let assert Ok(_) =
        ssh.write_file(
          channel,
          "/home/danielle/hello-world",
          <<"Hello World!">>,
          timeout,
        )
    })

  let assert Ok(_) =
    ssh.with_session(connection, timeout, fn(channel) {
      let assert Ok(_) = ssh.exec(channel, "ls -la ~/", timeout)

      let #(stdout, stderr, status) = ssh.recv(channel, timeout)

      io.println("STDOUT:")
      io.println(stdout)

      io.println("STDERR:")
      io.println(stderr)

      io.println("STATUS: " <> int.to_string(status))
    })
}
