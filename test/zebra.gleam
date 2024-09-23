import gleam/int
import gleam/io
import gleam/result
import quiet

pub fn main() {
  io.debug({
    let hostname = "winnie"
    let port = 22
    let options = quiet.ConnectOptions(username: "winnie")
    let timeout = 5000

    use conn <- quiet.with_conn(hostname, port, options, timeout)
    use channel <- quiet.with_channel(conn, timeout: timeout)

    use _ <- result.try({
      quiet.exec(channel, "ls -la ~/asdasda", timeout)
      |> result.nil_error
    })

    use out <- result.try({
      quiet.recv(channel, timeout)
      |> result.nil_error
    })

    io.println("STDOUT:")
    io.println(out.stdout)

    io.println("STDERR:")
    io.println(out.stderr)

    io.println("STATUS: " <> int.to_string(out.status))

    Ok(Nil)
  })
}
