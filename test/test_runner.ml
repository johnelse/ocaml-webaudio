open Webtest.Suite

let suite =
  "base_suite" >::: [
    Environment.suite;
    Buffer.suite;
    Oscillator.suite;
    Channels.suite;
    Nodes.suite;
    Offline.suite;
  ]

let () = Webtest_js.Runner.setup suite
