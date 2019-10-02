{ application
, erlcount
, [ { vsn, "1.0.0" }
  , {description, "Run regular expressions on Erlang source files"}
  , {applications, [stdlib, kernel, ppool]}
  , {modules, [erlcount, erlcount_sup, erlcount_lib, erlcount_dispatch, erlcount_counter]}
  , {applications, [ppool]}
  , {registered, [erlcount]}
  , {mod, {erlcount, []}}
  , {env, [{directory, "."}
    , {regex, ["if\\s.+->", "case\\s.+\\sof"]}
    , {max_files, 10}
    ]}
  ]
}.