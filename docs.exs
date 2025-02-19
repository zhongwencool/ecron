[
  extras: [
    "README.md",
    "./guide/Telemetry.md",
    "./guide/Global.md",
    "./guide/Changelog.md",
    "LICENSE"
  ],
  logo: "./guide/logo.svg",
  main: "readme",
  source_url: "https://github.com/zhongwencool/ecron",
  homepage_url: "https://github.com/zhongwencool/ecron",
  with_mermaid: true,
  api_reference: false,
  groups_for_docs: [
    "Job Management":
      &(&1[:name] in [:start_link, :create, :delete, :deactivate, :activate, :reload]),
    "Timer Functions":
      &(&1[:name] in [:send_after] or (&1[:name] == :send_interval and &1[:arity] in [2, 3, 4])),
    "Debugging & Testing": &(&1[:name] in [:statistic, :parse_spec]),
    "Deprecated Functions":
      &(&1[:name] in [:add, :add_with_time, :add_with_count] or
          (&1[:name] == :send_interval and &1[:arity] in [5, 7, 8]))
  ]
  # groups_for_modules: []
  # assets: "test/output"
]
