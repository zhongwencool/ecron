[
  extras: [
    {"README.md", title: "Home"},
    "./guide/crontab-spec.md",
    "./guide/implementation.md",
    "./guide/global.md",
    "./guide/changelog.md",
    "LICENSE"
  ],
  logo: "./guide/logo.svg",
  authors: [
    "Zhongwen Deng <zhongwencool@gmail.com>"
  ],
  main: "readme",
  source_url: "https://github.com/zhongwencool/ecron",
  homepage_url: "https://github.com/zhongwencool/ecron",
  with_mermaid: true,
  before_closing_body_tag: fn
    :html ->
      """
      <script src="https://cdn.jsdelivr.net/npm/mermaid/dist/mermaid.min.js"></script>
      <script>mermaid.initialize({startOnLoad: true})</script>
      """

    _ ->
      ""
  end,
  api_reference: false,
  warnings_as_errors: true,
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
